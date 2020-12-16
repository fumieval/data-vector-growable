module Data.Vector.Growable
  ( Growable
  -- * Type synonyms
  , GrowableVector
  , GrowableUnboxedVector
  , GrowableStorableVector
  , GrowableIOVector
  , GrowableUnboxedIOVector
  , GrowableStorableIOVector
  -- * Operations
  , new
  , withCapacity
  , replicate
  , replicateM
  , push
  , pop
  , length
  , null
  , read
  , write
  , modify
  , thaw
  , freeze
  , unsafeFreeze
  , fromGrowable
  , toGrowable
  ) where

import Prelude hiding (read, length, replicate, null)
import Control.Exception
import Control.Monad.Catch
import Control.Monad.Primitive
import Data.Bits
import Data.Primitive.MutVar
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MV
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Storable.Mutable as S

data GVState v s a = GVState !Int !(v s a)

-- | 'Growable' is a dynamic vector based on mutable vector @v@.
newtype Growable v s a = Growable (MutVar s (GVState v s a))

type GrowableVector = Growable V.MVector
type GrowableUnboxedVector = Growable U.MVector
type GrowableStorableVector = Growable S.MVector
type GrowableIOVector = Growable V.MVector RealWorld
type GrowableUnboxedIOVector = Growable U.MVector RealWorld
type GrowableStorableIOVector = Growable S.MVector RealWorld

-- | Create an empty vector with the given number of pre-allocated elements.
withCapacity :: (PrimMonad m, MVector v a) => Int -> m (Growable v (PrimState m) a) 
withCapacity cap = do
  vec <- MV.new cap
  Growable <$> newMutVar (GVState 0 vec)
{-# INlINE withCapacity #-}

-- | Create an empty vector
new :: (PrimMonad m, MVector v a) => m (Growable v (PrimState m) a) 
new = withCapacity 0
{-# INLINE new #-}

-- | Create a vector and fill with the initial value.
replicate :: (PrimMonad m, MVector v a) => Int -> a -> m (Growable v (PrimState m) a)
replicate len a = do
  vec <- MV.replicate len a
  Growable <$> newMutVar (GVState len vec)
{-# INLINE replicate #-}

-- | Like 'replicate', but initialises the elements by running the action repeatedly
replicateM :: (PrimMonad m, MVector v a) => Int -> m a -> m (Growable v (PrimState m) a)
replicateM len a = do
  vec <- MV.replicateM len a
  Growable <$> newMutVar (GVState len vec)
{-# INLINE replicateM #-}

-- | Append an element to the vector (not atomic).
push :: (PrimMonad m, MVector v a) => Growable v (PrimState m) a -> a -> m ()
push (Growable ref) val = do
  GVState len vec <- readMutVar ref
  vec' <- if MV.length vec == len
    then MV.unsafeGrow vec (max 1 len)
    else pure vec
  writeMutVar ref $ GVState (len + 1) vec'
  MV.write vec' len val
{-# INLINE push #-}

-- | Pop the last element. Returns 'Nothing' if the vector is empty.
pop :: (PrimMonad m, MVector v a) => Growable v (PrimState m) a -> m (Maybe a)
pop (Growable ref) = do
  GVState len vec <- readMutVar ref
  if len == 0
    then pure Nothing
    else do
      writeMutVar ref $ GVState (len - 1) vec
      Just <$> MV.unsafeRead vec (len - 1)
{-# INLINE pop #-}

-- | Get the length of the vector.
length :: (PrimMonad m) => Growable v (PrimState m) a -> m Int
length (Growable ref) = do
  GVState len _ <- readMutVar ref
  pure len
{-# INLINE length #-}

-- | Returns 'True' if the vector is empty
null :: (PrimMonad m) => Growable v (PrimState m) a -> m Bool
null (Growable ref) = do
  GVState len _ <- readMutVar ref
  pure $ len == 0
{-# INLINE null #-}

-- | May throw 'IndexOutOfBounds'
read :: (PrimMonad m, MVector v a, MonadThrow m) => Growable v (PrimState m) a -> Int -> m a
read (Growable ref) i = do
  GVState len vec <- readMutVar ref
  if i < len
    then MV.unsafeRead vec i
    else throwM $ IndexOutOfBounds $ show len <> " <= " <> show i
{-# INLINE read #-}

-- | Throws 'IndexOutOfBounds' if the index is larger than the size.
write :: (PrimMonad m, MVector v a, MonadThrow m) => Growable v (PrimState m) a -> Int -> a -> m ()
write (Growable ref) i val = do
  GVState len vec <- readMutVar ref
  case len `compare` i of
    LT -> throwM $ IndexOutOfBounds $ show len <> " < " <> show i
    EQ -> do
      let amount = 1 `shiftL` (finiteBitSize i - countLeadingZeros i) - MV.length vec
      vec' <- MV.unsafeGrow vec amount
      MV.unsafeWrite vec' i val
      writeMutVar ref $ GVState (i + 1) vec'
    GT -> MV.unsafeWrite vec i val
{-# INLINE write #-}

modify :: (PrimMonad m, MVector v a, MonadThrow m) => Growable v (PrimState m) a -> (a -> a) -> Int -> m ()
modify (Growable ref) f i = do
  GVState len vec <- readMutVar ref
  if len <= i
    then throwM $ IndexOutOfBounds $ show len <> " <= " <> show i
    else MV.unsafeModify vec f i
{-# INLINE modify #-}

-- | Thaw an immutable vector and create a 'Growable' one.
thaw :: (G.Vector v a, PrimMonad m) => v a -> m (Growable (G.Mutable v) (PrimState m) a)
thaw v = do
  vec <- G.thaw v
  Growable <$> newMutVar (GVState (G.length v) vec)
{-# INLINE thaw #-}

-- | Take a snapshot of a 'Growable' vector.
freeze :: (G.Vector v a, PrimMonad m) => Growable (G.Mutable v) (PrimState m) a -> m (v a)
freeze (Growable ref) = do
  GVState len vec <- readMutVar ref
  v <- G.freeze vec
  pure $! G.unsafeTake len v
{-# INLINE freeze #-}

-- | Take a snapshot of a 'Growable' vector. The original vector may not be used.
unsafeFreeze :: (G.Vector v a, PrimMonad m) => Growable (G.Mutable v) (PrimState m) a -> m (v a)
unsafeFreeze (Growable ref) = do
  GVState len vec <- readMutVar ref
  v <- G.unsafeFreeze vec
  pure $! G.unsafeTake len v
{-# INLINE unsafeFreeze #-}

-- | Turn 'Growable' vector into a regular mutable vector.
fromGrowable :: (PrimMonad m, MVector v a) => Growable v (PrimState m) a -> m (v (PrimState m) a)
fromGrowable (Growable ref) = do
  GVState len vec <- readMutVar ref
  pure $! MV.unsafeTake len vec
{-# INLINE fromGrowable #-}

-- | Create a 'Growable' vector from a mutable vector.
toGrowable ::  (PrimMonad m, MVector v a) => v (PrimState m) a -> m (Growable v (PrimState m) a)
toGrowable vec = Growable <$> newMutVar (GVState (MV.length vec) vec)
{-# INLINE toGrowable #-}
