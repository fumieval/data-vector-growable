{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Vector.Growable
  ( Growable
  -- * Type synonyms
  , GrowableVector
  , GrowableUnboxedVector
  , GrowableStorableVector
  , GrowablePrimitiveVector
  , GrowableIOVector
  , GrowableUnboxedIOVector
  , GrowableStorableIOVector
  , GrowablePrimitiveIOVector
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
  -- * Atomic operation
  , CASVector(..)
  , atomicPush
  ) where

import Prelude hiding (read, length, replicate, null)
import Control.Exception
import Control.Monad.Catch
import Control.Monad.Primitive
import Data.Atomics
import Data.Bits
import Data.Primitive.MutVar
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MV
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Primitive.Mutable as P
import qualified Data.Vector.Storable.Mutable as S
import Unsafe.Coerce

data Pending a = Pending !Int (Ticket a) !a | Complete

data GVState v s a = GVState !Int !(v s a) (MutVar s (Pending a))

-- | 'Growable' is a dynamic vector based on mutable vector @v@.
newtype Growable v s a = Growable (MutVar s (GVState v s a))

type GrowableVector = Growable V.MVector
type GrowableUnboxedVector = Growable U.MVector
type GrowableStorableVector = Growable S.MVector
type GrowablePrimitiveVector = Growable S.MVector
type GrowableIOVector = Growable V.MVector RealWorld
type GrowableUnboxedIOVector = Growable U.MVector RealWorld
type GrowableStorableIOVector = Growable S.MVector RealWorld
type GrowablePrimitiveIOVector = Growable P.MVector RealWorld

-- | Create an empty vector with the given number of pre-allocated elements.
withCapacity :: (PrimMonad m, MVector v a) => Int -> m (Growable v (PrimState m) a) 
withCapacity cap = do
  vec <- MV.new cap
  ref <- newMutVar Complete
  Growable <$> newMutVar (GVState 0 vec ref)
{-# INlINE withCapacity #-}

-- | Create an empty vector
new :: (PrimMonad m, MVector v a) => m (Growable v (PrimState m) a) 
new = withCapacity 0
{-# INLINE new #-}

-- | Create a vector and fill with the initial value.
replicate :: (PrimMonad m, MVector v a) => Int -> a -> m (Growable v (PrimState m) a)
replicate len a = do
  vec <- MV.replicate len a
  ref <- newMutVar Complete
  Growable <$> newMutVar (GVState len vec ref)
{-# INLINE replicate #-}

-- | Like 'replicate', but initialises the elements by running the action repeatedly
replicateM :: (PrimMonad m, MVector v a) => Int -> m a -> m (Growable v (PrimState m) a)
replicateM len a = do
  vec <- MV.replicateM len a
  ref <- newMutVar Complete
  Growable <$> newMutVar (GVState len vec ref)
{-# INLINE replicateM #-}

-- | Append an element to the vector (not atomic).
push :: (PrimMonad m, MVector v a) => Growable v (PrimState m) a -> a -> m ()
push (Growable ref) val = do
  GVState len vec pending <- readMutVar ref
  vec' <- if MV.length vec == len
    then MV.unsafeGrow vec (max 1 len)
    else pure vec
  writeMutVar ref $ GVState (len + 1) vec' pending
  MV.write vec' len val
{-# INLINE push #-}

-- | Pop the last element. Returns 'Nothing' if the vector is empty.
pop :: (PrimMonad m, MVector v a) => Growable v (PrimState m) a -> m (Maybe a)
pop (Growable ref) = do
  GVState len vec pending <- readMutVar ref
  if len == 0
    then pure Nothing
    else do
      writeMutVar ref $ GVState (len - 1) vec pending
      Just <$> MV.unsafeRead vec (len - 1)
{-# INLINE pop #-}

-- | Get the length of the vector.
length :: (PrimMonad m) => Growable v (PrimState m) a -> m Int
length (Growable ref) = do
  GVState len _ _ <- readMutVar ref
  pure len
{-# INLINE length #-}

-- | Returns 'True' if the vector is empty
null :: (PrimMonad m) => Growable v (PrimState m) a -> m Bool
null (Growable ref) = do
  GVState len _ _ <- readMutVar ref
  pure $ len == 0
{-# INLINE null #-}

-- | May throw 'IndexOutOfBounds'
read :: (PrimMonad m, MVector v a, MonadThrow m) => Growable v (PrimState m) a -> Int -> m a
read (Growable ref) i = do
  GVState len vec _ <- readMutVar ref
  if i < len
    then MV.unsafeRead vec i
    else throwM $ IndexOutOfBounds $ show len <> " <= " <> show i
{-# INLINE read #-}

-- | Throws 'IndexOutOfBounds' if the index is larger than the size.
write :: (PrimMonad m, MVector v a, MonadThrow m) => Growable v (PrimState m) a -> Int -> a -> m ()
write (Growable ref) i val = do
  GVState len vec pending <- readMutVar ref
  case len `compare` i of
    LT -> throwM $ IndexOutOfBounds $ show len <> " < " <> show i
    EQ -> do
      let amount = 1 `shiftL` (finiteBitSize i - countLeadingZeros i) - MV.length vec
      vec' <- MV.unsafeGrow vec amount
      MV.unsafeWrite vec' i val
      writeMutVar ref $ GVState (i + 1) vec' pending
    GT -> MV.unsafeWrite vec i val
{-# INLINE write #-}

modify :: (PrimMonad m, MVector v a, MonadThrow m) => Growable v (PrimState m) a -> (a -> a) -> Int -> m ()
modify (Growable ref) f i = do
  GVState len vec _ <- readMutVar ref
  if len <= i
    then throwM $ IndexOutOfBounds $ show len <> " <= " <> show i
    else MV.unsafeModify vec f i
{-# INLINE modify #-}

-- | Thaw an immutable vector and create a 'Growable' one.
thaw :: (G.Vector v a, PrimMonad m) => v a -> m (Growable (G.Mutable v) (PrimState m) a)
thaw v = do
  vec <- G.thaw v
  pending <- newMutVar Complete
  Growable <$> newMutVar (GVState (G.length v) vec pending)
{-# INLINE thaw #-}

-- | Take a snapshot of a 'Growable' vector.
freeze :: (G.Vector v a, PrimMonad m) => Growable (G.Mutable v) (PrimState m) a -> m (v a)
freeze (Growable ref) = do
  GVState len vec _ <- readMutVar ref
  v <- G.freeze vec
  pure $! G.unsafeTake len v
{-# INLINE freeze #-}

-- | Take a snapshot of a 'Growable' vector. The original vector may not be used.
unsafeFreeze :: (G.Vector v a, PrimMonad m) => Growable (G.Mutable v) (PrimState m) a -> m (v a)
unsafeFreeze (Growable ref) = do
  GVState len vec _ <- readMutVar ref
  v <- G.unsafeFreeze vec
  pure $! G.unsafeTake len v
{-# INLINE unsafeFreeze #-}

-- | Turn 'Growable' vector into a regular mutable vector.
fromGrowable :: (PrimMonad m, MVector v a) => Growable v (PrimState m) a -> m (v (PrimState m) a)
fromGrowable (Growable ref) = do
  GVState len vec _ <- readMutVar ref
  pure $! MV.unsafeTake len vec
{-# INLINE fromGrowable #-}

-- | Create a 'Growable' vector from a mutable vector.
toGrowable ::  (PrimMonad m, MVector v a) => v (PrimState m) a -> m (Growable v (PrimState m) a)
toGrowable vec = do
    pending <- newMutVar Complete
    Growable <$> newMutVar (GVState (MV.length vec) vec pending)
{-# INLINE toGrowable #-}

class MVector v a => CASVector v a where
  readVectorElem :: v RealWorld a -> Int -> IO (Ticket a)
  casVectorElem :: v RealWorld a -> Int -> Ticket a -> a -> IO (Bool, Ticket a)

instance CASVector V.MVector a where
  readVectorElem (V.MVector _ _ arr) i = readArrayElem arr i
  casVectorElem (V.MVector _ _ arr) i x y = casArrayElem arr i x y

instance CASVector P.MVector Int where
  readVectorElem vec i = forgeIntTicket <$> P.unsafeRead vec i
  casVectorElem (P.MVector _ _ arr) i x y = do
    let old = peekTicket x
    old' <- casByteArrayInt arr i old y
    pure (old == old', forgeIntTicket y)

forgeIntTicket :: Int -> Ticket Int
forgeIntTicket = unsafeCoerce

-- | Atomically push a value to the end of the vector.
-- | Based on <https://www.stroustrup.com/lock-free-vector.pdf Damian Dechev, Peter Pirkelbauer, and Bjarne Stroustrup - Lock-free Dynamically Resizable Arrays>
atomicPush :: CASVector v a => Growable v RealWorld a -> a -> IO ()
atomicPush (Growable (MutVar mut)) val = go
  where
    complete vec v = readMutVar v >>= \case
      Complete -> pure ()
      Pending i oldVal newVal -> do
        (_done, _) <- casVectorElem vec i oldVal newVal
        writeMutVar v Complete
    go = do
      old <- readMutVarForCAS mut
      let GVState len vec pending = peekTicket old
      complete vec pending
      vec' <- if MV.length vec == len
        then MV.unsafeGrow vec (max 1 len)
        else pure vec
      oldVal <- readVectorElem vec' len
      pending' <- newMutVar $ Pending len oldVal val
      (success, _) <- casMutVar mut old $ GVState (len + 1) vec' pending'
      if success
        then complete vec' pending'
        else go
