module Data.Vector.Growable
  ( Growable(..)
  , GVState(..)
  , new
  , replicate
  , replicateM
  , pushBack
  , popBack
  , length
  , read
  , write
  , modify
  , thaw
  , freeze
  , unsafeFreeze
  ) where

import Prelude hiding (read, length, replicate)
import Control.Monad.Primitive
import Data.Bits
import Data.Primitive.MutVar
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MV
import Data.Vector.Generic.Mutable (MVector)

data GVState v s a = GVState !Int !(v s a)

newtype Growable v s a = Growable { unGrowable :: MutVar s (GVState v s a) }

new :: (PrimMonad m, MVector v a) => Int -> m (Growable v (PrimState m) a) 
new len = do
  vec <- MV.new len
  Growable <$> newMutVar (GVState len vec)
{-# INLINE new #-}

replicate :: (PrimMonad m, MVector v a) => Int -> a -> m (Growable v (PrimState m) a)
replicate len a = do
  vec <- MV.replicate len a
  Growable <$> newMutVar (GVState len vec)
{-# INLINE replicate #-}

replicateM :: (PrimMonad m, MVector v a) => Int -> m a -> m (Growable v (PrimState m) a)
replicateM len a = do
  vec <- MV.replicateM len a
  Growable <$> newMutVar (GVState len vec)
{-# INLINE replicateM #-}

-- | Append an element to the vector (not atomic).
pushBack :: (PrimMonad m, MVector v a) => Growable v (PrimState m) a -> a -> m ()
pushBack (Growable ref) val = do
  GVState len vec <- readMutVar ref
  vec' <- if MV.length vec == len
    then MV.unsafeGrow vec (max 1 len)
    else pure vec
  writeMutVar ref $ GVState (len + 1) vec'
  MV.write vec' len val
{-# INLINE pushBack #-}

popBack :: (PrimMonad m, MVector v a) => Growable v (PrimState m) a -> m (Maybe a)
popBack (Growable ref) = do
  GVState len vec <- readMutVar ref
  if len == 0
    then pure Nothing
    else do
      writeMutVar ref $ GVState (len - 1) vec
      Just <$> MV.unsafeRead vec (len - 1)

length :: (PrimMonad m) => Growable v (PrimState m) a -> m Int
length (Growable ref) = do
  GVState len _ <- readMutVar ref
  pure len
{-# INLINE length #-}

read :: (PrimMonad m, MVector v a) => Growable v (PrimState m) a -> Int -> m a
read (Growable ref) i = do
  GVState _ vec <- readMutVar ref
  MV.unsafeRead vec i
{-# INLINE read #-}

write :: (PrimMonad m, MVector v a) => Growable v (PrimState m) a -> Int -> a -> m ()
write (Growable ref) i val = do
  GVState _len vec <- readMutVar ref
  if MV.length vec <= i
    then do
      let amount = 1 `shiftL` (finiteBitSize i - countLeadingZeros i) - MV.length vec
      vec' <- MV.unsafeGrow vec amount
      MV.unsafeWrite vec' i val
      writeMutVar ref $ GVState (i + 1) vec'
    else MV.unsafeWrite vec i val
{-# INLINE write #-}

modify :: (PrimMonad m, MVector v a) => Growable v (PrimState m) a -> (a -> a) -> Int -> m ()
modify (Growable ref) f i = do
  GVState len vec <- readMutVar ref
  if len <= i
    then error $ "index out of bounds: " <> show len <> " <= " <> show i
    else MV.unsafeModify vec f i
{-# INLINE modify #-}

thaw :: (G.Vector v a, PrimMonad m) => v a -> m (Growable (G.Mutable v) (PrimState m) a)
thaw v = do
  vec <- G.thaw v
  Growable <$> newMutVar (GVState (G.length v) vec)
{-# INLINE thaw #-}

freeze :: (G.Vector v a, PrimMonad m) => Growable (G.Mutable v) (PrimState m) a -> m (v a)
freeze (Growable ref) = do
  GVState len vec <- readMutVar ref
  v <- G.freeze vec
  pure $! G.unsafeTake len v
{-# INLINE freeze #-}

unsafeFreeze :: (G.Vector v a, PrimMonad m) => Growable (G.Mutable v) (PrimState m) a -> m (v a)
unsafeFreeze (Growable ref) = do
  GVState len vec <- readMutVar ref
  v <- G.unsafeFreeze vec
  pure $! G.unsafeTake len v
{-# INLINE unsafeFreeze #-}