{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Primitive.PrimArray
  ( -- * Types
    PrimArray(..)
  , MutablePrimArray(..)
    -- * Allocation
  , newPrimArray
  , emptyPrimArray
  , singletonPrimArray
    -- * Element Access
  , readPrimArray
  , writePrimArray
  , indexPrimArray
    -- * Freezing and Thawing
  , unsafeFreezePrimArray
  , unsafeThawPrimArray
    -- * Block Operations
  , copyPrimArray
  , copyMutablePrimArray
  , copyPrimArrayToPtr
  , copyPtrToMutablePrimArray
  , copyPtrToPrimArray
  , setPrimArray
    -- * Information
  , sameMutablePrimArray
  , getSizeofMutablePrimArray
  , sizeofPrimArray
  ) where

import GHC.Prim
import GHC.Exts (isTrue#)
import GHC.Int
import GHC.Ptr
import Data.Primitive
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Primitive.Types as PT

-- | Primitive arrays
data PrimArray a = PrimArray ByteArray#

-- | Mutable primitive arrays associated with a primitive state token
data MutablePrimArray s a = MutablePrimArray (MutableByteArray# s)

instance (Eq a, Prim a) => Eq (PrimArray a) where
  a1 == a2 = sizeofPrimArray a1 == sizeofPrimArray a2 && loop (sizeofPrimArray a1 - 1)
   where 
   loop !i | i < 0 = True
           | otherwise = indexPrimArray a1 i == indexPrimArray a2 i && loop (i-1)

-- It is actually possible to write this without the Prim constraint
-- on the type variable a. You just copy the byte arrays using
-- more primitive operations.
instance Prim a => Monoid (PrimArray a) where
  mempty = emptyPrimArray
  mappend a b = runST $ do
    let szA = sizeofPrimArray a
    let szB = sizeofPrimArray b
    c <- newPrimArray (szA + szB)
    copyPrimArray c 0 a 0 szA
    copyPrimArray c szA b 0 szB
    unsafeFreezePrimArray c

emptyPrimArray :: PrimArray a
{-# NOINLINE emptyPrimArray #-}
emptyPrimArray = runST $ primitive $ \s0# -> case newByteArray# 0# s0# of
  (# s1#, arr# #) -> case unsafeFreezeByteArray# arr# s1# of
    (# s2#, arr'# #) -> (# s2#, PrimArray arr'# #)

singletonPrimArray :: Prim a => a -> PrimArray a
singletonPrimArray a = runST $ do
  arr <- newPrimArray 1
  writePrimArray arr 0 a
  unsafeFreezePrimArray arr


newPrimArray :: forall m a. (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newPrimArray #-}
newPrimArray (I# n#)
  = primitive (\s# -> 
      case newByteArray# (n# *# sizeOf# (undefined :: a)) s# of
        (# s'#, arr# #) -> (# s'#, MutablePrimArray arr# #)
    )

readPrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> Int -> m a
{-# INLINE readPrimArray #-}
readPrimArray (MutablePrimArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)

writePrimArray ::
     (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int
  -> a
  -> m ()
{-# INLINE writePrimArray #-}
writePrimArray (MutablePrimArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

-- | Copy part of a mutable array into another mutable array.
--   In the case that the destination and
--   source arrays are the same, the regions may overlap.
copyMutablePrimArray :: forall m a.
     (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> MutablePrimArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
{-# INLINE copyMutablePrimArray #-}
copyMutablePrimArray (MutablePrimArray dst#) (I# doff#) (MutablePrimArray src#) (I# soff#) (I# n#)
  = primitive_ (copyMutableByteArray#
      src# 
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

-- | Copy part of an array into another mutable array.
copyPrimArray :: forall m a.
     (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> PrimArray a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
{-# INLINE copyPrimArray #-}
copyPrimArray (MutablePrimArray dst#) (I# doff#) (PrimArray src#) (I# soff#) (I# n#)
  = primitive_ (copyByteArray#
      src# 
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

-- | Copy a slice of an immutable primitive array to an address.
-- The offset and length are given in elements of type @a@.
copyPrimArrayToPtr :: forall m a. (PrimMonad m, Prim a)
  => Ptr a                            -- ^ destination pointer
  -> PrimArray a                      -- ^ source array
  -> Int                              -- ^ offset into source array
  -> Int                              -- ^ number of prims to copy
  -> m ()
{-# INLINE copyPrimArrayToPtr #-}
copyPrimArrayToPtr (Ptr addr#) (PrimArray ba#) (I# soff#) (I# n#) =
    primitive (\ s# ->
        let s'# = copyByteArrayToAddr# ba# (soff# *# siz#) addr# (n# *# siz#) s#
        in (# s'#, () #))
  where siz# = sizeOf# (undefined :: a)

copyPtrToMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int
  -> Ptr a
  -> Int
  -> m ()
{-# INLINE copyPtrToMutablePrimArray #-}
copyPtrToMutablePrimArray (MutablePrimArray ba#) (I# doff#) (Ptr addr#) (I# n#) = 
  primitive (\ s# ->
      let s'# = copyAddrToByteArray# addr# ba# (doff# *# siz#) (n# *# siz#) s#
      in (# s'#, () #))
  where siz# = sizeOf# (undefined :: a)

copyPtrToPrimArray :: forall a. Prim a
  => Ptr a
  -> Int
  -> PrimArray a
{-# INLINE copyPtrToPrimArray #-}
copyPtrToPrimArray ptr len = runST $ do
  arr <- newPrimArray len
  copyPtrToMutablePrimArray arr 0 ptr len
  unsafeFreezePrimArray arr

-- | Fill a slice of a mutable byte array with a value.
setPrimArray
  :: (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a -- ^ array to fill
  -> Int -- ^ offset into array
  -> Int -- ^ number of values to fill
  -> a -- ^ value to fill with
  -> m ()
{-# INLINE setPrimArray #-}
setPrimArray (MutablePrimArray dst#) (I# doff#) (I# sz#) x
  = primitive_ (PT.setByteArray# dst# doff# sz# x)

-- | Get the size of the mutable array.
getSizeofMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ array
  -> m Int
{-# INLINE getSizeofMutablePrimArray #-}
getSizeofMutablePrimArray (MutablePrimArray arr#)
  = primitive (\s# -> 
      case getSizeofMutableByteArray# arr# s# of
        (# s'#, sz# #) -> (# s'#, I# (quotInt# sz# (sizeOf# (undefined :: a))) #)
    )

-- | Check if the two arrays refer to the same memory block.
sameMutablePrimArray :: MutablePrimArray s a -> MutablePrimArray s a -> Bool
{-# INLINE sameMutablePrimArray #-}
sameMutablePrimArray (MutablePrimArray arr#) (MutablePrimArray brr#)
  = isTrue# (sameMutableByteArray# arr# brr#)

-- | Convert a mutable byte array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezePrimArray
  :: PrimMonad m => MutablePrimArray (PrimState m) a -> m (PrimArray a)
{-# INLINE unsafeFreezePrimArray #-}
unsafeFreezePrimArray (MutablePrimArray arr#)
  = primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, PrimArray arr'# #))

-- | Convert an immutable array to a mutable one without copying. The
-- original array should not be used after the conversion.
unsafeThawPrimArray
  :: PrimMonad m => PrimArray a -> m (MutablePrimArray (PrimState m) a)
{-# INLINE unsafeThawPrimArray #-}
unsafeThawPrimArray (PrimArray arr#)
  = primitive (\s# -> (# s#, MutablePrimArray (unsafeCoerce# arr#) #))

-- | Read a primitive value from the array.
indexPrimArray :: forall a. Prim a => PrimArray a -> Int -> a
{-# INLINE indexPrimArray #-}
indexPrimArray (PrimArray arr#) (I# i#) = indexByteArray# arr# i#

sizeofPrimArray :: forall a. Prim a => PrimArray a -> Int
{-# INLINE sizeofPrimArray #-}
sizeofPrimArray (PrimArray arr#) = I# (quotInt# (sizeofByteArray# arr#) (sizeOf# (undefined :: a)))

