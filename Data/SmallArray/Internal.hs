{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

module Data.SmallArray.Internal

import Prelude hiding (length)
import qualified Prelude

import Control.Monad.ST

import qualified Data.ByteArray as B
import Data.ByteArray (ByteArray, MutableByteArray)

import Data.Int
import Data.Word

data Array a
    = A {-# UNPACK #-}!Int
                      !ByteArray
data MArray s a
    = M {-# UNPACK #-}!Int
                      !(MutableByteArray s)

instance (Show e, Elt e) => Show (Array e) where
    show = show . toList

class IArray a where
    length :: a -> Int

instance IArray (Array a) where
    length (A n _) = n

instance IArray (MArray s a) where
    length (M n _) = n

unsafeNew :: Elt e => Int -> ST s (MArray s e)
unsafeNew n = new undefined
   where new :: Elt e => e -> ST s (MArray s e)
         new e = M n `fmap` B.new (bytesInArray n e)

new :: Elt e => Int -> e -> ST s (MArray s e)
new n e = do
  arr <- unsafeNew n
  mapM_ (flip (unsafeWrite arr) e) [0 .. (n-1)]
  return arr

unsafeFreeze :: MArray s e -> ST s (Array e)
unsafeFreeze (M n marr) = A n `fmap` B.unsafeFreeze marr 

run :: (forall s . ST s (MArray s e)) -> Array e
run act = runST $ act >>= unsafeFreeze

empty :: Elt e => Array e
empty = run $ unsafeNew 0

toList :: Elt e => Array e -> [e]
toList arr = f 0
 where f n | n < len = arr `unsafeIndex` n : f (n+1)
           | otherwise = []
       len = length arr

fromList :: Elt e => [e] -> Array e
fromList xs
    = run $ do
        arr <- unsafeNew len
        mapM_ (uncurry $ unsafeWrite arr) $ zip [0..(len-1)] xs
        return arr
 where len = Prelude.length xs

class Elt e where
    index :: Array e -> Int -> e
    index = unsafeIndex

    read :: MArray s e -> Int -> ST s e
    read = unsafeRead

    write :: MArray s e -> Int -> e -> ST s ()
    write = unsafeWrite

    bytesInArray :: Int -> e -> Int
    unsafeIndex :: Array e -> Int -> e
    unsafeRead :: MArray s e -> Int -> ST s e
    unsafeWrite :: MArray s e -> Int -> e -> ST s ()


#define deriveElt(Typ) \
instance Elt Typ where { \
   bytesInArray sz el = B.elemSize el * sz      \
;  unsafeIndex (A _ arr) n   = B.index arr n    \
;  unsafeRead  (M _ arr) n   = B.read  arr n    \
;  unsafeWrite (M _ arr) n x = B.write arr n x  \
} \

deriveElt(Char)
deriveElt(Double)
deriveElt(Float)
deriveElt(Int)
deriveElt(Int8)
deriveElt(Int16)
deriveElt(Int32)
deriveElt(Int64)
deriveElt(Word)
deriveElt(Word8)
deriveElt(Word16)
deriveElt(Word32)
deriveElt(Word64)
