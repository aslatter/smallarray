
import Control.DeepSeq
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS

import qualified Data.Array.Unboxed as UA

import qualified Data.StorableVector as SV

import qualified Data.SmallArray as SA

import Criterion.Main

main = defaultMain
       [ bgroup "Word8" $
          let bs = BS.pack word8s
              uarr = mkUArray8s word8sAssocs
              sv = SV.pack word8s
              sa = SA.fromList word8s
          in 
           [ bgroup "pack"
               [ bench "bytestring"   $ nf BS.pack word8s
               , bench "uarray"       $ nf mkUArray8s word8sAssocs
               , bench "storable-vec" $ nf SV.pack word8s
               , bench "small-array"  $ nf SA.fromList word8s
               ]
           , bgroup "unpack"
               [ bench "bytestring"   $ nf BS.unpack bs
               , bench "uarray"       $ nf UA.elems uarr
               , bench "storable-vec" $ nf SV.unpack sv
               , bench "small-array"  $ nf SA.toList sa
               ]
           , bgroup "eq"
               [ bench "bytestring"   $ nf (bs ==) bs
               , bench "uarray"       $ nf (uarr ==) uarr
               , bench "storable-vec" $ nf (sv ==) sv
               , bench "small-array"  $ nf (sa ==) sa
               ]
           ]
       , bgroup "Word16"
           [ bgroup "pack"
               [ bench "uarray"      $ nf mkUArray16s word16sAssocs
               , bench "storable-vec" $ nf SV.pack word16s
               , bench "small-array" $ nf SA.fromList word16s
               ]
           , bgroup "unpack"
               [ bench "uarray"      $ nf UA.elems (mkUArray16s word16sAssocs)
               , bench "storable-vec" $ nf SV.unpack (SV.pack word16s)
               , bench "small-array" $ nf SA.toList (SA.fromList word16s)
               ]
           ]
       ]

word8s :: [Word8]
word8s = [1..10] ++ [30..20] ++ [100..50]

mkUArray8s :: [(Int, Word8)] -> UA.UArray Int Word8
mkUArray8s = UA.array word8sBounds

word8sBounds :: (Int, Int)
word8sBounds = (1, length word8s)

word8sAssocs :: [(Int, Word8)]
word8sAssocs = zip [1..length word8s] word8s



word16s :: [Word16]
word16s = [1..10] ++ [30..20] ++ [100..50]

mkUArray16s :: [(Int, Word16)] -> UA.UArray Int Word16
mkUArray16s = UA.array word16sBounds

word16sBounds :: (Int, Int)
word16sBounds = (1, length word16s)

word16sAssocs :: [(Int, Word16)]
word16sAssocs = zip [1..length word16s] word16s



instance NFData BS.ByteString where
instance NFData (UA.UArray i e)
instance NFData (SV.Vector e)