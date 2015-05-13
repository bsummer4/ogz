{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE TemplateHaskell, TypeOperators, DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable, DeriveFoldable, LambdaCase #-}

module Gen where

import OGZ as OGZ

import ClassyPrelude hiding (mapM_,sum,concat,toList,foldr,forM_,forM)

import           Data.Array.Repa            ((:.) (..), Array, D (..), DIM2,
                                             DIM3, U (..), Z (..))

import           Codec.Compression.GZip     (compress, decompress)

import qualified Data.Array.Repa            as A
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.IEEE754
import           Data.Binary.Put
import           Data.Bits
import           Data.Bits
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Char
import           Data.DeriveTH
import           Data.Foldable
import qualified Data.List                  as L
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Traversable
import           Data.Vector                ((!), (!?))
import qualified Data.Vector                as V
import           Data.Word
import           Numeric
import           Numeric.Noise
import           Numeric.Noise.Perlin
import           Prelude.Unicode
import           System.Random
import           Test.QuickCheck            hiding ((.&.))
import           Text.Printf


-- Types -----------------------------------------------------------------------

type BitField2d r = Array r DIM2 Bool
type BitField3d r = Array r DIM3 Bool

data Tree a = Branch (Eight (Tree a))
            | Leaf a
  deriving (Show,Eq,Ord,Functor)


-- Map Generation --------------------------------------------------------------

solid = NSolid (Textures (Six 2 3 4 5 6 7)) noProps
empty = NEmpty (Textures (Six 0 0 0 0 0 0)) noProps

octree a b c d e f g h = Octree $ Eight a b c d e f g h
tree a b c d e f g h = Branch $ Eight a b c d e f g h

bottomOnly fill a b c d = tree a b c d (Leaf fill) (Leaf fill) (Leaf fill) (Leaf fill)

bottomOnlyXFour fill a = tree a a a a (Leaf fill) (Leaf fill) (Leaf fill) (Leaf fill)

pillars ∷ HeightMap2D → BitField3d D
pillars m = A.fromFunction (nByNByN size) translate
  where (Z:.w:.h, f) = A.toFunction m
        size = if w==h then w else error "Non-square heightmap"
        translate (Z:.x:.y:.z) = z > f(Z:.x:.y)

powerOfTwo ∷ Int → Bool
powerOfTwo = (1≡) . popCount

-- This is equivalent to ln_2(n).
depthNeeded ∷ Int → Maybe Int
depthNeeded n = if not(powerOfTwo n) then Nothing else Just(loop 0)
  where loop i = if testBit n i then i else loop (i+1)

idxTup ∷ DIM3 → (Int,Int,Int)
idxTup (Z:.x:.y:.z) = (x,y,z)

-- TODO This is just a guess! Figure out how the indexing system actually works.
indexTree ∷ Int → Tree DIM3
indexTree depth = recurse depth (Z:.0:.0:.0)
  where recurse 0 i = Leaf i
        recurse d (Z:.px:.py:.pz) =
          let (x,y,z) = (px*2,py*2,pz*2) in
          Branch $ Eight
            (recurse (d-1) $ Z :. x   :. y   :. z+1)
            (recurse (d-1) $ Z :. x+1 :. y   :. z+1)
            (recurse (d-1) $ Z :. x   :. y+1 :. z+1)
            (recurse (d-1) $ Z :. x+1 :. y+1 :. z+1)
            (recurse (d-1) $ Z :. x   :. y   :. z  )
            (recurse (d-1) $ Z :. x+1 :. y   :. z  )
            (recurse (d-1) $ Z :. x   :. y+1 :. z  )
            (recurse (d-1) $ Z :. x+1 :. y+1 :. z  )

leaves ∷ Tree a → [a]
leaves = f
  where f (Leaf x) = [x]
        f (Branch xs) = concat $ toList $ f <$> xs

maze3d ∷ BitField3d D → Tree Bool
maze3d bf = f <$> indexTree depth
  where (Z:.w:.h:.d, f) = A.toFunction bf
        depth = case (depthNeeded w, w≡h && h≡d) of
                  (Just d, True) → d
                  _              → error $ printf
                    "3d bit fields must be cubes with sizes that are powers of two. Not %s" (show(w,h,d))

maze2d ∷ BitField2d D → Tree Bool
maze2d bf = f3 <$> indexTree depth
  where (Z:.w:.h, f2) = A.toFunction bf
        f3 (Z:.x:.y:.z) = if z≠(depth-1) then False else f2(Z:.x:.y)
        depth = case (depthNeeded w, w≡h) of
                  (Just d, True) → d
                  _              → error "2d bit fields must be squares with sizes that are powers of two."

squareRoot :: Int -> Int
squareRoot = trace "squareRoot" . traceShowId . f . traceShowId
  where f n = case n of
          0 → 0
          1 → 1
          n → let (^!) :: Num a => a -> Int -> a
                  (^!) x n = x^n
                  twopows = L.iterate (^!2) 2
                  (lowerRoot, lowerN) =
                     L.last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
                  newtonStep x = div (x + div n x) 2
                  iters = L.iterate newtonStep (f (div n lowerN) * lowerRoot)
                  isRoot r  =  r^!2 <= n && n < (r+1)^!2
              in  L.head $ L.dropWhile (not . isRoot) iters

maze2d' ∷ [Int] → OctreeNode
maze2d' ls = treeOctN $ maze2d $ A.delay $ A.fromListUnboxed shape $ (≠0) <$> ls
  where shape = Z :. size :. size
        size = squareRoot $ traceShowId $ length ls

stripes ∷ [Int]
stripes = x4 $ x4 $ x4 [0,0,1,1]
  where x4 a = mconcat[a,a,a,a]

treeOctN ∷ Tree Bool → OctreeNode
treeOctN (Leaf False) = empty
treeOctN (Leaf True)  = solid
treeOctN (Branch b)   = NBroken $ Octree $ treeOctN <$> b

roomOGZ ∷ OctreeNode → OGZ
roomOGZ geom =
   OGZ 1024
    [OGZVar "skybox" (OStr "ik2k/env/iklake")]
     "fps"
     (Extras 0 0)
     (TextureMRU [2,4,3,5,7])
     [Entity (Vec3 520.0 520.0 516.0) PlayerStart 336 0 0 0 0 0]
     (octree solid solid solid solid solid solid solid geom)

simpleTestMap ∷ [Int] → OGZ
simpleTestMap = roomOGZ . maze2d'

dumpBytes ∷ [Word8] → String
dumpBytes = r 0 where
  r i [] = ""
  r 16 bs = '\n' : r 0 bs
  r 0 (b:bs) = printf "0x%02x" b ++ r 1 bs
  r i (b:bs) = " " ++ printf "0x%02x" b ++ r (i+1) bs

seed        = 9
octaves     = 5
scale       = 0.05
persistance = 0.5
perlinNoise = perlin seed octaves scale persistance

nByN ∷ Int → DIM2
nByN n = (Z :. n) :. n

nByNByN ∷ Int → DIM3
nByNByN n = Z :. n :. n :. n

type HeightMap2D = Array D ((Z :. Int) :. Int) Int

heightMap ∷ Noise a => a -> Int -> HeightMap2D
heightMap nf size = A.fromFunction (nByN size) (toHeight . noiseValue nf . fromIdx)
  where fromIdx ∷ (Z :. Int :. Int) → (Double,Double,Double)
        fromIdx ((Z :. a) :. b) = (fromIntegral a, fromIntegral b, 0)
        toHeight ∷ Double →  Int
        toHeight noiseVal = floor $ (fromIntegral size) * ((noiseVal+1)/2.0)

foobarzaz'  = heightMap perlinNoise 16
foobarzaz   = A.toList foobarzaz'

atl ∷ (A.Source r e, A.Shape sh) => Array r sh e → [e]
atl = A.toList

aHMap = heightMap perlinNoise 32
aBitField = pillars aHMap
a3dMaze = maze3d aBitField
terrain = roomOGZ $ treeOctN a3dMaze

test ∷ IO ()
test = do
  let outfile ∷ String
      outfile = "/Users/b/Library/Application Support/sauerbraten/packages/base/generated.ogz"
      outfile2 ∷ String
      outfile2 = "/Users/b/Library/Application Support/sauerbraten/packages/base/genterrain.ogz"

  g ← getStdGen
  let foo = take 256 $ randomRs (0,1) g

  traceM $ show foo
  let m = simpleTestMap foo -- stripes
  let mbytes = runPut $ put m

  traceM "<Writing to generated.ogz>"
  BL.writeFile outfile $ compress mbytes
  traceM "</Writing to generated.ogz>"

  traceM "<Writing to genterrain.ogz>"
  BL.writeFile outfile2 $ compress $ runPut $ put terrain
  traceM "</Writing to genterrain.ogz>"
