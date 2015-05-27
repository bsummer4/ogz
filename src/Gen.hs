{-# LANGUAGE TypeOperators #-}

module Gen
  (roomOGZ, heightMap, pillars, maze3d, treeOctN, maze2d',leaves,stripes)
  where

import Types
import Orphans()
import HomoTuple

import Prelude hiding (concat, mapM, mapM_, sum)

import           Control.Applicative hiding (empty)
import           Data.Array.Repa     ((:.) (..), Array, D, DIM2, DIM3, Z (Z))
import qualified Data.Array.Repa     as A
import           Data.Bits
import           Data.Foldable
import qualified Data.List           as L
import           Data.Monoid
import           Debug.Trace
import           Numeric.Noise
import           Prelude.Unicode
import           Text.Printf


-- Types -----------------------------------------------------------------------

type BitField2d r = Array r DIM2 Bool
type BitField3d r = Array r DIM3 Bool

data Tree a = Branch (LzTup8 (Tree a))
            | Leaf a
  deriving (Show,Eq,Ord,Functor)

type HeightMap2D = Array D ((Z :. Int) :. Int) Int


-- Operations ------------------------------------------------------------------

noProps ∷ Properties
noProps = Properties Nothing $ Faces $ Tup6 Nothing Nothing Nothing
                                            Nothing Nothing Nothing

solid ∷ Octree
solid = NSolid (Textures (Tup6 2 3 4 5 6 7)) noProps Nothing

empty ∷ Octree
empty = NEmpty (Textures (Tup6 0 0 0 0 0 0)) noProps Nothing

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

-- TODO This is just a guess! Figure out how the indexing system actually works.
indexTree ∷ Int → Tree DIM3
indexTree depth = recurse depth (Z:.0:.0:.0)
  where recurse 0 i = Leaf i
        recurse d (Z:.px:.py:.pz) =
          let (x,y,z) = (px*2,py*2,pz*2) in
          Branch $ LzTup8
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
        errmsg = "3d bit fields must be cubes with sizes that are powers of two. Not %s"
        depth = case (depthNeeded w, w≡h && h≡d) of
                  (Just requiredDepth, True) → requiredDepth
                  _                          → error(printf errmsg(show(w,h,d)))

maze2d ∷ BitField2d D → Tree Bool
maze2d bf = f3 <$> indexTree depth
  where (Z:.w:.h, f2) = A.toFunction bf
        f3 (Z :. x :. y :. z) | z≡bottom = f2 (Z :. x :. y)
        f3 (Z :. _ :. _ :. z)            = traceShow z False
        bottom = (depth*depth) - 1
        depth = traceShowId $ traceShowId $ traceShowId $
          case (depthNeeded w, w≡h) of
            (Just d, True) → d
            _              → error
              "2d bit fields must be squares with sizes that arepowers of two."

squareRoot :: Int -> Int
squareRoot = trace "squareRoot" . traceShowId . f . traceShowId
  where f n = case n of
          0 → 0
          1 → 1
          _ → let (^!) :: Num a => a -> Int -> a
                  (^!) x y = x^y
                  twopows = L.iterate (^!2) 2
                  (lowerRoot, lowerN) =
                     L.last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
                  newtonStep x = div (x + div n x) 2
                  iters = L.iterate newtonStep (f (div n lowerN) * lowerRoot)
                  isRoot r  =  r^!2 <= n && n < (r+1)^!2
              in  L.head $ L.dropWhile (not . isRoot) iters

maze2d' ∷ [Int] → Octree
maze2d' ls = treeOctN $ maze2d $ A.delay $ A.fromListUnboxed shape $ (≠0) <$> ls
  where shape = Z :. size :. size
        size = squareRoot $ traceShowId $ length ls

stripes ∷ [Int]
stripes = x4 $ x4 $ x4 [0,0,1,1]
  where x4 a = mconcat[a,a,a,a]

treeOctN ∷ Tree Bool → Octree
treeOctN (Leaf False) = empty
treeOctN (Leaf True)  = solid
treeOctN (Branch b)   = NBroken $ treeOctN <$> b

roomOGZ ∷ Octree → OGZ
roomOGZ geom =
   OGZ (WorldSize 10)
       [OGZVar "skybox" (OStr "ik2k/env/iklake")]
       (GameType "fps")
       (Extras 0 0)
       (TextureMRU [2,4,3,5,7])
       [Entity (Vec3 (Tup3 520.0 520.0 516.0)) (Tup5 0 0 0 0 0) PlayerStart 0]
       (LzTup8 solid solid solid solid solid solid solid geom)

nByN ∷ Int → DIM2
nByN n = (Z :. n) :. n

nByNByN ∷ Int → DIM3
nByNByN n = Z :. n :. n :. n

heightMap ∷ Noise a => a -> Int -> HeightMap2D
heightMap nf size = A.fromFunction (nByN size) (toHeight . noiseValue nf . fromIdx)
  where fromIdx ∷ (Z :. Int :. Int) → (Double,Double,Double)
        fromIdx ((Z :. a) :. b) = (fromIntegral a, fromIntegral b, 0)
        toHeight ∷ Double →  Int
        toHeight noiseVal = floor $ (fromIntegral size) * ((noiseVal+1)/2.0)
