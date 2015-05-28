{-# LANGUAGE TypeOperators #-}

module Gen (
  BitField2D, BitField3D, HeightMap, SauerbratenMap,
  Cube, Entity,
  empty, solid, entities,
  floorPattern, voxelPattern, heightMap, sauerbraten)
  where

import Orphans()
import HomoTuple
import qualified Types as Sauer

import Prelude hiding (concat, mapM, mapM_, sum)

import           Control.Applicative hiding (empty)
import           Data.Array.Repa     ((:.) (..), Array, D, DIM2, DIM3, Z (Z))
import qualified Data.Array.Repa     as A
import           Data.Bits
import           Data.Foldable
import           Data.Traversable
import           Prelude.Unicode
import           Text.Printf


-- Types -----------------------------------------------------------------------

type BitField2D r = Array r DIM2 Bool
type BitField3D r = Array r DIM3 Bool
type HeightMap r = Array r DIM2 Int

type SauerbratenMap = Sauer.OGZ
type Cube = Tree Bool

data Entity

data Tree a = Branch !(LzTup8 (Tree a))
            | Leaf !a
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)



-- Basics ----------------------------------------------------------------------

empty,solid ∷ Cube
empty = Leaf False
solid = Leaf True

entities ∷ Cube → [Entity]
entities = const []


-- Conversion ------------------------------------------------------------------

-- TODO Don't hard-code D! Why can't this work for any internal representation?
floorPattern ∷ BitField2D D → Cube
floorPattern bf = f3 <$> indexTree depth
  where (Z:.w:.h, f2) = A.toFunction bf
        f3 (Z :. x :. y :. z) | z≡bottom = f2 (Z :. x :. y)
        f3 (Z :. _ :. _ :. _)            = False
        errmsg = "2D bit fields must be squares with sizes that arepowers of two."
        bottom = (depth*depth) - 1
        depth = case (depthNeeded w, w≡h) of
                  (Just d, True) → d
                  _              → error errmsg

-- TODO Don't hard-code D! Why can't this work for any internal representation?
voxelPattern ∷ BitField3D D → Cube
voxelPattern bf = f <$> indexTree depth
  where (Z:.w:.h:.d, f) = A.toFunction bf
        errmsg = "3d bit fields must be cubes with sizes that are powers of two. Not %s"
        depth = case (depthNeeded w, w≡h && h≡d) of
                  (Just requiredDepth, True) → requiredDepth
                  _                          → error(printf errmsg(show(w,h,d)))

heightMap ∷ HeightMap D → Cube
heightMap = voxelPattern . pillars

sauerbraten ∷ Cube → SauerbratenMap
sauerbraten = roomOGZ . treeOctN


-- Operations ------------------------------------------------------------------

noProps ∷ Sauer.Properties
noProps = Sauer.Properties Nothing $ Sauer.Faces $
            Tup6 Nothing Nothing Nothing
                 Nothing Nothing Nothing

nSolid ∷ Sauer.Octree
nSolid = Sauer.NSolid (Sauer.Textures (Tup6 2 3 4 5 6 7)) noProps Nothing

nEmpty ∷ Sauer.Octree
nEmpty = Sauer.NEmpty (Sauer.Textures (Tup6 0 0 0 0 0 0)) noProps Nothing

nByNByN ∷ Int → DIM3
nByNByN n = Z :. n :. n :. n

pillars ∷ HeightMap D → BitField3D D
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

treeOctN ∷ Tree Bool → Sauer.Octree
treeOctN (Leaf False) = nEmpty
treeOctN (Leaf True)  = nSolid
treeOctN (Branch b)   = Sauer.NBroken $ treeOctN <$> b

roomOGZ ∷ Sauer.Octree → SauerbratenMap
roomOGZ geom =
  Sauer.OGZ
    (Sauer.WorldSize 10)
    [Sauer.OGZVar "skybox" (Sauer.OStr "ik2k/env/iklake")]
    (Sauer.GameType "fps")
    (Sauer.Extras 0 0)
    (Sauer.TextureMRU [2,4,3,5,7])
    [Sauer.Entity (Sauer.Vec3 (Tup3 520.0 520.0 516.0))
                  (Tup5 0 0 0 0 0)
                  Sauer.PlayerStart
                  0]
    (LzTup8 nEmpty nEmpty nEmpty nEmpty nEmpty nEmpty nEmpty geom)
