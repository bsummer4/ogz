module Main where

import           Control.Applicative
import           Data.Array.Repa      ((:.) (..), D, DIM2, Z (Z))
import qualified Data.Array.Repa      as A
import qualified Data.List            as L
import           Debug.Trace
import qualified Gen
import           Mapfile
import           Numeric.Noise
import           Numeric.Noise.Perlin
import           Prelude.Unicode
import           System.Random

nByN ∷ Int → DIM2
nByN n = (Z :. n) :. n

genTerrain ∷ Noise a => a -> Int -> Gen.HeightMap D
genTerrain nf size = A.fromFunction (nByN size) (toHeight . noiseValue nf . fromIdx)
  where fromIdx ∷ (Z :. Int :. Int) → (Double,Double,Double)
        fromIdx ((Z :. a) :. b) = (fromIntegral a, fromIntegral b, 0)
        toHeight ∷ Double →  Int
        toHeight noiseVal = floor $ fromIntegral size * ((noiseVal+1)/2.0)

squareRoot :: Int -> Int
squareRoot = f
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

maze ∷ [Int] → Gen.BitField2D D
maze ls = A.delay $ A.fromListUnboxed shape $ (≠0) <$> ls
  where shape = Z :. size :. size
        size = squareRoot $ length ls

genMaps ∷ IO ()
genMaps = do
    let outfile ∷ String
        outfile = "/Users/b/Library/Application Support/sauerbraten/packages/base/generated.ogz"
        outfile2 ∷ String
        outfile2 = "/Users/b/Library/Application Support/sauerbraten/packages/base/genterrain.ogz"

    let seed        = 9
    let octaves     = 5
    let scale       = 0.05
    let persistance = 0.5
    let perlinNoise = perlin seed octaves scale persistance

    let foobarzaz'  = genTerrain perlinNoise 16
    let _foobarzaz   = A.toList foobarzaz'

    let aHMap = genTerrain perlinNoise 32
    let a3dMaze = Gen.heightMap aHMap
    let terrain = Gen.sauerbraten a3dMaze

    g ← getStdGen
    let foo = take 256 $ randomRs (0,1) g

    traceM "<Generating map=\"generated.ogz\">"
    traceM $ show foo
    let m = Gen.sauerbraten . Gen.floorPattern . maze $ foo

    dumpOGZ outfile m
    traceM "</Generating>"

    traceM "<Generating map=\"genterrain.ogz\">"
    dumpOGZ outfile2 terrain
    traceM "</Generating>"

main ∷ IO ()
main = do
  genMaps
  testLoad
  exaustiveTest
