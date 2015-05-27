module Main where

import qualified Data.Array.Repa as A
import           Debug.Trace
import qualified Gen
import           Mapfile
import           System.Random
import           Numeric.Noise.Perlin

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

    let foobarzaz'  = Gen.heightMap perlinNoise 16
    let _foobarzaz   = A.toList foobarzaz'

    let aHMap = Gen.heightMap perlinNoise 32
    let aBitField = Gen.pillars aHMap
    let a3dMaze = Gen.maze3d aBitField
    let terrain = Gen.roomOGZ $ Gen.treeOctN a3dMaze

    g ← getStdGen
    let foo = take 256 $ randomRs (0,1) g

    traceM $ show foo
    let m = Gen.roomOGZ . Gen.maze2d' $ foo

    traceM "<Writing to generated.ogz>"
    dumpOGZ outfile m
    traceM "</Writing to generated.ogz>"

    traceM "<Writing to genterrain.ogz>"
    dumpOGZ outfile2 terrain
    traceM "</Writing to genterrain.ogz>"

main ∷ IO ()
main = do
  genMaps
  testLoad
  exaustiveTest
