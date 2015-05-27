# Cube Worlds

This is a library for creating and manipulating Sauerbraten Maps.
For right now only version 29 of the ogz format is supported, and
the precomputed light map data is ignored.

The current output of a test run against of the version 29 Sauerbraten
maps with svn rev @2293 is as follows:

    PASS: map depth is 12 (./testdata/maps/aard3c.ogz)
    PASS: map depth is 10 (./testdata/maps/academy.ogz)
    PASS: map depth is 11 (./testdata/maps/akroseum.ogz)
    PASS: map depth is 10 (./testdata/maps/aqueducts.ogz)
    PASS: map depth is 10 (./testdata/maps/arabic.ogz)
    PASS: map depth is 11 (./testdata/maps/authentic.ogz)
    PASS: map depth is 11 (./testdata/maps/berlin_wall.ogz)
    PASS: map depth is 11 (./testdata/maps/bt_falls.ogz)
    PASS: map depth is 10 (./testdata/maps/campo.ogz)
    PASS: map depth is 10 (./testdata/maps/complex.ogz)
    PASS: map depth is 12 (./testdata/maps/core_transfer.ogz)
    PASS: map depth is 10 (./testdata/maps/curvy_castle.ogz)
    PASS: map depth is 10 (./testdata/maps/cwcastle.ogz)
    PASS: map depth is 12 (./testdata/maps/damnation.ogz)
    PASS: map depth is 10 (./testdata/maps/deathtek.ogz)
    PASS: map depth is 13 (./testdata/maps/desecration.ogz)
    PASS: map depth is 11 (./testdata/maps/dock.ogz)
    PASS: map depth is 12 (./testdata/maps/douze.ogz)
    PASS: map depth is 10 (./testdata/maps/duel7.ogz)
    PASS: map depth is 10 (./testdata/maps/duel8.ogz)
    PASS: map depth is 11 (./testdata/maps/dust2.ogz)
    PASS: map depth is 11 (./testdata/maps/europium.ogz)
    PASS: map depth is 10 (./testdata/maps/example.ogz)
    PASS: map depth is 12 (./testdata/maps/face-capture.ogz)
    PASS: map depth is 10 (./testdata/maps/fanatic_quake.ogz)
    PASS: map depth is 10 (./testdata/maps/fc3.ogz)
    PASS: map depth is 12 (./testdata/maps/flagstone.ogz)
    PASS: map depth is 10 (./testdata/maps/forge.ogz)
    PASS: map depth is 12 (./testdata/maps/frostbyte.ogz)
    PASS: map depth is 10 (./testdata/maps/frozen.ogz)
    PASS: map depth is 11 (./testdata/maps/hallo.ogz)
    PASS: map depth is 11 (./testdata/maps/hog2.ogz)
    PASS: map depth is 10 (./testdata/maps/industry.ogz)
    PASS: map depth is 11 (./testdata/maps/injustice.ogz)
    PASS: map depth is 10 (./testdata/maps/island.ogz)
    PASS: map depth is 11 (./testdata/maps/justice.ogz)
    PASS: map depth is 11 (./testdata/maps/kalking1.ogz)
    PASS: map depth is 11 (./testdata/maps/kffa.ogz)
    PASS: map depth is 11 (./testdata/maps/killcore3.ogz)
    PASS: map depth is 12 (./testdata/maps/kmap5.ogz)
    PASS: map depth is 10 (./testdata/maps/level9.ogz)
    PASS: map depth is 11 (./testdata/maps/lost.ogz)
    PASS: map depth is 10 (./testdata/maps/lostinspace.ogz)
    PASS: map depth is 11 (./testdata/maps/mach2.ogz)
    PASS: map depth is 11 (./testdata/maps/mbt1.ogz)
    PASS: map depth is 10 (./testdata/maps/mbt2.ogz)
    PASS: map depth is 10 (./testdata/maps/memento.ogz)
    PASS: map depth is 12 (./testdata/maps/metl2.ogz)
    PASS: map depth is 11 (./testdata/maps/metl3.ogz)
    PASS: map depth is 11 (./testdata/maps/monastery.ogz)
    PASS: map depth is 13 (./testdata/maps/nevil_c.ogz)
    PASS: map depth is 11 (./testdata/maps/nmp8.ogz)
    PASS: map depth is 12 (./testdata/maps/nmp9.ogz)
    PASS: map depth is 12 (./testdata/maps/ogrosupply.ogz)
    PASS: map depth is 10 (./testdata/maps/osiris.ogz)
    PASS: map depth is 10 (./testdata/maps/ot.ogz)
    PASS: map depth is 10 (./testdata/maps/park.ogz)
    PASS: map depth is 11 (./testdata/maps/ph-capture.ogz)
    PASS: map depth is 12 (./testdata/maps/powerplant.ogz)
    PASS: map depth is 11 (./testdata/maps/recovery.ogz)
    PASS: map depth is 11 (./testdata/maps/redemption.ogz)
    PASS: map depth is 11 (./testdata/maps/reissen.ogz)
    PASS: map depth is 11 (./testdata/maps/river_c.ogz)
    PASS: map depth is 13 (./testdata/maps/sacrifice.ogz)
    PASS: map depth is 12 (./testdata/maps/shindou.ogz)
    PASS: map depth is 10 (./testdata/maps/shinmei1.ogz)
    PASS: map depth is 13 (./testdata/maps/shipwreck.ogz)
    PASS: map depth is 11 (./testdata/maps/stemple.ogz)
    PASS: map depth is 11 (./testdata/maps/tejen.ogz)
    PASS: map depth is 12 (./testdata/maps/tempest.ogz)
    PASS: map depth is 12 (./testdata/maps/thor.ogz)
    PASS: map depth is 10 (./testdata/maps/torment.ogz)
    PASS: map depth is 10 (./testdata/maps/turbine.ogz)
    PASS: map depth is 12 (./testdata/maps/urban_c.ogz)
    PASS: map depth is 12 (./testdata/maps/valhalla.ogz)
    PASS: map depth is 10 (./testdata/maps/venice.ogz)
    PASS: map depth is 10 (./testdata/maps/wdcd.ogz)

    tests
      x∷WorldSize = unpack(pack): OK
        +++ OK, passed 100 tests.
      Header
        x ≡ load(dump x):         OK (0.08s)
          +++ OK, passed 5000 tests.
        x ≡ load(dump x):         OK (0.02s)
          256 tests completed
        b ≡ dump(load b):         OK
          77 tests completed
      OGZVar
        x ≡ load(dump x):         OK (0.19s)
          +++ OK, passed 5000 tests.
        x ≡ load(dump x):         OK (0.21s)
          1025 tests completed
        b ≡ dump(load b):         OK
          405 tests completed
      GameType
        x ≡ load(dump x):         OK (0.12s)
          +++ OK, passed 5000 tests.
        x ≡ load(dump x):         OK
          206 tests completed
        b ≡ dump(load b):         OK
          1 tests completed
      WorldSize
        x ≡ load(dump x):         OK
          +++ OK, passed 5000 tests.
        x ≡ load(dump x):         OK
          5 tests completed
        b ≡ dump(load b):         OK
          4 tests completed
      Extras
        x ≡ load(dump x):         OK
          +++ OK, passed 5000 tests.
        x ≡ load(dump x):         OK
          25 tests completed
        b ≡ dump(load b):         OK
          1 tests completed
      TextureMRU
        x ≡ load(dump x):         OK (0.13s)
          +++ OK, passed 5000 tests.
        x ≡ load(dump x):         OK
          206 tests completed
        b ≡ dump(load b):         OK
          75 tests completed
      EntTy
        x ≡ load(dump x):         OK (0.02s)
          +++ OK, passed 5000 tests.
        x ≡ load(dump x):         OK
          31 tests completed
        b ≡ dump(load b):         OK
          30 tests completed
      Vec3
        x ≡ load(dump x):         OK (0.06s)
          +++ OK, passed 5000 tests.
        x ≡ load(dump x):         OK (0.03s)
          343 tests completed
        b ≡ dump(load b):         OK (0.02s)
          10000 tests completed
      Entity
        x ≡ load(dump x):         OK (0.08s)
          +++ OK, passed 5000 tests.
        b ≡ dump(load b):         OK (0.04s)
          10000 tests completed
      Textures
        x ≡ load(dump x):         OK (0.04s)
          +++ OK, passed 5000 tests.
        x ≡ load(dump x):         OK (0.06s)
          729 tests completed
        b ≡ dump(load b):         OK (0.38s)
          123577 tests completed
      Offsets
        x ≡ load(dump x):         OK (0.04s)
          +++ OK, passed 5000 tests.
        x ≡ load(dump x):         OK
          125 tests completed
        b ≡ dump(load b):         OK (0.12s)
          47858 tests completed
      Material
        x ≡ load(dump x):         OK (0.01s)
          +++ OK, passed 5000 tests.
        x ≡ load(dump x):         OK
          6 tests completed
        b ≡ dump(load b):         OK
          15 tests completed
      Normals
        x ≡ load(dump x):         OK (0.05s)
          +++ OK, passed 5000 tests.
        b ≡ dump(load b):         OK (0.27s)
          91482 tests completed
      LightMap
        x ≡ load(dump x):         OK (0.02s)
          +++ OK, passed 5000 tests.
        x ≡ load(dump x):         OK
          6 tests completed
        b ≡ dump(load b):         OK
          12 tests completed
      FaceInfoPlus
        x ≡ load(dump x):         OK (0.09s)
          +++ OK, passed 5000 tests.
        x ≡ load(dump x):         OK
          8 tests completed
        b ≡ dump(load b):         OK (0.02s)
          10000 tests completed
      Properties
        x ≡ load(dump x):         OK (0.73s)
          +++ OK, passed 5000 tests.
        b ≡ dump(load b):         OK (6.96s)
          876252 tests completed
      MergeInfo
        x ≡ load(dump x):         OK (0.03s)
          +++ OK, passed 5000 tests.
        b ≡ dump(load b):         OK (1.28s)
          452414 tests completed
      MergeData
        x ≡ load(dump x):         OK (0.15s)
          +++ OK, passed 5000 tests.
        b ≡ dump(load b):         OK (2.82s)
          547625 tests completed
      Octree
        x ≡ load(dump x):         OK (2.91s)
          +++ OK, passed 5000 tests.
      OGZ
        x ≡ load(dump x):         OK (38.29s)
          +++ OK, passed 5000 tests.

    All 52 tests passed (55.48s)
