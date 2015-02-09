# ogz-gen

This is a library for reading, writing, and manipulating Sauerbraten
Maps. For right now only version 29 of the ogz format is supported,
and all lighting information is ignored.

The current output of a test run against the Sauerbraten maps with
svn rev @2293 is as follows:

    PASS: 8 node were parsed (example.ogz)
    FAIL: Only version 29 is supported. This map has version 27 (packages/base/DM_BS1.ogz)
    PASS: 17576 node were parsed (packages/base/aard3c.ogz)
    PASS: 26704 node were parsed (packages/base/academy.ogz)
    PASS: 873368 node were parsed (packages/base/akroseum.ogz)
    PASS: 111160 node were parsed (packages/base/aqueducts.ogz)
    PASS: 163904 node were parsed (packages/base/arabic.ogz)
    FAIL: Only version 29 is supported. This map has version 28 (packages/base/asteroids.ogz)
    PASS: 93696 node were parsed (packages/base/authentic.ogz)
    PASS: 1566480 node were parsed (packages/base/berlin_wall.ogz)
    FAIL: Only version 29 is supported. This map has version 24 (packages/base/box_demo.ogz)
    PASS: 29976 node were parsed (packages/base/bt_falls.ogz)
    FAIL: Only version 29 is supported. This map has version 28 (packages/base/c_egypt.ogz)
    FAIL: Only version 29 is supported. This map has version 27 (packages/base/c_valley.ogz)
    PASS: 415112 node were parsed (packages/base/campo.ogz)
    FAIL: Only version 29 is supported. This map has version 27 (packages/base/canyon.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/capture_night.ogz)
    FAIL: Only version 29 is supported. This map has version 27 (packages/base/castle_trap.ogz)
    PASS: 29432 node were parsed (packages/base/complex.ogz)
    PASS: 204592 node were parsed (packages/base/core_transfer.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/corruption.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/curvedm.ogz)
    PASS: 64104 node were parsed (packages/base/curvy_castle.ogz)
    PASS: 166688 node were parsed (packages/base/cwcastle.ogz)
    PASS: 265776 node were parsed (packages/base/damnation.ogz)
    FAIL: Only version 29 is supported. This map has version 24 (packages/base/darkdeath.ogz)
    PASS: 35520 node were parsed (packages/base/deathtek.ogz)
    PASS: 212152 node were parsed (packages/base/desecration.ogz)
    PASS: 61712 node were parsed (packages/base/dock.ogz)
    FAIL: Only version 29 is supported. This map has version 21 (packages/base/door_demo.ogz)
    PASS: 60176 node were parsed (packages/base/douze.ogz)
    PASS: 73944 node were parsed (packages/base/duel7.ogz)
    PASS: 178224 node were parsed (packages/base/duel8.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/duomo.ogz)
    PASS: 114392 node were parsed (packages/base/dust2.ogz)
    PASS: 124160 node were parsed (packages/base/europium.ogz)
    PASS: 37048 node were parsed (packages/base/face-capture.ogz)
    PASS: 23368 node were parsed (packages/base/fanatic_quake.ogz)
    FAIL: Only version 29 is supported. This map has version 28 (packages/base/fb_capture.ogz)
    PASS: 52184 node were parsed (packages/base/fc3.ogz)
    FAIL: Only version 29 is supported. This map has version 28 (packages/base/fc4.ogz)
    FAIL: Only version 29 is supported. This map has version 6 (packages/base/firstevermap.ogz)
    PASS: 81536 node were parsed (packages/base/flagstone.ogz)
    PASS: 171840 node were parsed (packages/base/forge.ogz)
    FAIL: Only version 29 is supported. This map has version 27 (packages/base/fragplaza.ogz)
    PASS: 39376 node were parsed (packages/base/frostbyte.ogz)
    PASS: 97240 node were parsed (packages/base/frozen.ogz)
    FAIL: Only version 29 is supported. This map has version 27 (packages/base/guacamole.ogz)
    FAIL: Only version 29 is supported. This map has version 27 (packages/base/hades.ogz)
    PASS: 127264 node were parsed (packages/base/hallo.ogz)
    PASS: 36008 node were parsed (packages/base/hog2.ogz)
    PASS: 84600 node were parsed (packages/base/industry.ogz)
    PASS: 164320 node were parsed (packages/base/injustice.ogz)
    PASS: 21168 node were parsed (packages/base/island.ogz)
    PASS: 61016 node were parsed (packages/base/justice.ogz)
    PASS: 56568 node were parsed (packages/base/kalking1.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/katrez_d.ogz)
    PASS: 16664 node were parsed (packages/base/kffa.ogz)
    PASS: 142968 node were parsed (packages/base/killcore3.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/killfactory.ogz)
    PASS: 19832 node were parsed (packages/base/kmap5.ogz)
    FAIL: Only version 29 is supported. This map has version 28 (packages/base/konkuri-to.ogz)
    FAIL: Only version 29 is supported. This map has version 21 (packages/base/ksauer1.ogz)
    FAIL: Only version 29 is supported. This map has version 28 (packages/base/l_ctf.ogz)
    FAIL: Only version 29 is supported. This map has version 21 (packages/base/ladder.ogz)
    PASS: 238144 node were parsed (packages/base/level9.ogz)
    PASS: 492672 node were parsed (packages/base/lost.ogz)
    PASS: 78752 node were parsed (packages/base/lostinspace.ogz)
    PASS: 151704 node were parsed (packages/base/mach2.ogz)
    PASS: 382640 node were parsed (packages/base/mbt1.ogz)
    PASS: 55576 node were parsed (packages/base/mbt2.ogz)
    PASS: 56248 node were parsed (packages/base/memento.ogz)
    PASS: 39272 node were parsed (packages/base/metl2.ogz)
    PASS: 23032 node were parsed (packages/base/metl3.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/metl4.ogz)
    PASS: 209168 node were parsed (packages/base/monastery.ogz)
    FAIL: Only version 29 is supported. This map has version 24 (packages/base/moonlite.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/mpsp10.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/mpsp6a.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/mpsp6b.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/mpsp6c.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/mpsp9a.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/mpsp9b.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/mpsp9c.ogz)
    FAIL: Only version 29 is supported. This map has version 28 (packages/base/neondevastation.ogz)
    FAIL: Only version 29 is supported. This map has version 27 (packages/base/neonpanic.ogz)
    PASS: 81344 node were parsed (packages/base/nevil_c.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/nmp4.ogz)
    PASS: 29408 node were parsed (packages/base/nmp8.ogz)
    PASS: 28784 node were parsed (packages/base/nmp9.ogz)
    FAIL: Only version 29 is supported. This map has version 28 (packages/base/oasis.ogz)
    FAIL: Only version 29 is supported. This map has version 27 (packages/base/oddworld.ogz)
    PASS: 226384 node were parsed (packages/base/ogrosupply.ogz)
    FAIL: Only version 29 is supported. This map has version 28 (packages/base/orbe.ogz)
    FAIL: Only version 29 is supported. This map has version 21 (packages/base/orion.ogz)
    PASS: 86152 node were parsed (packages/base/osiris.ogz)
    PASS: 19152 node were parsed (packages/base/ot.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/paradigm.ogz)
    PASS: 50144 node were parsed (packages/base/park.ogz)
    FAIL: Only version 29 is supported. This map has version 21 (packages/base/pgdm.ogz)
    PASS: 33856 node were parsed (packages/base/ph-capture.ogz)
    FAIL: Only version 29 is supported. This map has version 27 (packages/base/phosgene.ogz)
    FAIL: Only version 29 is supported. This map has version 24 (packages/base/platform_demo.ogz)
    PASS: 67976 node were parsed (packages/base/powerplant.ogz)
    PASS: 337864 node were parsed (packages/base/recovery.ogz)
    PASS: 177912 node were parsed (packages/base/redemption.ogz)
    FAIL: Only version 29 is supported. This map has version 28 (packages/base/refuge.ogz)
    PASS: 240320 node were parsed (packages/base/reissen.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/relic.ogz)
    PASS: 97016 node were parsed (packages/base/river_c.ogz)
    FAIL: Only version 29 is supported. This map has version 26 (packages/base/roughinery.ogz)
    FAIL: Only version 29 is supported. This map has version 24 (packages/base/ruby.ogz)
    PASS: 80896 node were parsed (packages/base/sacrifice.ogz)
    FAIL: Only version 29 is supported. This map has version 8 (packages/base/sauerbraten.ogz)
    FAIL: Only version 29 is supported. This map has version 27 (packages/base/sdm1.ogz)
    FAIL: Only version 29 is supported. This map has version 7 (packages/base/secondevermap.ogz)
    FAIL: Only version 29 is supported. This map has version 25 (packages/base/serenity.ogz)
    FAIL: Only version 29 is supported. This map has version 21 (packages/base/shadowed.ogz)
    PASS: 27208 node were parsed (packages/base/shindou.ogz)
    PASS: 208736 node were parsed (packages/base/shinmei1.ogz)
    PASS: 70840 node were parsed (packages/base/shipwreck.ogz)
    FAIL: Only version 29 is supported. This map has version 21 (packages/base/spiralz.ogz)
    PASS: 84176 node were parsed (packages/base/stemple.ogz)
    FAIL: Only version 29 is supported. This map has version 24 (packages/base/tartech.ogz)
    PASS: 262576 node were parsed (packages/base/tejen.ogz)
    PASS: 298392 node were parsed (packages/base/tempest.ogz)
    FAIL: Only version 29 is supported. This map has version 21 (packages/base/thetowers.ogz)
    PASS: 81960 node were parsed (packages/base/thor.ogz)
    PASS: 41128 node were parsed (packages/base/torment.ogz)
    PASS: 18816 node were parsed (packages/base/turbine.ogz)
    PASS: 226984 node were parsed (packages/base/urban_c.ogz)
    PASS: 253264 node were parsed (packages/base/valhalla.ogz)
    PASS: 386592 node were parsed (packages/base/venice.ogz)
    FAIL: Only version 29 is supported. This map has version 27 (packages/base/wake5.ogz)
    PASS: 52920 node were parsed (packages/base/wdcd.ogz)
