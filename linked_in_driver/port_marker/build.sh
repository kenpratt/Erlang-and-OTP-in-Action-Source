#! /bin/sh
gcc  -O -g --std=c99 -Wall `MagickWand-config --cflags --cppflags` -o priv/marker c_src/main.c `MagickWand-config --ldflags --libs`

erlc +debug_info -o ./ebin ./src/*.erl