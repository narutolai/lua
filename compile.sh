#! /bin/bash
gcc -g -O0 test.c  -L. -llua -lm -ldl
gcc -g -O0 -fPIC -shared -o myUdata.so mylib.c -I. -L. -llua
