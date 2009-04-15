#!/bin/sh

aclocal -I `gauche-config --ac`
grep -q "GAUCHE" aclocal.m4 || cat `gauche-config --ac`/aclocal.m4 >>aclocal.m4
autoconf
