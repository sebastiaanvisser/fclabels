#!/bin/sh

set -e

OUT=dist/hpc
SUITE=./dist/build/suite/suite

rm -f suite.tix

$SUITE $*

rm -Rf $OUT
mkdir -p $OUT

echo "Generating HPC report"
hpc markup --exclude=Main --destdir=$OUT suite >/dev/null 2>&1

rm -f suite.tix

