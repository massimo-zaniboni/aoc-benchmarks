#!/usr/bin/env bash

mkdir -p /benchmarks/bencher/tmp
cd /benchmarks/bencher/tmp
wget https://downloads.asterisell.com/aoc/datasets-01.tar.lrz
lrzuntar datasets-01.tar.lrz
rm -f datasets-01.tar.lrz

# NOTE: leave a filter on name, otherwise "." will be used in the "rm -r -f ." command!!!
cd datasets-01
for f in $(find . -name 'aoc*' -type d)
do
  rm -r -f ../$(basename $f)
  mv $f ../.
done
cd ..
rm -r -f datasets-01
