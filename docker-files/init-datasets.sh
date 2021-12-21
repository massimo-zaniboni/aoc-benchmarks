#!/usr/bin/env bash

install_dataset () {
  mkdir -p /benchmarks/bencher/tmp
  cd       /benchmarks/bencher/tmp
  lrzuntar /downloads/"$1.tar.lrz"

  # NOTE: leave a filter on name, otherwise "." will be used in the "rm -r -f ." command!!!
  cd "$1"
  for f in $(find . -name 'aoc*' -type d)
  do
    mkdir -p ../$(basename $f)
  done

  # NOTE: leave a filter on name, otherwise "." will be used in the "rm -r -f ." command!!!
  for f in $(find . -name '*.txt' -type f)
  do
    mv --force "$f" ../$f
  done

  # NOTE: leave a filter on name, otherwise "." will be used in the "rm -r -f ." command!!!
  for f in $(find . -name '*.txt' -type l)
  do
    mv --force "$f" ../$f
  done

  # Remove temporary directory
  cd ..
  rm -r -f "$1"
}

install_dataset "datasets-01"
