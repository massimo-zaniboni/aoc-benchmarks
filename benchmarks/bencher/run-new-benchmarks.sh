#!/usr/bin/env bash

# Run only new tests, i.e. new code or updated code.

python bin/bencher.py

VIEWER_DIR=/benchmarks/mybenchmarks/websites/linux

rm -f $VIEWER_DIR/data/*
rm -f $VIEWER_DIR/code/*

mkdir -p $VIEWER_DIR/data
mkdir -p $VIEWER_DIR/code

cp summary/* $VIEWER_DIR/data/.
cp run_markup/* $VIEWER_DIR/code/.
