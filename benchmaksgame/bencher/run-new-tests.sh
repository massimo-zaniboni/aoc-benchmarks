#!/usr/bin/env bash

python bin/bencher.py

# Send data to the website

rm ../mybenchmarks/websites/linux/data/*
cp --force summary/* ../mybenchmarks/websites/linux/data/.

rm --force ../mybenchmarks/websites/linux/code/*
cp --force run_markup/* ../mybenchmarks/websites/linux/code/.
