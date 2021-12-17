#!/usr/bin/env bash

# Run only new tests, i.e. new code or updated code.

python bin/bencher.py

rm -r -f ../viewer/websites/linux/data
rm -r -f ../viewer/websites/linux/code

cp summary/* ../viewer/websites/linux/data/.
