#!/usr/bin/env bash

# Update the date of all programs to test
find programs -type f -exec touch {} \;

# Remove the expected results.
# The first invocation of a program is used as expected result.
find tmp/ -name '*_out' -type f -exec rm {} \;

# Clean log and intermediate results.
rm -r -f run_logs/*
rm -r -f run_markup/*
rm -r -f summary/*

./run-new-benchmarks.sh
