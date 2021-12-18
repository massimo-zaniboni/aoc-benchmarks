#!/usr/bin/env bash

# Create potentially missing directories
mkdir -p run_logs
mkdir -p run_markup
mkdir -p summary

# Update the date of all programs to test
find programs -type f -exec touch {} \;

# Remove the expected results.
# The first invocation of a program is used as expected result.
find tmp/ -name '*_out' -type f -exec rm {} \;

# Clean log and intermediate results.
rm -f run_logs/*
rm -f run_markup/*
rm -f summary/*

./run-new-benchmarks.sh
