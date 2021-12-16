#!/usr/bin/env bash

DOCKER_CMD=podman
# or
# DOCKER=docker

$DOCKER_CMD run --rm -it  -v "$(pwd)/bencher:/bencher" aoc-benchmarks ./run-all-benchmarks.sh
