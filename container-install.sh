#!/usr/bin/env bash

# Rebuild the image and run a new container

DOCKER_CMD=podman
# or
# DOCKER=docker

$DOCKER_CMD build -t aoc-benchmarks .
$DOCKER_CMD run --rm -v "$(pwd)/bencher:/bencher" aoc-benchmarks /bin/bash /init-datasets.sh
