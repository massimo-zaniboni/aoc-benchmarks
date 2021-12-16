#!/usr/bin/env bash

# Connect to the running container

DOCKER_CMD=podman
# or
# DOCKER=docker

$DOCKER_CMD run --rm -it  -v "$(pwd)/bencher:/bencher" aoc-benchmarks /bin/bash
