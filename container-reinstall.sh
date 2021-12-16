#!/usr/bin/env bash

# Rebuild the image and run a new container

DOCKER_CMD=podman
# or
# DOCKER=docker

$DOCKER_CMD stop aoc-benchmarks
$DOCKER_CMD rm aoc-benchmarks
$DOCKER_CMD build -t aoc-benchmarks .
$DOCKER_CMD run -d -v "$(pwd)/bencher:/host" --name aoc-benchmarks aoc-benchmarks
