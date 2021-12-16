#!/usr/bin/env bash

# Rebuild the image and run a new container

DOCKER=podman
# or
# DOCKER=docker

$DOCKER stop aoc-benchmarks
$DOCKER rm aoc-benchmarks
$DOCKER build -t aoc-benchmarks .
$DOCKER run -d -p 8000:8000 -v "$(pwd)/benchmaksgame:/host" --name aoc-benchmarks aoc-benchmarks
