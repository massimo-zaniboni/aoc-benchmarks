#!/usr/bin/env bash

# Run the container, without rebuilding the image

DOCKER=podman
# or
# DOCKER=docker

$DOCKER restart aoc-benchmarks
