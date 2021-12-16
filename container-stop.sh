#!/usr/bin/env bash

# Run the container, without rebuilding the image

DOCKER_CMD=podman
# or
# DOCKER=docker

$DOCKER_CMD stop aoc-benchmarks
