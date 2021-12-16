#!/usr/bin/env bash

# Connect to the running container

DOCKER_CMD=podman
# or
# DOCKER=docker


$DOCKER_CMD exec -it aoc-benchmarks bash
