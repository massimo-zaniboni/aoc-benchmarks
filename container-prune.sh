#!/usr/bin/env bash

# Run the container, without rebuilding the image

DOCKER_CMD=podman
# or
# DOCKER=docker

$DOCKER_CMD stop aoc-benchmarks
$DOCKER_CMD rm aoc-benchmarks
$DOCKER_CMD rmi aoc-benchmarks

echo "For removing image files execute"
echo ""
echo "  $DOCKER_CMD image prune --all"
echo ""
echo "This command will affect the images of all stopped containers!"
echo ""
echo "Maybe use the less aggresive"
echo ""
echo "  $DOCKER_CMD image prune"
echo ""
echo "The most aggressive command is"
echo ""
echo "  $DOCKER_CMD system prune"
echo ""
