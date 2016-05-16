#!/bin/bash

set -e

IMAGE_NAME="juloo_image"
CONTAINER_NAME="juloo"

docker build -t "$IMAGE_NAME" .
docker stop "$CONTAINER_NAME" 2> /dev/null && docker rm "$CONTAINER_NAME" || true
docker run -P --privileged -d --name "$CONTAINER_NAME" "$IMAGE_NAME"

printf "IP: " ; docker-machine ip default
printf "Port: " ; docker port "$CONTAINER_NAME" 22 | sed 's/.*://'
