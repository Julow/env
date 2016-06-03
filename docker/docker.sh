#!/bin/bash

docker-machine start default 2> /dev/null || true

eval "`docker-machine env default`"

IMAGE_NAME="juloo_image"
CONTAINER_NAME="juloo"
SHARE_FOLTER="/Users/Shared/juloo:/juloo"
RUN_CMD="bash"

if [[ "$1" == "re" ]]; then
	docker kill "$CONTAINER_NAME" 2> /dev/null
	docker rm "$CONTAINER_NAME" 2> /dev/null
	CONTAINER_STATE=""
else
	CONTAINER_STATE=$(docker inspect -f {{.State.Running}} "$CONTAINER_NAME" 2> /dev/null)
fi

if [[ "$CONTAINER_STATE" == "true" ]] ; then
	echo "Already running"
	docker exec -i -t "$CONTAINER_NAME" "$RUN_CMD"
elif [[ "$CONTAINER_STATE" == "false" ]]; then
	echo "Restart"
	docker start -i "$CONTAINER_NAME"
else
	docker build -t "$IMAGE_NAME" .
	docker run -P --privileged -it -v "$SHARE_FOLTER" --name "$CONTAINER_NAME" "$IMAGE_NAME" "$RUN_CMD"
fi
