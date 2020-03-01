#!/usr/bin/env bash

#
# Manage a docker image/container for a given Dockerfile
# Use the Dockerfile in the current directory
#
# The current directory is shared to `/shared`
#
# Usage:
# 	`docker.sh [re]`
# 		Run/restart a killed/stopped container
# 		If `re` is present, force rebuild
# -
# 	`docker.sh kill`
# 		Kill the running container
# -
# 	`docker.sh kill-all`
# 		Kill all running containers
# -
# 	`docker.sh shell [<cmd>]`
# 		Open a shell on the running container
# 		(Default value for `<cmd>` is `bash`)
# -
# 	`docker.sh rm`
# 		Remove the image/container for the current Dockerfile
# -
# 	`docker.sh rm-all`
# 		Remove all the images/containers
# -
# 	`docker.sh docker <cmd>`
# 		Run the docker command `<cmd>`

# -

if [[ -f "/usr/local/bin/docker" ]]; then
	PATH="/usr/local/bin:$PATH"
fi

docker-machine start default 2> /dev/null || true

eval "`docker-machine env default`"

# -

OPTION_RE=0

BASE_NAME="$(pwd -P | tr '[:upper:]/' '[:lower:].')"

echo "Current docker: ${BASE_NAME#.}"

IMAGE_NAME="i$BASE_NAME"
CONTAINER_NAME="c$BASE_NAME"

SHARE_FOLDER="$(pwd):/shared"

# -

if [[ $# > 0 ]]; then
	if [[ "$1" == "docker" ]]; then
		shift
		docker "$@"
		exit
	elif [[ "$1" == "re" ]]; then
		OPTION_RE=1
		shift
	elif [[ "$1" == "kill" ]]; then
		docker kill -s 9 "$CONTAINER_NAME"
		exit
	elif [[ "$1" == "kill-all" ]]; then
		TMP=$(docker ps -q)
		[[ -n $TMP ]] && docker kill -s 9 $TMP
		exit
	elif [[ "$1" == "shell" ]]; then
		docker exec -it "$CONTAINER_NAME" "${2:-bash}"
		exit
	elif [[ "$1" == "rm" ]]; then
		docker rm -f "$CONTAINER_NAME"
		docker rmi -f "$IMAGE_NAME"
		exit 0
	elif [[ "$1" == "rm-all" ]]; then
		TMP=$(docker ps -aq)
		[[ -n $TMP ]] && docker rm -f $TMP
		TMP=$(docker images -q)
		[[ -n $TMP ]] && docker rmi -f $TMP
		exit 0
	fi
fi

if ! [[ -f "Dockerfile" ]]; then
	echo Dockerfile not found
	exit 1
fi

# -

if [[ $OPTION_RE == 1 ]]; then
	{
		docker rm -f "$CONTAINER_NAME"
		docker rmi -f "$IMAGE_NAME"
	} &>/dev/null
	CONTAINER_STATE=""
else
	CONTAINER_STATE=$(docker inspect -f {{.State.Running}} "$CONTAINER_NAME" 2> /dev/null)
fi

if [[ "$CONTAINER_STATE" == "true" ]] ; then
	echo "Already running"
	exit 1
fi

docker build -t "$IMAGE_NAME" .

if [[ "$CONTAINER_STATE" == "false" ]]; then # Stopped
	echo "Restart"
	docker start -i "$CONTAINER_NAME"

else # Killed
	echo "Run"
	docker run -P -it -v "$SHARE_FOLDER" --name "$CONTAINER_NAME" "$IMAGE_NAME"

fi
