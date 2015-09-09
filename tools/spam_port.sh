#!/bin/bash

ADDRESS="$1"
# PORTS="4242"
PORTS="4646 3939 4141 4242 4444 4343 5555 4545 4040 3030 3232 3434 3131 3333 5050 5151 5252 6666 7777 8888 9999 2222 4747 4848 4949"

function try_spam
{
	while echo lol | nc "$ADDRESS" "$1"; do
		printf .
	done
	# cat "/dev/urandom" | nc "$ADDRESS" "$1" && try_spam "$1"
}

while true; do
	for p in $PORTS; do
		printf "Spam $ADDRESS:$p..";
		try_spam "$p"
		printf "nop\n";
	done
	echo "Wait..."
	sleep 5
done
