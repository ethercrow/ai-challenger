#!/usr/bin/env sh

set -eu

HOST=127.0.0.1

curl -XPOST -H "Content-Type: application/json" -d"{\"botName\":\"greedy\", \"botCommunication\":{\"tag\":\"ExecutableBot\", \"contents\":\"$PWD/game-grid/greedy.py\"}}" http://$HOST:8081/add-bot

curl -XPOST -H "Content-Type: application/json" -d"{\"botName\":\"randy\", \"botCommunication\":{\"tag\":\"ExecutableBot\", \"contents\":\"$PWD/game-grid/randy.py\"}}" http://$HOST:8081/add-bot

curl -XPOST http://$HOST:8081/launch-round-robin-tournament

sleep 1
echo ''
echo ''

curl -H "Accept: application/json" http://$HOST:8081/state | python -mjson.tool