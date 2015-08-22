#!/usr/bin/env sh

set -eu

curl -XPOST -H "Content-Type: application/json" -d"{\"botName\":\"greedy\", \"botCommunication\":{\"tag\":\"ExecutableBot\", \"contents\":\"$PWD/game-grid/greedy.py\"}}" http://127.0.0.1:8081/add-bot

curl -XPOST -H "Content-Type: application/json" -d"{\"botName\":\"randy\", \"botCommunication\":{\"tag\":\"ExecutableBot\", \"contents\":\"$PWD/game-grid/randy.py\"}}" http://127.0.0.1:8081/add-bot

curl -XPOST http://127.0.0.1:8081/launch-tournament

sleep 1
echo ''
echo ''

curl -H "Accept: application/json" http://127.0.0.1:8081/state | python -mjson.tool