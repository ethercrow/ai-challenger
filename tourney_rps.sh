#!/usr/bin/env sh

set -eu

curl -XPOST -H "Content-Type: application/json" -d"{\"botName\":\"pepper\", \"botCommunication\":{\"tag\":\"ExecutableBot\", \"contents\":\"$PWD/game-rps/paper.py\"}}" http://127.0.0.1:8081/add-bot

curl -XPOST -H "Content-Type: application/json" -d"{\"botName\":\"scarlett\", \"botCommunication\":{\"tag\":\"ExecutableBot\", \"contents\":\"$PWD/game-rps/scissors.py\"}}" http://127.0.0.1:8081/add-bot

curl -XPOST -H "Content-Type: application/json" -d"{\"botName\":\"rocky\", \"botCommunication\":{\"tag\":\"ExecutableBot\", \"contents\":\"$PWD/game-rps/rock.py\"}}" http://127.0.0.1:8081/add-bot

curl -XPOST -H "Content-Type: application/json" -d"{\"botName\":\"randy\", \"botCommunication\":{\"tag\":\"ExecutableBot\", \"contents\":\"$PWD/game-rps/randy.py\"}}" http://127.0.0.1:8081/add-bot

curl -XPOST http://127.0.0.1:8081/launch-round-robin-tournament

sleep 1
echo ''
echo ''

curl -H "Accept: application/json" http://127.0.0.1:8081/state | python -mjson.tool