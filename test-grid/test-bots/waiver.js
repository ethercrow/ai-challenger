#!/usr/bin/env node

function bot() {
  process.stdin.resume();
  process.stdin.setEncoding('utf8');
  var util = require('util');
  var field;
  var myId;
  var energy;

  function cleanup() {myId = 0; energy = 0; field = []}

  function takeTurn() {
    var w = [];
    var front = 1;

    function buildFront() {
      var succeed = 0;
      front += 1;
      for (var i = 0; i < w.length; i++)
        for (var j = 0; j < w[i].length; j++) {
            if (w[i][j] != 0) continue;
            if ((i > 0 && w[i-1][j] == front -1) ||
                (j > 0 && w[i][j-1] == front -1) ||
                (i < w.length - 1 && w[i+1][j] == front - 1) ||
                (j < w[i].length -1 && w[i][j+1] == front - 1)) {
                  w[i][j] = front;
                  succeed = 1;
                }
        }
      return succeed;
    }
    function tryToTakeThem() {
      for (var i = 0; i < w.length; i++)
        for (var j = 0; j < w[i].length; j++) {
            if (w[i][j]!=front || energy <= 0) continue;
            if ((i > 0 && field[i-1][j] == myId) ||
                (j > 0 && field[i][j-1] == myId) ||
                ((i < w.length - 1) && field[i+1][j] == myId) ||
                ((j < w[i].length -1) && field[i][j+1] == myId)) {
                  process.stdout.write("C " + j + " " + i + "\n");
                  energy -= 1;
                }
        }
    }

    for (var i = 0; i < field.length; i++) {
      var ts = [];
      for (var j = 0; j < field[i].length; j++)
        ts.push((field[i][j] != "-" && field[i][j] != myId) ? front : 0);
      w.push(ts);
    }
    do {
      tryToTakeThem();
    } while (energy > 0 && buildFront());
    process.stdout.write(".\n");
    cleanup();
  }

  cleanup();

  process.stdin.on('data', function (text) {
    var lines = text.split("\n");
    for (i in lines) { line = lines[i];
      var parts = line.trim().split(" ");
      if (parts[0] == "Y") myId = parts[1];
      else if (parts[0] == "E") energy = parseInt(parts[1]);
      else if (parts[0] == ".") takeTurn();
      else if (line.trim()) field.push(line);
    }
  });
}

bot();