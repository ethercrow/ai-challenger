
// tournamentId and botNames are already defined at this point

var ws = new WebSocket('ws://' + window.location.host + '/tournament/' + tournamentId);

var expecting = 'tournament';
var scoreTable = document.getElementById('scores');
var matches = [];

function updateScoreTable() {
  var botNamesAndScores = botNames.map(function(n){
    [w, d, l] = matches.reduce(
      function([pw, pd, pl], m){
        var winners = m.matchWinners.map(function(b){return b.botName});
        var names = m.matchBots.map(function(b){return b.botName});
        if (winners.length == 1 && winners[0] == n) {
          return [pw + 1, pd, pl];
        } else if (winners.length == 1 && names.indexOf(n) >= 0) {
          return [pw, pd, pl + 1];
        } else if (names.indexOf(n) >= 0) {
          return [pw, pd + 1, pl];
        };
        return [pw, pd, pl];
      },
      [0, 0, 0]);
    return [n, w, d, l];
  });
  botNamesAndScores.sort(function([n1, w1, d1, l1], [n2, w2, d2, l2]){
    return (w2 - w1) * 3 + (d2 - d1);
  });
  scoreTable.innerHTML =
    '<thead><tr><th>Bot</th><th>W</th><th>D</th><th>L</th></tr></thead><tbody>' +
    botNamesAndScores.map(
      function([n, w, d, l]){
        return '<tr><td>' + n + '</td><td>' + w + '</td><td>' + d + '</td><td>' + l + '</td></tr>';
      }).join('') + '</tbody>';
};

ws.onmessage = function(e){
  switch (expecting) {
    case 'match':
      var match = JSON.parse(e.data);
      matches.push(match);
      console.log('completed match ' + match.matchId);
      bots = match.matchBots;
      var cellId = bots[0].botName + '_vs_' + bots[1].botName;
      var otherCellId = bots[1].botName + '_vs_' + bots[0].botName;
      var matchCell = document.getElementById(cellId);
      var otherMatchCell = document.getElementById(otherCellId);
      var matchCellText = getMatchCellText(match);
      matchCell.innerHTML = '<a href="/matches/' + match.matchId + '">' + matchCellText + '</a>';
      otherMatchCell.innerHTML = '<a href="/matches/' + match.matchId + '">' + reverseCellText(matchCellText) + '</a>';
      updateScoreTable();
      break;
    case 'tournament':
      expecting = 'match';
      break;
  };
};

function getMatchCellText (match) {
  winText = function(){
    if (JSON.stringify(match.matchWinners) == JSON.stringify([match.matchBots[0]])) {
      return 'W';
    } else if (match.matchWinners.length == 1) {
      return 'L';
    };
    return 'D';
  };
  gameOverText = function(){
    switch (match.matchGameOverType.tag) {
      case 'Disqualification':
        return 'DQ';
      case 'Elimination':
        return 'E';
      case 'TurnLimit':
        return 'TL';
    };
  };
  return winText() + ':' + gameOverText();
};

function reverseCellText (t) {
  if (t[0] == 'W') {
    return 'L' + t.slice(1);
  } else if (t[0] == 'L') {
    return 'W' + t.slice(1);
  };
  return t;
};
