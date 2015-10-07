
// 'tournaments' is already defined at this point

var tournamentTableContainer = document.getElementById('tournaments');

var ws = new WebSocket('ws://' + window.location.host + '/tournaments');
ws.onmessage = function(e){
  var tournament = JSON.parse(e.data);
  var triple = [renderKind(tournament.tKind), tournament.tId, tournament.tMatchIds.length];
  if (tournaments.filter(function(x){
        return JSON.stringify(x) == JSON.stringify(triple)
      }).length == 0) {
    tournaments.unshift(triple);
    updateTournamentsTable();
  };
};

function updateTournamentsTable() {
  tournamentTableContainer.innerHTML =
      '<table>' + tournaments.map(renderTournament).join('') + '</table>';
};

function renderTournament([kind, id, size]) {
  return '<tr><td><a href="/tournaments/' + id + '">' + kind + ' #' + id + ' (' + size + ' matches)</a></td></tr>';
};

function renderKind(kind) {
  switch (kind.tag) {
    case 'RoundRobin':
      return 'RoundRobin';
    case 'Training':
      return 'Training ' + kind.contents;
  };
};