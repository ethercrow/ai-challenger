
Player communication
--------------------

Players communicate with the judge using stdin and stdout.
Every turn, the judge sends world description to every bot and bots send back their orders.
Both judge and bot messages are terminated by a dot on a separate line.

World description format
------------------------

In this game there's no interesting world state that changes from turn to turn.
The only thing that judge is providing to a bot is their id.

Example judge message for some turn::

    Y 2
    E 3
    .

Example of reading world state::

    id_line = sys.stdin.readline().rstrip().split(' ')
    assert id_line[0] == 'Y'
    my_id = int(id_line[1])
    # my_id is now 42

Order format
------------

Valid orders are 'R', 'P' and 'S' for rock, paper and scissors respectively.
There must be exactly one order for every turn. Producing zero or more than one
order in any turn results in a disqualification and a lost match.

Example order message for some turn::

    R
    .

Example of making such an order in Python::

    # play rock
    print 'R'
    print '.'

Disqualification conditions
---------------------------

Player is disqualified during a match if...

 0. they crash
 1. they close stdin or stdout before a match ends
 2. they fail to produce an order in one second (wall time)
 3. they produce an invalid order

Victory conditions
------------------

The player with the largest score after the last turn wins.
If both players have the same score the match is declared a draw.

Replay format
-------------

Understanding replay format is not needed for implementing a successful bot,
but it can be useful for debugging or implementing a replay visualizer.

Replay format is designed to allow parsing in streaming fashion. Match state at turn N can be parsed even if data about turn N+1 is not available yet.

Every non-empty line describes a single turn in a match::

  <Player 1 score> <Player 2 score> <Player 1 order> <Player 2 order>

Example excerpt from a replay::

    4 8 P S
    4 9 P S
    4 9 S S
    4 9 S S
    5 9 R S
    6 9 R S