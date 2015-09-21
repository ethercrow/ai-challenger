
Grid game
---------

It's turn based strategy game on a rectangular grid where players try to control
the map and eliminate each other by capturing tiles and managing resources.

The game is symmetrical in a sense that, unlike in chess, players make moves
simultaneously.

There's also no fog of war.

Player communication
--------------------

Players communicate with the judge using stdin and stdout. Every turn, the judge
sends world description to every bot and bots send back their orders. Both judge
and bot messages are terminated in a dot on a separate line.

Judge message format
------------------------

The first line is "Y <your integer id>".
The second line is "E <your energy budget>".

After that, the judge sends the world map. It's a rectangular grid of tiles,
empty tile is denoted by '-' and a captured tile is denoted by the id of its
owner.

Example judge message for some turn::

    Y 2
    E 1
    --1
    --1
    22-
    .

Example of reading a judge message in Python::

    def read_world():
        while True:
            l = sys.stdin.readline()
            if l == '.\n':
                return
            else:
                yield l.rstrip()

    id_line = sys.stdin.readline().rstrip().split(' ')
    assert id_line[0] == 'Y'
    my_id = id_line[1]
    log('My id: {0}'.format(my_id))

    energy_line = sys.stdin.readline().rstrip().split(' ')
    assert energy_line[0] == 'E'
    budget = int(energy_line[1])

    world = list(read_world())

Energy budget
~~~~~~~~~~~~~

Energy budget for player P is calculated as 1 plus one third of N (rounded up).

N is a number of empty tiles that have strictly more neighbor tiles captured by
player P than by the other player. For energy budget calculations neighbor
relationship is defined as sharing at least one side. That means that non-edge
tiles have 4 neighbors.

In these examples a center tile contributes to energy budget of player 1::

  -1-  -1-
  ---  --2
  ---  -1-

In these examples a center tile doesn't give energy to either player::

  1-2 112 ---
  --- 1-2 ---
  --- 121 ---

Order format
------------

Valid order looks like "C <x-coord> <y-coord>" and means "Capture the tile at
point (x, y)". Coordinates are 0-based integers, so that for example for a 2x2
world map valid coordinates are (0, 0), (0, 1), (1, 0) and (1, 1).

A player can only capture tiles that share at least one side with a tile already
captured by them. So for example if a player controls (0, 0), they can capture
(0, 1) or (1, 0) but not (1, 1).

If both players try to capture the same tile it remains in the previous state.

Number of capture orders must not be greater than energy budget available at
a given turn.

Example order message for some turn::

    C 1 2
    C 3 3
    .

Example of making such an order in Python::

    tiles_to_capture = [(1, 2), (3, 3)]
    for (x, y) in tiles_to_capture:
        print 'C {0} {1}'.format(x, y)
    print '.'

Tile suffocation
----------------

If a tile doesn't share a point with any empty tiles, that is it's completely
surrounded by tiles captured by either player, it becomes empty on the next
turn.

In the following example the center tile will be eliminated and become empty::

    122
    112
    221

Disqualification conditions
---------------------------

Player is disqualified during a match if...

 0. they crash
 1. they close stdin or stdout before a match ends
 2. they fail to produce an order in one second (wall time)
 3. they produce an invalid order

Victory conditions
------------------

Elimination
~~~~~~~~~~~

If a player doesn't control any tiles at some point, they lose by elimination.
Draw by elimination is also possible if for example the whole map is captured
and all the tiles suffocate the next turn.

Turn limit
~~~~~~~~~~

The player with the most controlled tiles after turn 200 wins. If players
control equal number of tiles, that match is declared a draw.

Replay format
-------------

Understanding replay format is not needed for implementing a successful bot,
but it can be useful for debugging or implementing a replay visualizer.

Replay format is designed to allow parsing in streaming fashion. Match state at
turn N can be parsed even if data about turn N+1 is not available yet.

Every dot-terminated message describes a single turn in a match::

    W
    <world map>
    O 1
    <orders by player 1>
    O 2
    <orders by player 2>
    .

Example ::

    W
    11--------
    1---------
    ----------
    ----------
    ----------
    ----------
    ----------
    ----------
    ---------2
    --------22
    O 1
    C 0 1
    C 1 0
    O 2
    C 8 9
    C 9 8
    .
