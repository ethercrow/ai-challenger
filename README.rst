
.. image:: https://travis-ci.org/ethercrow/ai-challenger.svg?branch=master
    :target: https://travis-ci.org/ethercrow/ai-challenger

AI Challenger is a platform for hosting bot competitions.

Prerequisites
-------------

 - npm for building frontend
 - stack for building backend

Building
----------

::

  stack setup # if you don't have ghc-7.10.2 installed
  make release # this has a side effect of producing a tarball with a binary

Running
-------

::

  ./ai-challenger-grid/ai-challenger-grid --port 8081 --address 127.0.0.1
  ./tourney_grid.sh # launch a tournament between bundled bots

Debugging your bot
------------------

::

  ./launch_training_tournament.py 10x10 mybot.py # launch a training session of your bot vs all bundled ones

Server API doc...
------------------

...is served by server itself: http://localhost:8081/help

Open it in browser for html version or get with
```curl -H 'Accept: text/plain'``` for markdown version

Running tests
-------------

::

  stack test

Playing remotely
----------------

::

  # register your bot for playing over websocket
  ai-challenger-remote-play --host 127.0.0.1 --port 8081 --name mybot --executable mybot.py
