
.. image:: https://travis-ci.org/ethercrow/ai-challenger.svg?branch=master
    :target: https://travis-ci.org/ethercrow/ai-challenger

AI Challenger is a platform for hosting bot competitions.

Installing
----------

::

  stack setup # if you don't have ghc-7.10.2 installed
  stack install

Running
-------

For Rock-Paper-Scissors game::

  ai-challenger-rps

For Grid game::

  ai-challenger-grid

Server API doc...
------------------

...is served by server itself: http://localhost:8081/help

Open it in browser for html version or get with
```curl -H 'Accept: text/plain'``` for markdown version

Running tests
-------------

::

  stack test