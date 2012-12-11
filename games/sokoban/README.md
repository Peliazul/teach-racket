# Introduction

This is the game of sokoban. For the rules check wikipedia.  Except,
this is a multiplayer game. This adds some fun and some twists to the
original sokoban game. For example, some constallations can only be
solved by two players working together.

Since there are no means of communication between the players built into
the user interface, you have to move your avatar and hope someone else
realises you need help... 

For the server, you need a game file in the widley used
sokoban-format (see examples/wikipedia). The only difference is that
there can be several players.

After starting the server, it waits for enough clients, and then
starts the game.

The client sokoban-client2.rkt needs png-pictures for
boxes, walls, empties etc., examples are provided. 
Just call "connect" with the correct IP address and port.


* Known Bugs

The server relies on the fact that the clients stay connected. 
If a client first connects, and then disappears the game halts.

This gets worse since the client, after sending a request 
to the server to start the game, just waits. "Don't hang up"!

* Usage in class

To understand and tweak the server is a rather advanced task, although
it is less than 300 lines and uses no involved techniques.

To understand the client however, and watch the messages via
WireShark, change the graphics etc is so much fun.

Notice how this is different from web apps, since it uses one TCP
connection per client, and the connection remains open, so no
identification is needed when sending a request/move.

* Note

Writing this program, and watching students playing it, has been one
of my most joyful Racket-related experiences. Especially since one of
my first Pascal programs 20 years earlier has been a single-player
text-based sokoban. 

Happy Hacking!