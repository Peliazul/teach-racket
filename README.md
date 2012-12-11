# Introduction

This repo contains a loose collection of short Racket programs which
were used to teach programming to high school students.

Some are pretty standard, like converting numbers to binary, some are
fun (hangman, rock-paper-scissors), some try shamelessly to gain
attention (reddit clone, facemash clone, spamming) and some are more
advanced (multiplayer sokoban).

All were developed for, or as part of, lessons in computer science.
That mans, above all, they aim for simplicity: The students should easily 
grasp what the program is about, what it does, and how it does it.

Some of the programs presented here were originally given as tasks to
the students, yet all the programs are now fully functioning. While
that might spoil some fun, these programs provide examples for simple
but useful code, and a good starting point for many, many experiments.

# Where to start?

- net/reddit is a simple but not trivial reddit clone, along
  with code for cheating and  deniel-of-service attacks. Fun and
  interesting for students, yet easy to understand.
- games/sokoban is an advanced multiplayer sokoban game. 
  Very much fun to play, but not so easy to hack on.
- net/facemash: The origin of facebook (photos needed). Very
  much fun for students, medium level to hack on.
- games/rock: multiplayer, web-based Rock-paper-scissors.
  Much fun to play, very instructive for web
- automata/cellular is a nicely integrated simulation
  of one-dimensional cellular automata which outputs graphics
  directly in in interaction window and allows for some exploration,
  but perhaps for interested students only.

# A note on code quality

These programs contain some repetetive code, use antiquated libraries
or outright ignore best practice algorithms. In my opinion, none of
that matters.  The only way to learn is to do. Everything that
encourages students to try out, to play with or to extend the code is
right, everything that makes students shy away from doing so,
remaining inactive, is wrong.

# Prerequisites

There is no strict curriculum here, like "first we learn X, then Y".
It's just no use teaching advanced concepts without examples, and if
you have eamples to play with, students will figure out the concepts
automtically.

Having said that, the examples in this collection emphasize

- Definition of side-effect free functions
- Working with lists, often using recursion, map, apply
- Some use default parameters to avoid function pairs where
  the first function just calls the second with an empty accumulator
- Ad-hoc functions with lambda

# On comments

The programs contain only few comments. In part to let the students
themselves figure out how the program works, in part because
the students tend not to use comments in their own programs.

Honestly, I'm not sure if they should. Programs with comments seem to
use advanced techniques, like a text in a foreign language written
largely with help of a dictionary, which has to be annotated with
translations of all the unfamiliar words.  Compare that to a less
elegant, less eloquent text/program, which the students create using
the techniques/words they know by heart.

# A note for hacker turned teachers

You're going to fail. Not because you lack social skills, or because
students make fun of you, but because you care about your subject, and
you experienced the profound joy of creating and understanding
something after many many ours of trying.

Guess what: the majority of students has neither time nor interest to
invest much energy into the majority of subjects. And if you're going
after Aha-moments you're gonna miss out on the silent majority of
somewhat willing but uninterested students, and your class is gonna be
a mess.
