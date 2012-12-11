# Introduction

This program simulates (and draws) one-dimensional cellular automata.
The cells are on a line, each having a left and right neighbour.  Each
cell is either alive (1) or dead (0).

In every generation, the state of a cell is defined by the 
state of the left and right neighbor and the own state in
the last generation.

For these three values, there are 8 combinations, from 000, 001 ... to
111. The rules for live and death can therefor be given as eight
numbers, each 0 or 1 (these 8 numbers can also be viewed as one
8-digit binary number).

The program first defines the rules, and then applies them to an
initial state (random). It is really instructive to work out simple
examples by hand. I chose this - over Game of Life - because the
generations can be drawn so easily on one sheet of paper.

# Tipps

The definitive guide to cellular automata of this kind is
"A New kind of science" by Stephen Wolfram. 

The "Aha"-moment for the students should be to watch how complex
structures (try rules 0 0 1 1 0 1 1 1 0) emerge from deterministic
application of the most basic rules. Some rules, like
(0 0 0 0 0 0 0 0) can directly be associated with the respective pattern,
while others generate compleatly surprising patterns.