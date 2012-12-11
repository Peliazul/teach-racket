# Introduction

If you havn't heard of it, please take a minute to familiarize
yourself with the [Brainf*ck language](http://en.wikipedia.org/Brainfuck),
in short, it's a turing-complete language which looks like

       ,>++++++[<-------->-],[<+>-]<.

The file bf.rkt contains a interpreter which takes a Brainf*ck-Program
as a string and executes it.

The file bf-compiler.rkt contains a compiler from Brainf*ck to Racket,
which takes a Brainf*ck-Program as a string and returns a Racket-Program
(as a string).

# Expericences

This is so much fun. While writing complex Brainf*ck programs is certainly
hard (but can be used to keep the "wise guys" busy), seeing a program being 
transformed into another language, and being able to execute the 
transformed program really deepens you understanding of languages
and how computers work.

# Further reading...

The interpreter works byte by byte, and therefor has to scan the input
multiple times. It's easy to see that this is inefficient. To come up
with datastrutures and algorithms to do better, namely to write
a parser and have something like an AST, requires some knowledge
in computer science or a lot of creativity.

 