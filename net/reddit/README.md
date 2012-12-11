# Introduction

This is a simple reddit clone, letting students submit a text and
upvote a submitted text and presenting a list of the submitted texts
ordered by upvotes.

If you ever thought your students lack enthusiasm, just fire this up
in DrRacket, and let the students submit things like "Hannah Montana is
cute", "WoW players smell" or other controversial statements, 
and you'll see them upvoting like crazy. 

Honestly, I had lessons where groups of students clicked in the 5
digits, trying to put their submission on top.

# Experiments

The fun doesn't stop here. Using bot.rkt you can make the computer
automatically upvote. The point is that students who take the time to
figure out how to use bot.rkt achive, after a minute of
fiddelling, suddenly orders of magnitute more upvotes than those
students who just sit and click like maniacs.

After that, you can try to crash the server using the denial-of-service
function. Observe that while using the "upvote" functons merely slows the
server down, using threads in the "dos" function really leads 
to rejected connections. 