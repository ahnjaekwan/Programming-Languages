/* Name: Jaekwan Ahn

   UID: 604057669

   Others With Whom I Discussed Things: None

   Other Resources I Consulted: Discussion Slides, Piazza and Lecture Notes
   
*/


/* Problem 1: duplist(X,Y) */

duplist([],[]).
duplist([H|T1],[H,H|T2]) :- duplist(T1,T2).



/* Problem 2: subseq(X,Y) */

subseq([],[]).
subseq([],[H|T]).
subseq([H|T1],[H|T2]) :- subseq(T1,T2).
subseq([H1|T1],[H2|T2]) :- subseq([H1|T1],T2).



/* Problem 3: blocksworld */
/* pickup */
move(world([H|T],S2,S3,none), pickup(H,stack1), world(T,S2,S3,H)).
move(world(S1,[H|T],S3,none), pickup(H,stack2), world(S1,T,S3,H)).
move(world(S1,S2,[H|T],none), pickup(H,stack3), world(S1,S2,T,H)).
/* putdown */
move(world(S1,S2,S3,H), putdown(H,stack1), world([H|S1],S2,S3,none)).
move(world(S1,S2,S3,H), putdown(H,stack2), world(S1,[H|S2],S3,none)).
move(world(S1,S2,S3,H), putdown(H,stack3), world(S1,S2,[H|S3],none)).
/* main function (use above database) */
blocksworld(World, [], World).
blocksworld(Start, [M|Ms], Goal) :- move(Start,M,State), blocksworld(State, Ms, Goal).



/* Problem 4: verbalarithmetic */
/* word: transform list into number */
word([],0).
word([H|T],N) :- reverse([H|T],[H1|T1]), reverse(T1,T2), word(T2,N2), N is N2*10 + H1.
/* numset: valid number set (first letter should not be 0) */
numset1([]).
numset1([H|T]) :- member(H,[0,1,2,3,4,5,6,7,8,9]), numset1(T).
numset([H|T]) :- member(H,[1,2,3,4,5,6,7,8,9]), numset1(T).
/* main function (use above helper-functions) */
verbalarithmetic(L,L1,L2,L3) :- fd_all_different(L), numset(L1), numset(L2), numset(L3), word(L1,N1), word(L2,N2), word(L3,N3), N3 is N1 + N2.



