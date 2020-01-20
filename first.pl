% CPSC 312 2018
% Some simple Prolog examples. In public domain.

% To load in Prolog, at the ?- prompt:
% [first].
  
% append([a1,a2,..an], [b1,..,bm], [a1,..,an,b1,..,bm])
append([], L2, L2).
append([H|R], L2, [H|L3]) :-
    append(R,L2,L3).

% Some queries:
% append([1,2,3], [7,8,9], R).
% append([1,2], X, [1,2,3,4,5]).
% append([6,7], X, [1,2,3,4,5]).
% append(X,Y,[1,2,3,4,5]).
% append(X,[3|Y],[1,2,3,4,5]).
% append(X,[3|Y],[1,2,3,4,5,4,3,2,1]).

% del1(E,L,R) is true if R is list L with one instance of E removed
del1(X,[X|Y],Y).
del1(X,[H|T],[H|Z]) :-
    del1(X,T,Z).

% del1(a,[a,v,a,t,a,r],A).
% del1(b,[a,v,a,t,a,r],A).
