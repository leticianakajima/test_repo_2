% CPSC 312 - 2016 - Exam 1 solution with runnable code
% (either the question code, or answer code)
% Copyright D. Poole 2016. You may not redistribute this code.

% Question 1
:- dynamic w/0,m/0.
p :- v.
p :- q, r.
t :- n.
t :- m.
q :- s, t.
n.
v :- w.
r :- s.
s.

% Answer:
% (a)
% v is not a logical consequence of KB means v is not true in all models of KB.
% To show why, you should provide a model in which v is false.
% It is not true in any model in which v and w are both false.
% (E.g., the model where v and w are false and all other atoms are true).
% It is also not true in the minimal model (which you should give if
% that is the model you want to use in which v is false.

% (b)
%    Answer Clause      | Clause resolved
%    yes :- p.          |
%    yes :- q,r.        | p :- q,r.
%    yes :- s,t,r.      | q :- s,t.
%    yes :- t,r.        | s.
%    yes :- n,r.        | t :- n.
%    yes :- r.          | n.
%    yes :- s.          | r :- s.
%    yes :- .           | s.


% Question 2(a)
:- dynamic bird/0, emu/0, penguin/0, on_plane/0, plane_broken/0.
flies :- bird, \+ abfly.
flies :- on_plane, \+ plane_broken.
abfly :- emu.
abfly :- penguin.
bird :- emu.
bird :- penguin.
bird.

% Question 2(b)
:- dynamic f/0.
a :- b.
a :- \+ c.
b :- \+ d.
b :- c.
d.
c :- f.
e :- \+ f.

% Answer: {d, \+f, \+c, a, \+b, e}


% Question 3
next_hour(am(12,X),am(1,X)).
next_hour(am(N,X),am(N1,X)) :- N>0, N<11, N1 is N+1.
next_hour(am(11,X),pm(12,X)).
next_hour(pm(12,X),pm(1,X)).
next_hour(pm(N,X),pm(N1,X)) :- N>0, N<11, N1 is N+1.

%?- next_hour(am(10,23),T).
%?- next_hour(am(11,23),T1), next_hour(T1,T2).

% Question 4
del1(E,[E|T],T).
del1(E,[H|T],[H|R]) :- del1(E,T,R).
%?- del1(a,[a,v,a,t,a,r],R).
%?- del1(2,L,[a,b,c,a]).
