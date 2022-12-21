/*
our goal, we picked up from all ships
cg boat is empty (cap =0)
*/

:- include("KB.pl").

/* remove element from a list */
remover( _, [], []).
remover( R, [R|T], T2) :- remover( R, T, T2).
remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).

/* check id x,y is a ship location*/
shipExists(I,J):- ships_loc(X), shipExists(I, J, X).
shipExists(_, _, []):- false.
shipExists(I, J, [H|T]):- H == [I,J]; shipExists(I, J, T).

/*x,y - agent's loc, c- curr capacity, h- curr ships list*/
/* define the initial state*/
agentState(X, Y, C, Ships, S):-
	agent_loc(X, Y), ships_loc(Ships), S=s0, C=0.

agentState(X, Y, C, Ships, result(A, S)):-
	(agentState(X, Y, C2, Ships, S), A=drop, station(X, Y),  C2>0, C = 0);
	(agentState(X, Y, C2, Ships2, S), A=pickup, shipExists(X, Y), capacity(Max),
	C2<Max, C is C2+1, remover([X, Y], Ships2, Ships));
	(agentState(X2, Y, C, Ships, S), A=up, X2>0, X is X2-1);
	(agentState(X2, Y, C, Ships, S), A=down, grid(W, _), X2<W-1, X is X2+1);
	(agentState(X, Y2, C, Ships, S), A=left, Y2>0, Y is Y2-1);
	(agentState(X, Y2, C, Ships, S), A=right, grid(_, W), Y2<W-1, Y is Y2+1).

goal(S):- agentState(_, _, C, Ships, S), C == 0, Ships == [].


ids(S,L):-
	(call_with_depth_limit(goal(S),L,R), number(R));
	(call_with_depth_limit(goal(S),L,R), R=depth_limit_exceeded,
	L1 is L+1, ids(S,L1)).
	
