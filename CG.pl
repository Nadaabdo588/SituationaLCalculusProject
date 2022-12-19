/*
grid(3,3).
agent_loc(0,1).
ships_loc([[2,2],[1,2]]).
station(1,1).
capacity(1).
*/

:- include("KB.pl").

remover( _, [], []).
remover( R, [R|T], T2) :- remover( R, T, T2).
remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).


shipExists(I,J):- ships_loc(X), shipExists(I,J,X).
shipExists(_, _, []):- false.
shipExists(I, J, [H|T]):- H == [I,J]; shipExists(I, J, T).

/*
x,y - agent's loc, c- curr capacity, h- curr ships
*/
/*agent_loc(X,Y), ships_loc(H), agentState(X,Y,0,H,s0).*/
agentState(0,1,0,[[2,2],[1,2]],s0).
agentState(X,Y,C,H,result(A,S)):-
	(agentState(X,Y,C2,H,S), A=drop, station(X,Y),  C2>0, C = 0);
	(agentState(X,Y,C2,H2,S), A=pickup, shipExists(X,Y), capacity(Max),
	C2<Max, C is C2+1, remover([X,Y], H2, H));
	(agentState(X2,Y,C,H,S), A=up, X2>0, X is X2-1);
	(agentState(X2,Y,C,H,S), A=down, grid(W,_), X2<W-1, X is X2+1);
	(agentState(X,Y2,C,H,S), A=left, Y2>0, Y is Y2-1);
	(agentState(X,Y2,C,H,S), A=right, grid(_,W), Y2<W-1, Y is Y2+1).



goal(S):- agentState(_,_,C,H,S), C == 0, H == [].


ids(S,L):-
	(call_with_depth_limit(goal(S),L,R), number(R));
	(call_with_depth_limit(goal(S),L,R), R=depth_limit_exceeded,
	L1 is L+1, ids(S,L1)).

/*
our goal, we picked up from all ships
cg boat is empty (cap =0)
*/
