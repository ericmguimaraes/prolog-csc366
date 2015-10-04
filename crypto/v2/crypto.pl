:- consult('../gv.pl').
:- consult('../combosets.pl').

crypto(N1,N2,Goal, ex(N1, +, N2)):- Goal is ( N1 + N2).
crypto(N1,N2,Goal, ex(N1, *, N2)):- Goal is ( N1 * N2).
crypto(N1,N2,Goal, ex(N1,-, N2)) :- Goal is ( N1 - N2).
crypto(N1,N2,Goal, ex(N2,-, N1)) :- Goal is ( N2 - N1).
crypto(N1,N2,Goal, ex(N1, /, N2)) :- N2 > 0 , Goal is ( N1 / N2).
crypto(N1,N2,Goal, ex(N2, /, N1)) :- N1 > 0 , Goal is ( N2 / N1).

crypto(N1,N2,N3,G,Expr):-
	combos(set(N1,N2,N3), combo(A, B) , extra(C)),
	crypto(A, B, SG,SGE),
	crypto(C, SG, G, UGE),
	substitute(SGE,SG,UGE,Expr).
crypto(N1,N2,N3,N4,G,Expr):-
	combos(set(N1,N2,N3,N4), combo(A, B), extra(C, D)),
	crypto(A, B, SG, SGE),
	crypto(C,D,SG,G,UGE),
	substitute(SGE,SG,UGE,Expr).

substitute(New, Old, ex(Old, O, Z), ex(New, O ,Z )).
substitute(New, Old, ex(X, O, Old), ex(X, O,New )).
substitute(New, Old, ex(X, O, Z), ex(Q, O ,Z )):- 
	substitute(New, Old, X, Q).
substitute(New, Old, ex(X, O, Z), ex(X, O ,Q )):-
	substitute(New, Old,Z,Q).

