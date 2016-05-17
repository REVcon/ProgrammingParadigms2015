trim([X|L],LR) :- append(LR,[_], L).
first(X,[X|_]).
last(X, L):- append(_, [X], L).

first_last(L1,L2):-trim(L1,L), trim(L2,L), first(H,L1), first(T,L2), last(T,L1), last(H, L2).

total([H|T],P):-total(T,P1),P is H*P1.
total([],1).

place(S,L,R):-placeLR(S,[],L,R).
placeLR(S,LL,[H|LR],R):-(append(LL, [S], LS), append(LS,[H|LR],R)); (append(LL,[H],LLN), placeLR(S,LLN,LR,R)).
placeLR(S,LL,[],R):-append(LL,[S],R).

double([H|T],LL):- append([H,H], LLT, LL), double(T, LLT).
double([],[]).

oddeven(X, O, E):-Val is (X mod 2), (Val == 1 -> O = [X], E = []; E = [X], O = []).
split([H|T], L2, L3):- split(T,Lt2,Lt3), oddeven(H,Odd,Even), append(Lt2,Even,L2),append(Lt3,Odd,L3).
split([], _, _).

repeat_back3(L1, L2):- trim(L1, L), first(H,L1), last(T,L1), append([T,T,T], L, Lt), append(Lt, [H,H,H], L2).

combi([H1|T1],[H2|T2], L3):-append([H1,H2],LT,L3), combi(T1,T2,LT).
combi([H1],[H2|T2], L3):-append([H1,H2],T2,L3).
combi([H1|T1],[H2], L3):-append([H1,H2],T1,L3).
combi([H1],[H2], [H1,H2]).