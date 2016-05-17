/*memberT(X,[X|_]):-!.
memberT(X,[_|L]) :- member(X,L).*/
/*4.1 зеленое отсечение*/

fib(X, _):-X < 1,write('error'),!.
fib(1, 1):-!.
fib(2, 1):-!.
fib(N,R):-(N1 is (N - 1)), fib(N1, R1),(N2 is (N - 2)), fib(N2,R2),  R is R1 + R2.

sort_3:-write('list?'),nl, read(L), nl, shellsort(L,R), 
    write('answer'), write(R),nl. 

shellsort(L,L).
/*shellsort([], []). 
shellsort([X|L], M) :-shellsort(L, N), shellsortx(X, N, M).
shellsortx(X, [A|L], [A|M]) :-order(A, X), !, shellsortx(X, L, M). 
shellsortx(X, L, [X|L]).*/

sort_4_1:-write('list?'),nl, read(L),nl, sortn(L,R), 
    write('answer'), write(R),nl.
   
sortn(L1, L2) :-permutation(L1, L2), sorted(L2),!. 
permutation(L, [H|T]) :-append(V, [H|U], L),append(V, U, W), 
    permutation(W, T). 
permutation([], []).
sorted([_]).
sorted([X,Y|T]) :-order(X,Y), sorted([Y|T]). 
order(X, Y) :- X =< Y. 

sort_4_2 :-write('list?'),nl, read(L),nl, busort(L,R), 
    write('answer'), write(R),nl. 

busort(L, S) :-swap(L, M), !, busort(M, S). 
busort(L, L) :-!. 
swap([X, Y|R], [Y, X|R]) :-order(Y, X). 
swap([X|R], [X|R1]) :-swap(R, R1).  

sort_4_3:-write('list?'),nl, read(L),nl, insort(L,R), 
    write('answer'), write(R),nl. 

insort([], []). 
insort([X|L], M) :-insort(L, N), insortx(X, N, M).
insortx(X, [A|L], [A|M]) :-order(A, X), !, insortx(X, L, M). 
insortx(X, L, [X|L]).

sort_4_4:-write('list?'),nl, read(L),nl, qsort(L,R), 
    write('answer'), write(R),nl.

qsort([], []).
qsort([H|Tail], S) :-split(H, Tail, Small, Big), qsort(Small,Small1),
    qsort(Big, Big1), append(Small1, [H|Big1], S),!.
split(H, [A|Tail], [A|Small], Big) :-order(A, H), !, split(H,Tail, Small, Big).
split(H, [A|Tail], Small, [A|Big]) :-split(H, Tail, Small, Big).
split(_, [], [], []).


common(L1, L2, L3) :- append(L1, L2, S), deldupl(S, R), busort(R, L3).
deldupl([], []).
deldupl([H | T1], T2):-member(H, T1), !, deldupl(T1, T2).
deldupl([H | T1], [H | T2]):-deldupl(T1, T2).

most_oft(L,X):-msort(L,R), often(R,X).
often(L,L):-deldupl(L,X), length(X,R), length(L,T), R == T,!.
often(L,R):-deldupl(L,LD), dellist(L,LD,X), often(X,R).
dellist(X,[],X).
dellist([H|X],[H|D],R):-dellist(X,D,R),!.
dellist([H|X],D,[H|R]):-dellist(X,D,R),!.