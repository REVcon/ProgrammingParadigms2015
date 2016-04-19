vertical(4).
vertical(6).
vertical(7).
vertical(8).
vertical(9).
horiz(1).
horiz(2).
horiz(3).
horiz(5).
seg(1, point(1,11), point(14,11)).
seg(2, point(2,4), point(13,4)).
seg(3, point(2, 2), point(9, 2)).
seg(4, point(3,1), point(3,10)).
seg(5, point(7,10), point(13,10)).
seg(6, point(8,0), point(8,13)).
seg(7, point(10,3), point(10,12)).
seg(8, point(11,3), point(11,13)).
seg(9, point(12,2), point(12,12)).
len(point(X,Y), point(A,B), Z) :- Z is round(sqrt((X-A)*(X-A) + (Y-B)*(Y-B))).
different(X,Y):-X\=Y.
iscrossed(N,M) :- horiz(M), vertical(N), seg(M, point(A,Y), point(B,Y)), seg(N, point(X,C), point(X,D)), different(N,M), X >= A, X =< B, Y >= C, Y =< D.
cross(N,M, point(X,Y), NL,ML) :- iscrossed(N,M), seg(M, point(A,Y), point(B,Y)), seg(N, point(X,C), point(X,D)), len(point(A,Y), point(B,Y), ML), len(point(X,C), point(X,D), NL).
perimetr(A,B,C,D,P,S):- cross(A,B, point(X,Y),_,_),
    cross(A,D, point(X,Y1),_,_),
    cross(C,B,point(X1,Y),_,_),
    iscrossed(C,D),
    len(point(X,Y), point(X,Y1),V), 
    len(point(X,Y), point(X1,Y), H),
    (P is 2 * (V + H)), (S is H * V), different(A,C), different(B,D), A < C, B < D.