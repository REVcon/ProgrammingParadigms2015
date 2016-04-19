male(bill).
male(joe).
male(paul).
male(jim).
male(bob).
female(tammy).
female(sue).
female(ann).
female(mary).
parent(bill, joe).
parent(joe, tammy).
parent(sue, ann).
parent(bill, ann).
parent(sue, joe).
parent(ann, bob).
parent(jim, bob).
parent(paul, jim).
parent(mary, jim).
child(X,Y):-parent(Y,X).
different(X,Y):-X\=Y.
sister(X,Y):-parent(Z,X), parent(Z,Y), female(X), different(X,Y).
aunt(X,Y):-parent(Z,Y), sister(X,Z).
gmother(X,Y):-parent(Z,Y), parent(X,Z), female(X).
gson(X,Y):-parent(Z,X), parent(Y,Z), male(X).
cousin(X,Y):-parent(Z,X),parent(L,Z), parent(K,Y), parent(L,K), different(Z,K).

likes(ellen,reading).
likes(john,computers).
likes(john,badminton).
likes(john,photo).
likes(john,reading).
likes(leonard,badmintoon).
likes(eric,swimming).
likes(eric,reading).
likes(eric,chess).
likes(paul,swimming).

greater(A,B):-A@>B.
hasFourHobbies(P):-likes(P,A),likes(P,B),likes(P,C),likes(P,D), greater(A,B), greater(B,C), greater(C,D).