willOpponentWin(X, Y) :- X + Y + 3 > 12; X + Y + 2 > 12; X + Y + 4 > 12.

opponentWinInAnyMove(X,Y) :- willOpponentWin(X+3, Y), willOpponentWin(X, Y + 2), willOpponentWin(X, Y + 4).

go :- assert(winner(1,0)), assert(winner(2,0)), not(makeMove(3, 2, 0)), printResult.

makeMove(X, Y, Steps) :- opponentWinInAnyMove(X,Y), !, defineWinner(Steps), printPoint(X,Y).

makeMove(X, Y, Steps) :- (not(willOpponentWin(X + 3, Y)),!, makeMove(X + 3, Y, Steps + 1)), (not(willOpponentWin(X, Y + 2)), !, makeMove(X, Y + 2, Steps + 1)),
 (not(willOpponentWin(X, Y + 4)), !,makeMove(X, Y + 4, Steps + 1)).

defineWinner(Steps) :- Winner is (Steps mod 2), winner(Winner,X), R is (X + 1), retract(winner(Winner,X)), assert(winner(1,R)).

printResult :- winner(1,P1), winner(2,P2), (P1 > P2 -> write('Player 1 will be winner in logical game'); write('Player 2 will be winner in logical game')).

printPoint(X,Y):- write('Move to win: '), write(X), write(','), write(Y), nl.