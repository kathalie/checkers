alphabeta(Board, 0, _, _, Score) :-
    evaluate_board(Board, Score).

%alphabeta(Board, Depth, Alpha, Beta, Score) :- 