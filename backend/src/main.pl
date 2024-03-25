:- module(main, [
    best_move/6
]).

:- use_module(alphabeta).

% best_board(+Depth, +Board, -BestBoard)
best_board(Depth, Board, BestBoard) :-
    minimax(white, Board, Depth, BestBoard).

% best_move(+Depth, +Board, -XFrom, -YFrom, -XTo, -YTo).
best_move(Depth, Board, XFrom, YFrom, XTo, YTo) :- 
    % write(Board),
    XFrom = Depth,
    YFrom = 1,
    XTo = 0,
    YTo = 0.
    %best_board(Depth, Board, BestBoard).

    %TODO: 
    % - assign EVERY checker with a unique key
    % - extend predicates with argument for a key of a checker that made a move
    % - implement `best_move` using a unique key of a moved checker.
