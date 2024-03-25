:- module(main, [
    best_move/6
]).

:- use_module(mechanics).
:- use_module(alphabeta).

% best_move(+Depth, +Board, -RFrom, -CFrom, -RTo, -CTo).
best_move(Depth, BoardBefore, RFrom, CFrom, RTo, CTo) :- 
    minimax(BoardBefore, Depth, white, BoardAfter),
    detect_move(BoardBefore, BoardAfter, RFrom, CFrom, RTo, CTo), !.


% detect_move(+BoardBefore, +BoardAfter, -RFrom, -CFrom, -RTo, -CTo).
detect_move(BoardBefore, BoardAfter, RFrom, CFrom, RTo, CTo) :-
    checker(BoardBefore, K, RFrom, CFrom, _),
    checker(BoardAfter, K, RTo, CTo, _),
    RFrom \= RTo,
    CFrom \= CTo.


/**
boards:board_q_move(Board), 
write("Board:"), nl, boards:print_board(Board), nl,
best_move(3, Board, RFrom, CFrom, RTo, CTo).
*/