:- module(evaluation, [evaluate_board/2]).

material_value(b, 1).
material_value(w, -1).
material_value(bq, 2).
material_value(wq, -2).

% evaluate_board(+Board, -Score)
evaluate_board(Board, -1000) :-
    wins(Board, b).

evaluate_board(Board, 1000) :-
    wins(Board, w).

evaluate_board(Board, Score) :-
    evaluate_board(Board, 0, Score).

evaluate_board([], Score, Score).

evaluate_board([cell(Ch, Row, Col) | RestOfBoard], CurrentScore, Score) :-
    material_value(Ch, AddToScore),
    NewScore is CurrentScore + AddToScore,
    evaluate_board(RestOfBoard, NewScore, Score).


% % positional_value(+Ch, +Row, -Value)
% positional_value(Ch, Row, Value) :-
%     Coef = 