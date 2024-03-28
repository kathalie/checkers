:- module(evaluation, [
    evaluate_board/3
]).

:- use_module(mechanics).

static_material_value(w, 1).
static_material_value(b, 1).
static_material_value(wq, 5).
static_material_value(bq, 5).

% Evaluates Ch for a Player.
% Example: for a "white" Player "b" will result in -1, "wq" in 2.
% material_value(+Player, +Ch, -Eval).
material_value(Player, Ch, Eval) :-
    player(Player, Ch),
    static_material_value(Ch, Eval), !.

material_value(Player, Ch, Eval) :-
    \+ player(Player, Ch),
    static_material_value(Ch, AbsEval),
    Eval is -AbsEval, !.

% evaluate_board(+Player, +Board, -Score).
evaluate_board(Player, Board, 1000) :- 
    wins(Board, Player).

evaluate_board(Player, Board, -1000) :- 
    player(Player, Opponent),
    wins(Board, Opponent).

evaluate_board(Player, Board, Score) :-
    evaluate_board(Player, Board, 0, Score).

% evaluate_board(+Player, +Board, +PrevScore, -Score).
evaluate_board(_, [], Score, Score) :- !.

evaluate_board(Player, [cell(_, Ch, _, _) | RestOfBoard], CurrentScore, Score) :-
    material_value(Player, Ch, AddToScore),
    NewScore is CurrentScore + AddToScore,
    evaluate_board(Player, RestOfBoard, NewScore, Score).


% % positional_value(+Ch, +Row, -Value).
% positional_value(Ch, Row, Value) :-
%     Coef = 