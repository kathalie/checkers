% Define initial board configuration
initial_board([
    cell(enemy, 1, 1), cell(enemy, 1, 3), cell(enemy, 1, 5), cell(enemy, 1, 7),
    cell(enemy, 2, 2), cell(enemy, 2, 4), cell(enemy, 2, 6), cell(enemy, 2, 8),
    cell(enemy, 3, 1), cell(enemy, 3, 3), cell(enemy, 3, 5), cell(enemy, 3, 7),
    cell(curr_player, 6, 2), cell(curr_player, 6, 4), cell(curr_player, 6, 6), cell(curr_player, 6, 8),
    cell(curr_player, 7, 1), cell(curr_player, 7, 3), cell(curr_player, 7, 5), cell(curr_player, 7, 7),
    cell(curr_player, 8, 2), cell(curr_player, 8, 4), cell(curr_player, 8, 6), cell(curr_player, 8, 8)
]).

board2([
    cell(enemy, 1, 1), cell(enemy, 1, 3), cell(enemy, 1, 5), cell(enemy, 1, 7),
    cell(enemy, 2, 2), cell(enemy, 2, 4), cell(enemy, 2, 6), cell(enemy, 2, 8),
    cell(enemy, 3, 1), cell(enemy, 3, 3), cell(enemy, 3, 5), cell(enemy, 3, 7),
    cell(curr_player, 6, 2), cell(curr_player, 6, 4), cell(curr_player, 6, 6), cell(curr_player, 6, 8),
    cell(curr_player, 7, 1), cell(curr_player, 7, 3), cell(curr_player, 7, 5), cell(curr_player, 7, 7)
]).

material_value(enemy, 1).
material_value(curr_player, -1).
material_value(enemy_king, 2).
material_value(curr_player_king, -2).

% % positional_value(+Checker, +Row, -Value)
% positional_value(Checker, Row, Value) :-
%     Coef = 



% evaluate_board(+Board, -Score)
evaluate_board(Board, Score) :-
    evaluate_board(Board, 0, Score).

evaluate_board([], Score, Score).

evaluate_board([cell(Checker, Row, Col) | RestOfBoard], CurrentScore, Score) :-
    material_value(Checker, AddToScore),
    NewScore is CurrentScore + AddToScore,
    evaluate_board(RestOfBoard, NewScore, Score).


