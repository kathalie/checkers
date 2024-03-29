:- module(minimax, [
    best_board/4
]).

:- use_module(mechanics).
:- use_module(evaluation).

/** <examples>
% board_eating, white, 1

?- boards:board_eating(Board), 
write("Board:"), nl, boards:print_board(Board), nl,
best_board(Board, 1, white, NewBoard), 
write("New Board: "), nl, boards:print_board(NewBoard), nl, 
!.


% board_eating, black, 1

?- boards:board_eating(Board), 
write("Board:"), nl, boards:print_board(Board), nl,
best_board(Board, 1, black, NewBoard), 
write("New Board: "), nl, boards:print_board(NewBoard), nl, 
!.


% board_eating, black, 3

?- boards:board_eating(Board), 
write("Board:"), nl, boards:print_board(Board), nl,
best_board(Board, 1, black, NewBoard), 
write("New Board: "), nl, boards:print_board(NewBoard), nl, 
!.


% board_cannot_eat, black, 3

?- boards:board_cannot_eat(Board), 
write("Board:"), nl, boards:print_board(Board), nl,
best_board(Board, 1, black, NewBoard), 
write("New Board: "), nl, boards:print_board(NewBoard), nl, 
!.

% board_cannot_eat, white, 3

?- boards:board_cannot_eat(Board), 
write("Board:"), nl, boards:print_board(Board), nl,
best_board(Board, 1, white, NewBoard), 
write("New Board: "), nl, boards:print_board(NewBoard), nl, 
!.

Board = [cell(15, w, 7, 0),
       cell(31, w, 7, 2),
       cell(47, w, 7, 4),
       cell(63, w, 7, 6),
       cell(54, w, 3, 6),
       cell(22, wq, 1, 6),
       cell(16, b, 1, 0),
       cell(66, b, 6, 7),
       cell(32, b, 1, 4)],
boards:print_board(Board), nl,
best_board(Board, 5, white, BestBoard),
boards:print_board(BestBoard), nl.

Board = [cell(70, b, 1, 7),
       cell(47, b, 3, 7),
       cell(54, b, 3, 5),
       cell(63, b, 2, 4),
       cell(15, b, 3, 3),
       cell(22, b, 3, 1),
       cell(64, w, 5, 7)],
boards:print_board(Board), nl,
best_board(Board, 5, white, BestBoard),
boards:print_board(BestBoard), nl.
*/



% function minimax(position, depth, alpha, beta, maximizing)
% 	if depth == 0 or game over in position
% 		return static evaluation of position
% 	if maximizing
% 		maxEval = -infinity
% 		for each child of position
% 			eval = minimax(child, depth - 1, alpha, beta false)
% 			maxEval = max(maxEval, eval)
% 			alpha = max(alpha, eval)
% 			if beta <= alpha
% 				break
% 		return maxEval
% 	else
% 		minEval = +infinity
% 		for each child of position
% 			eval = minimax(child, depth - 1, alpha, beta true)
% 			minEval = min(minEval, eval)
% 			beta = min(beta, eval)
% 			if beta <= alpha
% 				break
% 		return minEval


maximizing(white).
minimizing(black).

% best_board(+Board, +Depth, +Player, -BestBoard).
best_board(Board, Depth, Player, BestBoard) :- 
    findall(
        NextBoard, 
        possible_actions(Player, Board, NextBoard),
        NextBoards
    ),
    member(_,NextBoards), % Check that there are available moves! 
    best_board(NextBoards, Depth, -9999, 9999, Player, _, BestBoard), !.

%best_board(+Boards, +Depth, +Alpha, +Beta, +Player, +CurrentBestBoard, -BestBoard).
best_board([], _, _, _, _, BestBoard, BestBoard).

best_board([NextBoard | Rest], Depth, Alpha, Beta, Player, _, BestBoard) :-
    minimax(NextBoard, Depth, Alpha, Beta, Player, Eval),
    Eval > Alpha,
    best_board(Rest, Depth, Eval, Beta, Player, NextBoard, BestBoard).

best_board([NextBoard | Rest], Depth, Alpha, Beta, Player, CurrentBestBoard, BestBoard) :-
    minimax(NextBoard, Depth, Alpha, Beta, Player, Eval),
    Eval =< Alpha,
    best_board(Rest, Depth, Alpha, Beta, Player, CurrentBestBoard, BestBoard).


% minimax(+Board, +Depth, +Alpha, +Beta, +Player, -Eval, -BestBoard).
% Base cases when the game is over or it is the maximum wanted depth.
minimax(Board, Depth, _, _, Player, Eval) :-  
    (
        Depth =:= 0;
        \+ possible_actions(Player, Board, _)
    ),
    evaluate_board(Player, Board, Eval).

% Case when to get the worst board for the opponent.
minimax(Board, Depth, Alpha, Beta, Player, Eval) :- 
    % write(Player), write(' '), write(Depth), write(' '), write(Eval), nl,
    % boards:print_board(Board), nl,
    minimizing(Player),
    minimax_helper(Board, NewBoard, Depth, NewDepth, Player),
    minEval(NewBoard, NewDepth, Alpha, Beta, Player, 9999, Eval).

% Case when to get the best board for the current player.
minimax(Board, Depth, Alpha, Beta, Player, Eval) :- 
    % write(Player), write(' '), write(Depth), write(' '), write(Eval), nl,
    % boards:print_board(Board), nl,
    maximizing(Player),
    minimax_helper(Board, NewBoard, Depth, NewDepth, Player),
    maxEval(NewBoard, NewDepth, Alpha, Beta, Player, -9999, Eval).


% minimax_helper(+Board, -NewBoard, +Depth, -NewDepth, +Player, -NextPlayer).
minimax_helper(Board, NewBoard, Depth, NewDepth, Player) :-
    Depth > 0,
    \+ game_over(Board),
    possible_actions(Player, Board, NewBoard),
    NewDepth is Depth - 1.


% maxEval(+Board, +Depth, +Alpha, +Beta, +Player, +TempMaxEval, -MaxEval, -Board).
maxEval(Board, Depth, Alpha, Beta, Player, TempMaxEval, MaxEval) :-
    opponent(Player, NextPlayer),
    minimax(Board, Depth, Alpha, Beta, NextPlayer, Eval),
    MaxEval is max(TempMaxEval, Eval),
    NewAlpha is max(Alpha, Eval),
    (
        Beta > NewAlpha;
        Beta =< NewAlpha, !
    ).

% minEval(+Board, +Depth, +Alpha, +Beta, +Player, +TempMaxEval, -MinEval, -Board).
minEval(Board, Depth, Alpha, Beta, Player, TempMinEval, MinEval) :-
    opponent(Player, NextPlayer),
    minimax(Board, Depth, Alpha, Beta, NextPlayer, Eval),
    MinEval is min(TempMinEval, Eval),
    NewBeta is min(Beta, Eval),
    (
        NewBeta > Alpha;
        NewBeta =< Alpha, !
    ).