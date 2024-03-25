:- module(minimax, [
    minimax/4
]).

:- use_module(mechanics).
:- use_module(evaluation).

/**
% board_eating, white, 1

boards:board_eating(Board), 
write("Board:"), nl, boards:print_board(Board), nl,
minimax(Board, 1, white, NewBoard), 
write("New Board: "), nl, boards:print_board(NewBoard), nl, 
!.


% board_eating, black, 1

boards:board_eating(Board), 
write("Board:"), nl, boards:print_board(Board), nl,
minimax(Board, 1, black, NewBoard), 
write("New Board: "), nl, boards:print_board(NewBoard), nl, 
!.


% board_eating, black, 3

boards:board_eating(Board), 
write("Board:"), nl, boards:print_board(Board), nl,
minimax(Board, 1, black, NewBoard), 
write("New Board: "), nl, boards:print_board(NewBoard), nl, 
!.



% board_cannot_eat, black, 3

boards:board_cannot_eat(Board), 
write("Board:"), nl, boards:print_board(Board), nl,
minimax(Board, 1, black, NewBoard), 
write("New Board: "), nl, boards:print_board(NewBoard), nl, 
!.

% board_cannot_eat, white, 3

boards:board_cannot_eat(Board), 
write("Board:"), nl, boards:print_board(Board), nl,
minimax(Board, 1, white, NewBoard), 
write("New Board: "), nl, boards:print_board(NewBoard), nl, 
!.
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

% minimax(+Board, +Depth, +Player, -BestBoard).
minimax(Board, Depth, Player, BestBoard) :- 
    minimax(Board, Depth, -9999, 9999, Player, Eval, BestBoard), write(Eval), nl.

% minimax(+Board, +Depth, +Alpha, +Beta, +Player, -Eval, -BestBoard).
% Base cases when the game is over or it is the maximum wanted depth.
minimax(Board, Depth, _, _, Player, Eval, Board) :-  
    (
        game_over(Board);
        Depth =:= 0
    ),
    evaluate_board(Player, Board, Eval).
    % boards:print_board(Board), nl, write("Eval: "), write(Eval), nl.

% Case when to get the worst board for the opponent.
minimax(Board, Depth, Alpha, Beta, Player, Eval, BestBoard) :- 
    minimax_helper(Board, NewBoard, Depth, NewDepth, Player),
    minimizing(Player),
    minEval(NewBoard, NewDepth, Alpha, Beta, Player, 9999, Eval, BestBoard).

% Case when to get the best board for the current player.
minimax(Board, Depth, Alpha, Beta, Player, Eval, BestBoard) :- 
    minimax_helper(Board, NewBoard, Depth, NewDepth, Player),
    maximizing(Player),
    maxEval(NewBoard, NewDepth, Alpha, Beta, Player, -9999, Eval, BestBoard).


% minimax_helper(+Board, -NewBoard, +Depth, -NewDepth, +Player).
minimax_helper(Board, NewBoard, Depth, NewDepth, Player) :-
    Depth > 0,
    possible_actions(Player, Board, NewBoard),
    NewDepth is Depth - 1.


% maxEval(+Board, +Depth, +Alpha, +Beta, +Player, +TempMaxEval, -MaxEval, -Board).
maxEval(Board, Depth, Alpha, Beta, Player, TempMaxEval, MaxEval, Board) :-
    opponent(Player, NextPlayer),
    minimax(Board, Depth, Alpha, Beta, NextPlayer, Eval, _),
    MaxEval is max(TempMaxEval, Eval),
    NewAlpha is max(Alpha, Eval),
    (
        Beta > NewAlpha;
        Beta =< NewAlpha, !
    ).

% minEval(+Board, +Depth, +Alpha, +Beta, +Player, +TempMaxEval, -MinEval, -Board).
minEval(Board, Depth, Alpha, Beta, Player, TempMinEval, MinEval, Board) :-
    opponent(Player, NextPlayer),
    minimax(Board, Depth, Alpha, Beta, NextPlayer, Eval, _),
    MinEval is min(TempMinEval, Eval),
    NewBeta is min(Beta, Eval),
    (
        NewBeta > Alpha;
        NewBeta =< Alpha, !
    ).