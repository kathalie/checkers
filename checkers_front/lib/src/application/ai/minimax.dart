import 'dart:math';

import '../../domain/constraints/checker_color.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';
import '../board/board.dart';
import '../board/board_children.dart';
import '../board/board_evaluation.dart';
import '../board/board_winner.dart';

const _maxVal = 100000;

Movement difference(Board a, Board b, CheckerColor lastMoved) => maybeDifference(a, b, lastMoved)!;

Movement? maybeDifference(Board a, Board b, CheckerColor lastMoved) {
  Set<Position> positions(Board board) =>
      lastMoved == CheckerColor.white ? board.whites.toSet() : board.blacks.toSet();

  final from = positions(a).difference(positions(b)).singleOrNull;
  final to = positions(b).difference(positions(a)).singleOrNull;

  return from != null && to != null ? (from: from, to: to) : null;
}

Board? nextBoard(Board board, int depth, Position? lastMoved, CheckerColor playerColor) {
  final children = _children(board, playerColor, lastMoved);

  Board? bestBoard = children.firstOrNull;

  var eval = -_maxVal;

  for (final child in children) {
    final val = minimax(child, depth, -100000, 100000, playerColor.flipped(), playerColor);

    if (val > eval) {
      eval = val;
      bestBoard = child;
    }
  }

  return bestBoard;
}

int minimax(
  Board board,
  int depth,
  int alpha,
  int beta,
  CheckerColor maximizingPlayer,
  CheckerColor playerColor,
) {
  if (depth == 0) {
    return board.evaluate(maximizingPlayer);
  }

  final boardWinner = board.winner;
  if (boardWinner != null) {
    return (boardWinner == playerColor ? 100000 : -100000);
  }

  final children = _children(board, maximizingPlayer, null);

  if (maximizingPlayer == playerColor) {
    var maxEval = -_maxVal;

    for (final child in children) {
      final eval = minimax(child, depth - 1, alpha, beta, maximizingPlayer.flipped(), playerColor);
      maxEval = max(maxEval, eval);
      alpha = max(alpha, eval);
      if (beta <= alpha) {
        break;
      }
    }

    return maxEval;
  }

  var minEval = _maxVal;

  for (final child in children) {
    final eval = minimax(child, depth - 1, alpha, beta, maximizingPlayer.flipped(), playerColor);
    minEval = min(minEval, eval);
    beta = min(beta, eval);
    if (beta <= alpha) {
      break;
    }
  }

  return minEval;
}

Iterable<Board> _children(Board board, CheckerColor maximizingPlayer, Position? lastMoved) {
  final checkerPositions = lastMoved != null
      ? [lastMoved]
      : maximizingPlayer == CheckerColor.white
          ? board.whites
          : board.blacks;

  final allModes =
      checkerPositions.expand((pos) => board.possibleMoves(from: pos)).whereType<CanMoveOrBeat>();

  final mustBeat = allModes.whereType<MustBeat>();
  final modes = mustBeat.isNotEmpty ? mustBeat : allModes;

  final moves = modes.map((mode) => (from: mode.from, to: mode.to));

  return board.childrenFrom(moves);
}
