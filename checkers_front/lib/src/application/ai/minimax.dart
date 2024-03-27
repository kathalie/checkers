import 'dart:math';

import '../../domain/constraints/checker_color.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';
import '../board/board.dart';
import '../board/board_children.dart';
import '../board/board_evaluation.dart';

const _maxVal = 100000;

Movement difference(Board a, Board b, CheckerColor lastMoved) {
  Set<Position> positions(Board board) =>
      lastMoved == CheckerColor.white ? board.whites.toSet() : board.blacks.toSet();

  print(positions(a));
  print(positions(b));

  final from = positions(a).difference(positions(b)).single;
  final to = positions(b).difference(positions(a)).single;

  return (from: from, to: to);
}

Board nextBoard(Board board, int depth, Position? lastMoved) {
  final children = _children(board, CheckerColor.white, lastMoved);

  Board? bestBoard;

  var eval = -_maxVal;

  for (final child in children) {
    final val = minimax(child, depth, -100000, 100000, CheckerColor.black);

    if (val > eval) {
      eval = val;
      bestBoard = child;
    }
  }

  return bestBoard!;
}

int minimax(Board board, int depth, int alpha, int beta, CheckerColor maximizingPlayer) {
  if (depth == 0) {
    return board.evaluate(maximizingPlayer);
  }

  final boardWinner = board.winner;
  if (boardWinner != null) {
    return (boardWinner == CheckerColor.white ? 100000 : -100000);
  }

  final children = _children(board, maximizingPlayer, null);

  if (maximizingPlayer == CheckerColor.white) {
    var maxEval = -_maxVal;

    for (final child in children) {
      final eval = minimax(child, depth - 1, alpha, beta, CheckerColor.black);
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
    final eval = minimax(child, depth - 1, alpha, beta, CheckerColor.white);
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

extension _BoardWinner on Board {
  CheckerColor? get winner {
    if (whites.isEmpty) {
      return CheckerColor.black;
    }

    if (blacks.isEmpty) {
      return CheckerColor.white;
    }

    return null;
  }
}
