import '../../domain/constants.dart';
import '../../domain/constraints/checker_color.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/position_functions.dart';
import '../../domain/typedefs.dart';
import '../checker.dart';
import 'board.dart';

const _cannotMove = CannotMove();

mixin BoardMixin on Board {
  @override
  bool isValidPosition(Position position) {
    bool isValidIndex(int index) => index >= 0 && index < boardSide;

    final (row, col) = position;

    return isValidIndex(row) && isValidIndex(col);
  }

  @override
  MoveMode moveMode({
    required Position from,
    required Position to,
  }) {
    if (!isValidPosition(from) || !isValidPosition(to)) {
      throw StateError('Invalid position: from $from, to $to');
    }

    final checker = this[from];

    if (checker == null || from == to || this[to] != null) {
      return _cannotMove;
    }

    final vec = vector(from: from, to: to);
    if (!isDiagonal(vec)) {
      return _cannotMove;
    }

    final firstCheckerBetween = _firstCheckerBetween(from, to);

    if (firstCheckerBetween == null) {
      return checker.canMove(from: from, to: to) ? CanMove(from: from, to: to) : _cannotMove;
    }

    final (other, otherPos) = firstCheckerBetween;

    if (other.color != checker.color) {
      late final next = _firstCheckerBetween(otherPos, to);

      if (diagonalDistanceBetween(from, to) == 2 ||
          (checker.isKing &&
              (next == null ||
                  diagonalDistanceBetween(otherPos, next.$2) >
                      diagonalDistanceBetween(otherPos, to)))) {
        return MustBeat(from: from, to: to, at: otherPos);
      }
    }

    return _cannotMove;
  }

  @override
  Iterable<CanMoveOrBeat> possibleMoves({required Position from}) =>
      this[from]
          ?.possibleTargets(fromPosition: from, board: this)
          .map((target) => moveMode(from: from, to: target))
          .whereType<CanMoveOrBeat>() ??
      [];

  @override
  Iterable<MustBeat> mustBeatAt(Position pos) => possibleMoves(from: pos).whereType<MustBeat>();

  @override
  bool playerMustBeat(CheckerColor color) =>
      (color == CheckerColor.white ? whites : blacks).any((pos) => mustBeatAt(pos).isNotEmpty);

  (Checker, Position)? _firstCheckerBetween(Position pos1, Position pos2) {
    final direction = directionOf(vector(from: pos1, to: pos2));

    var pos = add(vector: direction, toPosition: pos1);
    while (pos != pos2 && isValidPosition(pos)) {
      final checker = this[pos];

      if (checker != null) {
        return (checker, pos);
      }

      pos = add(vector: direction, toPosition: pos);
    }

    return null;
  }
}
