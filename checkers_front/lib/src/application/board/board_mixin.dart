import '../checker.dart';
import '../../domain/constants.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/position_functions.dart';
import '../../domain/typedefs.dart';
import 'board.dart';

const _cannotMove = CannotMove();

mixin BoardMixin on Board {
  @override
  bool isValidPosition(Position position) {
    final (row, col) = position;

    return _isValidIndex(row) && _isValidIndex(col);
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

    final length = diagonalVectorLength(vec);

    if (length == 1) {
      return CanMove(from: from, to: to);
    }

    final firstCheckerBetween = _firstCheckerBetween(from, to);

    if (firstCheckerBetween == null) {
      return checker.isKing ? CanMove(from: from, to: to) : _cannotMove;
    }

    final (other, pos) = firstCheckerBetween;

    if (other.color != checker.color &&
        (checker.isKing || diagonalDistanceBetween(from, pos) == 2)) {
      return MustBeat(from: from, to: to, beatsAt: pos);
    }

    return _cannotMove;
  }

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

  bool _isValidIndex(int index) => index >= 0 && index < boardSide;
}
