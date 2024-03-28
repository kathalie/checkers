import 'package:collection/collection.dart';

import '../domain/constants.dart';
import '../domain/constraints/checker_color.dart';
import '../domain/position_functions.dart';
import '../domain/typedefs.dart';
import 'board/board.dart';

const _directions = [(-1, -1), (1, 1), (-1, 1), (1, -1)];
final _manDirections = [_directions, _directions.map((vec) => multiply(vec, 2))].flattened;
final _kingDirections = List.generate(boardSide, (index) => index + 1)
    .expand((dist) => _directions.map((vec) => multiply(vec, dist)));

class Checker {
  final CheckerColor color;
  final bool isKing;
  final int key;

  const Checker({
    required this.color,
    this.isKing = false,
    required this.key,
  });

  /// Returns all the possible target cells where this checker could go.
  ///
  /// It does not check whether the path is available.
  Iterable<Position> possibleTargets({required Position fromPosition, required Board board}) {
    final pos = fromPosition;

    return (isKing ? _kingDirections : _manDirections)
        .map((dir) => add(vector: dir, toPosition: pos))
        .where(board.isValidPosition);
  }

  /// Whether this checker can go from [from] to [to].
  ///
  /// It only checks the direction and distance, and not obstacles.
  /// It also does not account for the situations when a checker cab beat backwards.
  bool canMove({required Position to, required Position from}) {
    if (isKing) {
      return true;
    }

    final (fromRow, toRow) = (from.$1, to.$1);

    return (color == CheckerColor.black ? fromRow < toRow : fromRow > toRow) &&
        diagonalDistanceBetween(from, to) == 1;
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Checker &&
          runtimeType == other.runtimeType &&
          color == other.color &&
          isKing == other.isKing &&
          key == other.key;

  @override
  int get hashCode => color.hashCode ^ isKing.hashCode ^ key.hashCode;
}
