import '../domain/constants.dart';
import '../domain/constraints/checker_color.dart';
import '../domain/position_functions.dart';
import '../domain/typedefs.dart';
import 'board/board.dart';

const _manDirections = [(-1, -1), (1, 1), (-1, 1), (1, -1)];
final _kingDirections = List.generate(boardSide, (index) => index + 1)
    .expand((dist) => _manDirections.map((vec) => multiply(vec, dist)));

class Checker {
  final CheckerColor color;
  final bool isKing;

  const Checker({required this.color, this.isKing = false});

  /// Returns all the possible target cells where this checker could go.
  ///
  /// It does not check whether the path is available.
  Iterable<Position> possibleTargets({required Position fromPosition, required Board board}) {
    final pos = fromPosition;

    return (isKing ? _kingDirections : _manDirections)
        .map((dir) => add(vector: dir, toPosition: pos))
        .where(board.isValidPosition);
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Checker &&
          runtimeType == other.runtimeType &&
          color == other.color &&
          isKing == other.isKing;

  @override
  int get hashCode => color.hashCode ^ isKing.hashCode;
}
