import '../domain/constants.dart';
import '../domain/typedefs.dart';

/// Transforms the [index] in a flat representation into a [Position].
Position flatToPosition(int index) {
  assert(index >= 0 && index < boardCellCount, 'Index must be in the board bounds');

  final row = index ~/ boardSide;
  final column = index - boardSide * row;

  return (row, column);
}

/// Transforms the [position] into an index in a flat representation.
int positionToFlat(Position position) {
  final (row, column) = position;

  assert(row >= 0 && row < boardSide, 'Row must be in the board bounds');
  assert(column >= 0 && column < boardSide, 'Column must be in the board bounds');

  return boardSide * row + column;
}
