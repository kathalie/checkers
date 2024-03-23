import '../../domain/typedefs.dart';
import '../checker.dart';
import 'board.dart';
import 'board_mixin.dart';

abstract class _BoardImpl implements Board {}

class BoardImpl extends _BoardImpl with BoardMixin {
  final List<List<Checker?>> field;

  BoardImpl(this.field);

  @override
  Checker? operator [](Position pos) {
    _validatePosition(pos);

    final (row, col) = pos;

    return field[row][col];
  }

  @override
  void operator []=(Position pos, Checker? checker) {
    _validatePosition(pos);

    final (row, col) = pos;

    field[row][col] = checker;
  }

  void _validatePosition(Position pos) {
    if (!isValidPosition(pos)) {
      throw StateError('Tried accessing an invalid position $pos');
    }
  }
}
