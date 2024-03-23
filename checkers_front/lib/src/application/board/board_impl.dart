import 'package:collection/collection.dart';

import '../../domain/typedefs.dart';
import '../checker.dart';
import 'board.dart';
import 'board_mixin.dart';

abstract class _BoardImpl implements Board {}

class BoardImpl extends _BoardImpl with BoardMixin {
  final List<List<Checker?>> _field;

  BoardImpl(this._field);

  @override
  Checker? operator [](Position pos) {
    _validatePosition(pos);

    final (row, col) = pos;

    return _field[row][col];
  }

  @override
  void operator []=(Position pos, Checker? checker) {
    _validatePosition(pos);

    final (row, col) = pos;

    _field[row][col] = checker;
  }

  void _validatePosition(Position pos) {
    if (!isValidPosition(pos)) {
      throw StateError('Tried accessing an invalid position $pos');
    }
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is BoardImpl &&
          runtimeType == other.runtimeType &&
          const ListEquality(DeepCollectionEquality()).equals(_field, other._field);

  @override
  int get hashCode => _field.hashCode;
}
