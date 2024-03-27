import 'package:collection/collection.dart';

import '../../domain/constants.dart';
import '../../domain/constraints/checker_color.dart';
import '../../domain/typedefs.dart';
import '../checker.dart';
import 'board.dart';
import 'board_mixin.dart';

abstract class _BoardImpl implements Board {}

class BoardImpl extends _BoardImpl with BoardMixin {
  final List<List<Checker?>> _field;

  final Set<Position> _whiteCheckersCache = {};

  final Set<Position> _blackCheckersCache = {};

  BoardImpl(this._field) {
    _cacheCheckers();
  }

  @override
  Iterable<Position> get whites => UnmodifiableSetView(_whiteCheckersCache);

  @override
  Iterable<Position> get blacks => UnmodifiableSetView(_blackCheckersCache);

  void _cacheCheckers() {
    for (var row = 0; row < boardSide; row++) {
      for (var col = 0; col < boardSide; col++) {
        final checker = _field[row][col];
        _cacheChecker(checker, (row, col));
      }
    }
  }

  void _cacheChecker(Checker? checker, Position pos) {
    if (checker != null) {
      (checker.color == CheckerColor.white ? _whiteCheckersCache : _blackCheckersCache).add(pos);
      return;
    }

    _whiteCheckersCache.remove(pos);
    _blackCheckersCache.remove(pos);
  }

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

    _cacheChecker(checker, pos);
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

  @override
  Board copy() => BoardImpl(
        List.from(
          _field.map((row) => row.toList(growable: false)),
          growable: false,
        ),
      );
}
