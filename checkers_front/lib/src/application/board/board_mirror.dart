import '../../domain/constants.dart';
import '../../domain/constraints/checker_color.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';
import '../checker.dart';
import 'board.dart';
import 'board_mixin.dart';

abstract class _BoardMirror implements Board {}

/// A mirrored [Board] that has its vertical axis and checker colors flipped.
class BoardMirror extends _BoardMirror with BoardMixin {
  final Board _wrapped;

  BoardMirror(this._wrapped);

  @override
  Checker? operator [](Position pos) => _wrapped[flip(pos)]?.flipped();

  @override
  void operator []=(Position pos, Checker? checker) => _wrapped[flip(pos)] = checker?.flipped();

  @override
  Iterable<Position> get blacks => _wrapped.blacks.map(flip);

  @override
  Iterable<Position> get whites => _wrapped.whites.map(flip);

  @override
  bool isValidPosition(Position position) => _wrapped.isValidPosition(flip(position));

  @override
  MoveMode moveMode({required Position from, required Position to}) =>
      _wrapped.moveMode(from: flip(from), to: flip(to)).flipped();

  @override
  Iterable<MustBeat> mustBeatAt(Position pos) =>
      _wrapped.mustBeatAt(flip(pos)).map((e) => e.flipped() as MustBeat);

  @override
  bool playerMustBeat(CheckerColor color) => _wrapped.playerMustBeat(color.flipped());

  @override
  Iterable<CanMoveOrBeat> possibleMoves({required Position from}) =>
      _wrapped.possibleMoves(from: flip(from)).map((e) => e.flipped() as CanMoveOrBeat);
}

Position flip(Position pos) {
  final (row, col) = pos;

  return (lastIndex - row, col);
}

extension _ColorFlip on Checker {
  Checker flipped() => Checker(
        color: color.flipped(),
        isKing: isKing,
        key: key,
      );
}

extension _FlipPositions on MoveMode {
  MoveMode flipped() => switch (this) {
        CanMove(:final from, :final to) => CanMove(from: flip(from), to: flip(to)),
        MustBeat(:final from, :final to, :final at) =>
          MustBeat(from: flip(from), to: flip(to), at: flip(at)),
        _ => this,
      };
}
