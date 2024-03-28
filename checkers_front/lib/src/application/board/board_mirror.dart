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
  final Board wrapped;

  BoardMirror(this.wrapped);

  @override
  Checker? operator [](Position pos) => wrapped[flip(pos)]?.flipped();

  @override
  void operator []=(Position pos, Checker? checker) => wrapped[flip(pos)] = checker?.flipped();

  @override
  Iterable<Position> get blacks => wrapped.blacks.map(flip);

  @override
  Iterable<Position> get whites => wrapped.whites.map(flip);

  @override
  bool isValidPosition(Position position) => wrapped.isValidPosition(flip(position));

  @override
  MoveMode moveMode({required Position from, required Position to}) =>
      wrapped.moveMode(from: flip(from), to: flip(to)).flipped();

  @override
  Iterable<MustBeat> mustBeatAt(Position pos) =>
      wrapped.mustBeatAt(flip(pos)).map((e) => e.flipped() as MustBeat);

  @override
  bool playerMustBeat(CheckerColor color) => wrapped.playerMustBeat(color.flipped());

  @override
  Iterable<CanMoveOrBeat> possibleMoves({required Position from}) =>
      wrapped.possibleMoves(from: flip(from)).map((e) => e.flipped() as CanMoveOrBeat);

  @override
  Board copy() => BoardMirror(wrapped.copy());

  @override
  Movement? get lastMove => wrapped.lastMove;

  @override
  set lastMove(Movement? movement) => wrapped.lastMove = movement;
}

Position flip(Position pos) {
  final (row, col) = pos;

  return (lastIndex - row, lastIndex - col);
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
