import '../../domain/constraints/checker_color.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';
import '../checker.dart';

abstract interface class Board {
  Checker? operator [](Position pos);

  void operator []=(Position pos, Checker? checker);

  Iterable<Position> get whites;

  Iterable<Position> get blacks;

  MoveMode moveMode({required Position from, required Position to});

  Iterable<CanMoveOrBeat> possibleMoves({required Position from});

  Iterable<MustBeat> mustBeatAt(Position pos);

  bool playerMustBeat(CheckerColor color);

  bool isValidPosition(Position position);

  Board copy();
}
