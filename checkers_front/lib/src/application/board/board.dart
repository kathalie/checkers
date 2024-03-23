import '../checker.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';

abstract interface class Board {
  Checker? operator [](Position pos);

  void operator []=(Position pos, Checker? checker);

  MoveMode moveMode({required Position from, required Position to});

  bool isValidPosition(Position position);
}
