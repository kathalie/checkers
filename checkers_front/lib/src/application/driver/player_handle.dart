import '../../domain/constraints/checker_color.dart';
import '../../domain/typedefs.dart';
import '../board/board.dart';

abstract interface class PlayerHandle {
  String get name;

  CheckerColor get color;

  Future<Movement> takeTurn({
    required Board board,
    required Position? lastMoved,
    required int depth,
  });
}
