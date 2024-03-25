import '../../domain/constraints/checker_color.dart';
import '../../domain/typedefs.dart';
import '../board/board.dart';

abstract interface class PlayerHandle {
  CheckerColor get color;

  Future<Movement> takeTurn({
    required Board board,
    required Position? lastMoved,
  });
}
