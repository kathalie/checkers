import '../../domain/constraints/checker_color.dart';
import '../../domain/typedefs.dart';
import '../board/board.dart';

abstract interface class PlayerHandle {
  Future<Movement> takeTurn({
    required Board board,
    required CheckerColor color,
    required Position? lastMoved,
  });
}
