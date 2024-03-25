import '../../../domain/constraints/checker_color.dart';
import '../../../domain/typedefs.dart';
import '../../board/board.dart';
import '../player_handle.dart';

class MockHandle implements PlayerHandle {
  @override
  final CheckerColor color;

  const MockHandle(this.color);

  @override
  Future<Movement> takeTurn({
    required Board board,
    required Position? lastMoved,
  }) async =>
      (from: (0, 0), to: (1, 1));
}
