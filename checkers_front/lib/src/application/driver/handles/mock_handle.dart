import '../../../domain/constraints/checker_color.dart';
import '../../../domain/typedefs.dart';
import '../../board/board.dart';
import '../player_handle.dart';

class MockHandle implements PlayerHandle {
  @override
  String get name => 'Mock';

  @override
  final CheckerColor color;

  const MockHandle(this.color);

  @override
  Future<Movement> takeTurn({
    required Board board,
    required Position? lastMoved,
    required int depth,
  }) async =>
      (from: (0, 0), to: (1, 1));

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is MockHandle && runtimeType == other.runtimeType && color == other.color;

  @override
  int get hashCode => color.hashCode;
}
