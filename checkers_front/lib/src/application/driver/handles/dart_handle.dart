import 'package:flutter/foundation.dart';

import '../../../domain/constraints/checker_color.dart';
import '../../../domain/typedefs.dart';
import '../../ai/minimax.dart';
import '../../board/board.dart';
import '../player_handle.dart';

class DartHandle implements PlayerHandle {
  @override
  String get name => 'Dart AI';

  @override
  bool get needsAnimation => true;

  @override
  final CheckerColor color;

  const DartHandle(this.color);

  @override
  Future<Movement?> takeTurn({
    required Board board,
    required Position? lastMoved,
    required int depth,
  }) async {
    final next = await compute(
      (message) => nextBoard(message.$1, message.$2, message.$3, message.$4),
      (board, 6, lastMoved, color),
    );

    if (next == null) {
      return null;
    }

    final movement = maybeDifference(board, next, color);

    return movement;
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is DartHandle && runtimeType == other.runtimeType && color == other.color;

  @override
  int get hashCode => color.hashCode;
}
