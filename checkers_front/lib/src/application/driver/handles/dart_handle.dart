import 'package:flutter/foundation.dart';

import '../../../domain/constraints/checker_color.dart';
import '../../../domain/typedefs.dart';
import '../../ai/minimax.dart';
import '../../board/board.dart';
import '../../board/board_mirror.dart';
import '../player_handle.dart';

class DartHandle implements PlayerHandle {
  @override
  String get name => 'Dart AI';

  @override
  final CheckerColor color;

  const DartHandle(this.color);

  @override
  Future<Movement> takeTurn({
    required Board board,
    required Position? lastMoved,
    required int depth,
  }) async {
    // todo dart ai for black checkers

    final shallFlip = color == CheckerColor.black;

    final next = await compute(
      (message) => nextBoard(message.$1, message.$2, message.$3),
      (shallFlip ? BoardMirror(board) : board, 5, lastMoved),
    );

    final movement = difference(
      shallFlip ? BoardMirror(board) : board,
      next,
      color,
    );

    if (!shallFlip) {
      return movement;
    }

    return (from: flip(movement.from), to: flip(movement.to));
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is DartHandle && runtimeType == other.runtimeType && color == other.color;

  @override
  int get hashCode => color.hashCode;
}
