import 'dart:async';

import '../../../domain/constraints/checker_color.dart';
import '../../../domain/typedefs.dart';
import '../../board/board.dart';
import '../player_handle.dart';

class RealPlayerHandle implements PlayerHandle {
  @override
  final CheckerColor color;

  final StreamController<Movement> _movementStreamController = StreamController.broadcast();

  RealPlayerHandle(this.color);

  /// Movements are sent through the [movementSink] then the player has chosen their movement.
  StreamSink<Movement> get movementSink => _movementStreamController.sink;

  /// Closes the composed streams and frees the resources.
  /// Must be called after this handle is no more needed.
  void dispose() => _movementStreamController.close();

  @override
  Future<Movement> takeTurn({
    required Board board,
    required Position? lastMoved,
  }) =>
      _movementStreamController.stream.first;
}
