import '../../../domain/constraints/checker_color.dart';
import '../../../domain/typedefs.dart';
import '../../../http/prolog_player_service.dart';
import '../../board/board.dart';
import '../../board/board_mirror.dart';
import '../player_handle.dart';

class PrologHandle implements PlayerHandle {
  @override
  String get name => 'Prolog AI';

  @override
  final CheckerColor color;

  final PrologPlayerService _service;

  const PrologHandle(
    this.color, {
    required PrologPlayerService service,
  }) : _service = service;

  @override
  Future<Movement> takeTurn({
    required Board board,
    required Position? lastMoved,
    required int depth,
  }) async {
    final shallFlip = color == CheckerColor.black;

    final movement = await _service.fetchTurn(
      board: shallFlip ? BoardMirror(board) : board,
      lastMoved: lastMoved,
      depth: depth,
    );

    await Future.delayed(const Duration(milliseconds: 250));

    if (!shallFlip) {
      return movement;
    }

    return (from: flip(movement.from), to: flip(movement.to));
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is PrologHandle && runtimeType == other.runtimeType && color == other.color;

  @override
  int get hashCode => color.hashCode;
}
