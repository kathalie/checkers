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
    final normalizedBoard = color == CheckerColor.white ? board : BoardMirror(board);

    return _service.fetchTurn(
      board: normalizedBoard,
      lastMoved: lastMoved,
      depth: depth,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is PrologHandle && runtimeType == other.runtimeType && color == other.color;

  @override
  int get hashCode => color.hashCode;
}
