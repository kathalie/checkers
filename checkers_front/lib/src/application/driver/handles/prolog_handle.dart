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

  final int _depth;

  final PrologPlayerService _service;

  const PrologHandle(
    this.color, {
    required PrologPlayerService service,
    int depth = 3,
  })  : _depth = depth,
        _service = service;

  @override
  Future<Movement> takeTurn({required Board board, required Position? lastMoved}) async {
    final normalizedBoard = color == CheckerColor.white ? board : BoardMirror(board);

    return _service.fetchTurn(
      board: normalizedBoard,
      lastMoved: lastMoved,
      depth: _depth,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is PrologHandle &&
          runtimeType == other.runtimeType &&
          color == other.color &&
          _depth == other._depth;

  @override
  int get hashCode => color.hashCode ^ _depth.hashCode;
}
