import '../../domain/constraints/checker_color.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';
import '../board/board.dart';
import '../checker.dart';
import 'player_handle.dart';

class GameDriver {
  final Board board;
  final List<PlayerHandle> _handles;
  CheckerColor _currentPlayerColor = CheckerColor.white;
  Position? _lastMoved;

  /// Is called when the movement has been made and the turn was switched.
  void Function()? onStepEnded;

  GameDriver(
    this.board, {
    required PlayerHandle p1Handle,
    required PlayerHandle p2Handle,
  }) : _handles = List.unmodifiable([p1Handle, p2Handle]);

  /// Returns a copy of this [GameDriver].
  GameDriver copy() {
    return GameDriver(board, p1Handle: _handles.first, p2Handle: _handles.last)
      .._currentPlayerColor = _currentPlayerColor
      .._lastMoved = _lastMoved;
  }

  CheckerColor get currentPlayer => _currentPlayerColor;

  PlayerHandle get _currentHandle =>
      _currentPlayerColor == CheckerColor.white ? _handles.first : _handles.last;

  void _switchTurn() {
    _currentPlayerColor = _currentPlayerColor.flip();
    _lastMoved = null;
  }

  /// Is called when the movement has been made, but the current turn is not over yet.
  void _onMoved() {
    final lastMoved = _lastMoved;

    if (lastMoved == null || lastMoved.$1 != _currentPlayerColor.flip().homeRow) {
      return;
    }

    _promoteAt(lastMoved);
  }

  Future<void> step() async {
    final Movement(:from, :to) = await _currentHandle.takeTurn(
      board: board,
      color: _currentPlayerColor,
      lastMoved: _lastMoved,
    );

    _validateCheckerAt(from);

    final moveMode = board.moveMode(from: from, to: to);

    switch (moveMode) {
      case CannotMove():
        throw StateError(
          'Player ${_currentPlayerColor.name} tried '
          'to make an impossible move',
        );
      case CanMove():
        _move(from: from, to: to);
        _onMoved();
        _switchTurn();
      case MustBeat(:final at):
        if (board[at] == null) {
          throw StateError('Cannot beat a missing checker at $at');
        }
        _move(from: from, to: to);
        board[at] = null;
        // todo score

        _onMoved();

        if (!_mustBeatAt(to)) {
          _switchTurn();
        }
      default:
        throw StateError('Something went terribly wrong with MoveMode switch');
    }

    onStepEnded?.call();
  }

  void _move({required Position from, required Position to}) {
    board[to] = board[from];
    board[from] = null;
    _lastMoved = to;
  }

  void _promoteAt(Position pos) {
    final checker = board[pos];

    if (checker == null || checker.isKing) {
      return;
    }

    board[pos] = Checker(color: checker.color, isKing: true);
  }

  bool _mustBeatAt(Position pos) => board.possibleMoves(from: pos).whereType<MustBeat>().isNotEmpty;

  void _validateCheckerAt(Position pos) {
    final checker = board[pos];

    if (checker == null) {
      throw StateError(
        'Player ${_currentPlayerColor.name} tried '
        'to move a checker that is not present in position $pos',
      );
    }

    if (checker.color != _currentPlayerColor) {
      throw StateError(
        'Player ${_currentPlayerColor.name} tried '
        'to move a checker of the opponent',
      );
    }
  }
}
