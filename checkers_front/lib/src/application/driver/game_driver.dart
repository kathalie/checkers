import 'package:flutter/foundation.dart';

import '../../domain/constraints/checker_color.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';
import '../board/board.dart';
import '../checker.dart';
import 'handles/real_player_handle.dart';
import 'player_handle.dart';

class GameDriver extends ChangeNotifier {
  final Board board;
  final Handles _handles;
  CheckerColor _currentPlayerColor = CheckerColor.white;
  Position? _lastMoved;

  /// Is called when the movement has been made and the turn was switched.
  void Function()? onStepEnded;

  GameDriver(
    this.board, {
    required PlayerHandle whiteHandle,
    required PlayerHandle blackHandle,
  }) : _handles = (white: whiteHandle, black: blackHandle);

  CheckerColor get currentPlayer => _currentPlayerColor;

  Iterable<Position> get currentPlayerPositions =>
      currentPlayer == CheckerColor.white ? board.whites : board.blacks;

  PlayerHandle get currentHandle =>
      _currentPlayerColor == CheckerColor.white ? _handles.white : _handles.black;

  bool get isGameOver => board.whites.isEmpty || board.blacks.isEmpty;

  @override
  void dispose() {
    final (:white, :black) = _handles;
    [black, white].whereType<RealPlayerHandle>().forEach((handle) => handle.dispose());
    super.dispose();
  }

  void _switchTurn() {
    _currentPlayerColor = _currentPlayerColor.flipped();
    _lastMoved = null;
  }

  /// Is called when the movement has been made, but the current turn is not over yet.
  void _onMoved() {
    final lastMoved = _lastMoved;

    if (lastMoved == null || lastMoved.$1 != _currentPlayerColor.flipped().homeRow) {
      return;
    }

    _promoteAt(lastMoved);
  }

  Future<void> step(int depth) async {
    if (isGameOver) {
      return;
    }

    late final Position from;
    late final Position to;

    try {
      final movement = await currentHandle.takeTurn(
        board: board,
        lastMoved: _lastMoved,
        depth: depth,
      );

      from = movement.from;
      to = movement.to;
    } catch (err) {
      return;
    }

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

        if (board.mustBeatAt(to).isEmpty) {
          _switchTurn();
        }
      default:
        throw StateError('Something went terribly wrong with MoveMode switch');
    }

    onStepEnded?.call();
    notifyListeners();
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

    board[pos] = Checker(
      color: checker.color,
      isKing: true,
      key: checker.key,
    );
  }

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
