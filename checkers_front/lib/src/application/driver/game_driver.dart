import 'package:flutter/foundation.dart';

import '../../domain/constraints/checker_color.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';
import '../board/board.dart';
import 'player_handle.dart';

class GameDriver extends ChangeNotifier {
  final Board board;
  final List<PlayerHandle> handles;
  CheckerColor _currentPlayerColor = CheckerColor.white;
  Position? _lastMoved;

  GameDriver(this.board, {required PlayerHandle p1Handle, required PlayerHandle p2Handle})
      : handles = [p1Handle, p2Handle];

  PlayerHandle get _currentHandle =>
      _currentPlayerColor == CheckerColor.white ? handles.first : handles.last;

  void _switchTurn() {
    _currentPlayerColor = _currentPlayerColor.flip();
    _lastMoved = null;

    notifyListeners();
  }

  /// Is called when the movement has been made, but the current turn is not over yet.
  void _onMoved() {}

  /// Is called when the movement has been made and the turn was switched.
  void _onStepEnded() {}

  Future<void> step() async {
    final Movement(:from, :to) = await _currentHandle.takeTurn(
      board: board,
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

        if (!_canBeatAt(to)) {
          _switchTurn();
        }
    }

    _onStepEnded();
  }

  void _move({required Position from, required Position to}) {
    board[to] = board[from];
    board[from] = null;
  }

  bool _canBeatAt(Position pos) {
    final checker = board[pos];

    if (checker == null) {
      return false;
    }

    final targets = checker
        .possibleTargets(fromPosition: pos, board: board)
        .map((target) => board.moveMode(from: pos, to: target))
        .whereType<MustBeat>();

    return targets.isNotEmpty;
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
