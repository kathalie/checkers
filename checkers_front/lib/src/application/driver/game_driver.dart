import 'package:flutter/foundation.dart';

import '../../domain/constraints/checker_color.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';
import '../board/board.dart';
import '../checker.dart';
import 'player_handle.dart';

class GameDriver extends ChangeNotifier {
  final Board _board;
  final List<PlayerHandle> _handles;
  CheckerColor _currentPlayerColor = CheckerColor.white;
  Position? _lastMoved;

  GameDriver(
    this._board, {
    required PlayerHandle p1Handle,
    required PlayerHandle p2Handle,
  }) : _handles = List.unmodifiable([p1Handle, p2Handle]);

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

  /// Is called when the movement has been made and the turn was switched.
  void _onStepEnded() => notifyListeners();

  Future<void> step() async {
    final Movement(:from, :to) = await _currentHandle.takeTurn(
      board: _board,
      lastMoved: _lastMoved,
    );

    _validateCheckerAt(from);

    final moveMode = _board.moveMode(from: from, to: to);

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
        if (_board[at] == null) {
          throw StateError('Cannot beat a missing checker at $at');
        }
        _move(from: from, to: to);
        _board[at] = null;
        // todo score

        _onMoved();

        if (!_canBeatAt(to)) {
          _switchTurn();
        }
    }

    _onStepEnded();
  }

  void _move({required Position from, required Position to}) {
    _board[to] = _board[from];
    _board[from] = null;
    _lastMoved = to;
  }

  void _promoteAt(Position pos) {
    final checker = _board[pos];

    if (checker == null || checker.isKing) {
      return;
    }

    _board[pos] = Checker(color: checker.color, isKing: true);
  }

  bool _canBeatAt(Position pos) {
    final checker = _board[pos];

    if (checker == null) {
      return false;
    }

    final targets = checker
        .possibleTargets(fromPosition: pos, board: _board)
        .map((target) => _board.moveMode(from: pos, to: target))
        .whereType<MustBeat>();

    return targets.isNotEmpty;
  }

  void _validateCheckerAt(Position pos) {
    final checker = _board[pos];

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
