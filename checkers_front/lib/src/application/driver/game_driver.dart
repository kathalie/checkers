import 'package:flutter/foundation.dart';

import '../../domain/constraints/checker_color.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';
import '../board/board.dart';
import '../board/board_winner.dart';
import '../checker.dart';
import 'player_handle.dart';

class GameDriver extends ChangeNotifier {
  static Future<void> noAnimationCallback() => Future.delayed(const Duration());

  final Board board;
  final Handles _handles;
  CheckerColor _currentPlayerColor = CheckerColor.white;
  Position? _lastMoved;

  /// Is called when the movement has been made and the turn was switched.
  void Function()? onStepEnded;

  /// This callback is called after the board has been changed if the current
  /// handle requires animation.
  ///
  /// The driver will wait for the returned Future to complete before switching turns.
  Future<void> Function() animationCallback = noAnimationCallback;

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

  bool get isGameOver => winner != null;

  CheckerColor? _winner;

  CheckerColor? get winner => _winner ?? board.winner(currentPlayer);

  void _switchTurn() {
    _currentPlayerColor = _currentPlayerColor.flipped();
    _lastMoved = null;
  }

  PlayerHandle handleOf(CheckerColor playerColor) =>
      playerColor == CheckerColor.white ? _handles.white : _handles.black;

  bool _isDisposed = false;

  @override
  void dispose() {
    _isDisposed = true;
    super.dispose();
  }

  @override
  void notifyListeners() {
    if (_isDisposed) {
      return;
    }

    super.notifyListeners();
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

      if (movement == null) {
        _winner = currentPlayer.flipped();
        return;
      }

      from = movement.from;
      to = movement.to;
    } on Error catch (err) {
      print(err);
      print(err.stackTrace);
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
        await _waitForAnimationIfNeeded();
        _onMoved();
        _switchTurn();
      case MustBeat(:final at):
        if (board[at] == null) {
          throw StateError('Cannot beat a missing checker at $at');
        }
        _move(from: from, to: to);
        await _waitForAnimationIfNeeded();
        board[at] = null;
        _onMoved();

        if (board.mustBeatAt(to).isEmpty) {
          _switchTurn();
        }
      default:
        throw StateError('Something went terribly wrong with MoveMode switch');
    }

    notifyListeners();
    onStepEnded?.call();
  }

  Future<void> _waitForAnimationIfNeeded() async {
    if (!currentHandle.needsAnimation) {
      return;
    }

    return animationCallback();
  }

  void _move({required Position from, required Position to}) {
    board[to] = board[from];
    board[from] = null;
    _lastMoved = to;
    board.lastMove = (from: from, to: to);
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
        'to move a checker of the opponent at $pos',
      );
    }
  }
}
