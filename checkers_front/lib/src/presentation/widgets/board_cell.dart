import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../../application/board/board.dart';
import '../../application/checker.dart';
import '../../application/driver/handles/real_player_handle.dart';
import '../../application/providers/current_handle_provider.dart';
import '../../application/providers/movable_checkers_provider.dart';
import '../../application/providers/possible_moves_notifier.dart';
import '../../application/providers/session_settings_notifier.dart';
import '../../domain/constants.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/position_functions.dart';
import '../../domain/typedefs.dart';
import '../visuals.dart';
import 'checker_widget.dart';

class BoardCell extends ConsumerWidget {
  final (int, int) position;
  final Checker? pieceContained;
  final bool displayPiece;

  const BoardCell({
    required this.position,
    this.pieceContained,
    this.displayPiece = true,
    super.key,
  });

  static BoardCell fromBoard(
    Board board, {
    required Position position,
    bool displayPiece = true,
  }) =>
      BoardCell(
        position: position,
        pieceContained: board[position],
        displayPiece: displayPiece,
      );

  Border get border {
    const side = BorderSide();
    const noSide = BorderSide.none;

    final (row, col) = position;

    return Border(
      top: row.isEven ? side : noSide,
      right: col.isEven || col == lastIndex ? side : noSide,
      bottom: row.isEven || row == lastIndex ? side : noSide,
      left: col.isEven ? side : noSide,
    );
  }

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final pieceContained = this.pieceContained;
    late final cellBackground = isBlackCell(position) ? blackCellColor : null;

    final possibleMoves = ref.watch(movableCheckersProvider);
    final canPieceMove = possibleMoves[position] != null;

    final possibleMove = ref
        .watch(possibleMovesNotifierProvider)
        .where((mode) => mode.willHighlight(position))
        .firstOrNull;

    final assistHighlight = ref.watch(sessionSettingsNotifierProvider).movesHighlight
        ? switch (possibleMove) {
            CanMoveOrBeat(:final to) when to == position => highlightColors.accessibleCell,
            MustBeat(:final at) when at == position => highlightColors.toBeBeatenChecker,
            _ => null,
          }
        : null;

    return DragTarget<(Checker, Position)>(
      onWillAccept: (data) {
        final currentHandle = ref.read(currentHandleProvider);

        return currentHandle is RealPlayerHandle;
      },
      onAccept: (data) {
        if (possibleMove == null) {
          return;
        }

        final currentHandle = ref.read(currentHandleProvider);

        if (currentHandle is RealPlayerHandle) {
          currentHandle.movementSink.add(
            (from: data.$2, to: possibleMove.to),
          );
        }
      },
      builder: (context, candidateData, rejectedData) => AspectRatio(
        aspectRatio: 1 / 1,
        child: Container(
          decoration: BoxDecoration(
            border: border,
            color:
                assistHighlight ?? (canPieceMove ? highlightColors.movableChecker : cellBackground),
          ),
          child: displayPiece && pieceContained != null
              ? Center(
                  child: CheckerWidget(
                    checker: pieceContained,
                    position: position,
                  ),
                )
              : null,
        ),
      ),
    );
  }
}
