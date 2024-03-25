// ignore_for_file: avoid_manual_providers_as_generated_provider_dependency
import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';

import '../../domain/constraints/checker_color.dart';
import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';
import 'current_handle_provider.dart';
import 'game_driver_provider.dart';

part 'movable_checkers_provider.g.dart';

@riverpod
IMap<Position, IList<CanMoveOrBeat>> movableCheckers(MovableCheckersRef ref) {
  // rebuild on turn switch
  final currentColor = ref.watch(currentHandleProvider).color;

  final driver = ref.watch(gameDriverProvider);
  final board = driver.board;

  final checkerPositions = currentColor == CheckerColor.white ? board.whites : board.blacks;

  final movesFromPos = IMap.fromEntries(
    checkerPositions.map(
      (pos) => MapEntry(pos, IList(board.possibleMoves(from: pos))),
    ),
  );

  final mustBeat = movesFromPos
      .map((pos, moves) => MapEntry(pos, IList(moves.whereType<MustBeat>())))
      .removeWhere((pos, moves) => moves.isEmpty);

  if (mustBeat.isNotEmpty) {
    return mustBeat;
  }

  return movesFromPos.removeWhere((pos, moves) => moves.isEmpty);
}
