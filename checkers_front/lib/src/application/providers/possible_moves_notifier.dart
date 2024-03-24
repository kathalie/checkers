import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';

import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';
import 'game_driver_provider.dart';

part 'possible_moves_notifier.g.dart';

@riverpod
class PossibleMovesNotifier extends _$PossibleMovesNotifier {
  @override
  IList<CanMoveOrBeat> build() => <CanMoveOrBeat>[].lock;

  void reset() => state = <CanMoveOrBeat>[].lock;

  void updateMovesFor(Position pos) {
    final gameDriver = ref.read(gameDriverNotifierProvider);
    final board = gameDriver.board;
    final checker = board[pos];

    if (checker == null) {
      return reset();
    }

    final moves = board.possibleMoves(from: pos);

    final mustBeat = moves.whereType<MustBeat>();
    if (mustBeat.isNotEmpty) {
      state = mustBeat.toList(growable: false).lock;
      return;
    }

    if (gameDriver.currentPlayerPositions.any((pos) => board.mustBeatAt(pos).isNotEmpty)) {
      state = <CanMoveOrBeat>[].lock;
      return;
    }

    state = moves.toList(growable: false).lock;
  }
}
