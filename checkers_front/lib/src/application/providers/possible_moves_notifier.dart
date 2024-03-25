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
    // ignore: avoid_manual_providers_as_generated_provider_dependency
    final gameDriver = ref.read(gameDriverProvider);
    final board = gameDriver.board;
    final checker = board[pos];

    if (checker == null) {
      return reset();
    }

    final moves = board.possibleMoves(from: pos);

    final mustBeat = moves.whereType<MustBeat>();
    if (mustBeat.isNotEmpty) {
      state = IList(mustBeat);
      return;
    }

    state = IList(moves);
  }
}
