import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';

import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';
import 'game_driver_provider.dart';

part 'highlight_notifier.g.dart';

@riverpod
class HighlightNotifier extends _$HighlightNotifier {
  @override
  IList<MoveMode> build() => <MoveMode>[].lock;

  void reset() => state = <MoveMode>[].lock;

  void updateHighlightFor(Position pos) {
    final board = ref.read(gameDriverNotifierProvider).board;
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

    state = moves.toList(growable: false).lock;
  }
}
