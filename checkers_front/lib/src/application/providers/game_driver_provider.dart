import 'package:riverpod_annotation/riverpod_annotation.dart';

import '../board/board.dart';
import '../driver/game_driver.dart';
import '../driver/player_handle.dart';

part 'game_driver_provider.g.dart';

@riverpod
class GameDriverNotifier extends _$GameDriverNotifier {
  @override
  GameDriver build(Board board, PlayerHandle p1Handle, PlayerHandle p2Handle) {
    ref.keepAlive();
    return GameDriver(board, p1Handle: p1Handle, p2Handle: p2Handle);
  }

  Future<void> step() async {
    await state.step();

    _notifyListeners();
  }

  void _notifyListeners() {
    final copy = state.copy()..onStepEnded = state.onStepEnded;
    state.onStepEnded = null;
    state = copy;
  }
}
