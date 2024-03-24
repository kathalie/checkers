import 'package:riverpod_annotation/riverpod_annotation.dart';

import '../board/board_generator.dart';
import '../board/board_impl.dart';
import '../driver/game_driver.dart';
import '../driver/handles/mock_handle.dart';
import 'handles_provider.dart';

part 'game_driver_provider.g.dart';

@riverpod
class GameDriverNotifier extends _$GameDriverNotifier {
  @override
  GameDriver build() {
    ref.keepAlive();

    final (p1Handle, p2Handle) = ref.watch(handlesProviderNotifierProvider) ??
        (
          const MockHandle(),
          const MockHandle(),
        );

    return GameDriver(
      BoardImpl(generateInitialBoard()),
      p1Handle: p1Handle,
      p2Handle: p2Handle,
    );
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
