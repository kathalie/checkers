import 'package:riverpod_annotation/riverpod_annotation.dart';

import '../board/board_generator.dart';
import '../board/board_impl.dart';
import '../driver/game_driver.dart';
import 'handles_notifier.dart';

part 'game_driver_provider.g.dart';

@riverpod
class GameDriverNotifier extends _$GameDriverNotifier {
  @override
  GameDriver build() {
    ref.keepAlive();
    ref.onDispose(() => state.dispose());

    final (:white, :black) = ref.watch(handlesNotifierProvider);

    return GameDriver(
      BoardImpl(generateInitialBoard()),
      whiteHandle: white,
      blackHandle: black,
    );
  }
}
