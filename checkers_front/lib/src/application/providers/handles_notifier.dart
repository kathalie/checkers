import 'package:riverpod_annotation/riverpod_annotation.dart';

import '../../domain/constraints/checker_color.dart';
import '../../domain/typedefs.dart';
import '../driver/handles/real_player_handle.dart';
import '../driver/player_handle.dart';

part 'handles_notifier.g.dart';

@riverpod
class HandlesNotifier extends _$HandlesNotifier {
  @override
  Handles build() {
    ref.keepAlive();
    ref.onDispose(() {
      final (:white, :black) = state;
      [white, black].whereType<RealPlayerHandle>().forEach((h) => h.dispose());
    });

    return (
      white: RealPlayerHandle(CheckerColor.white),
      black: RealPlayerHandle(CheckerColor.black)
    );
  }

  Handles current() => state;

  void updateWith(PlayerHandle handle) {
    final (:white, :black) = state;

    final newState = switch (handle.color) {
      CheckerColor.white => (() {
          if (white is RealPlayerHandle) {
            white.dispose();
          }
          return (white: handle, black: black);
        })(),
      CheckerColor.black => (() {
          if (black is RealPlayerHandle) {
            black.dispose();
          }
          return (white: white, black: handle);
        })(),
    };

    state = newState;
  }
}
