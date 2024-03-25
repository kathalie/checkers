import 'package:riverpod_annotation/riverpod_annotation.dart';

import '../../domain/constraints/checker_color.dart';
import '../../domain/typedefs.dart';
import '../driver/handles/real_player_handle.dart';
import '../driver/player_handle.dart';

part 'handles_notifier.g.dart';

@riverpod
class HandlesNotifier extends _$HandlesNotifier {
  @override
  Handles build() =>
      (white: RealPlayerHandle(CheckerColor.white), black: RealPlayerHandle(CheckerColor.black));

  Handles current() => state;

  void updateWith(PlayerHandle handle) {
    final (:white, :black) = state;

    final newState = switch (handle.color) {
      CheckerColor.white => (white: handle, black: black),
      CheckerColor.black => (white: white, black: handle),
    };

    state = newState;
  }
}
