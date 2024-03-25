import 'package:riverpod_annotation/riverpod_annotation.dart';

import '../../domain/constraints/checker_color.dart';
import '../driver/handles/real_player_handle.dart';
import '../driver/player_handle.dart';

part 'handles_notifier.g.dart';

@riverpod
class HandlesNotifier extends _$HandlesNotifier {
  @override
  (PlayerHandle, PlayerHandle) build() =>
      (RealPlayerHandle(CheckerColor.white), RealPlayerHandle(CheckerColor.black));
}
