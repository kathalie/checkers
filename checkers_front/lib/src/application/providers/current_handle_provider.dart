import 'package:riverpod_annotation/riverpod_annotation.dart';

import '../driver/player_handle.dart';
import 'game_driver_provider.dart';

part 'current_handle_provider.g.dart';

@riverpod
PlayerHandle currentHandle(CurrentHandleRef ref) =>
    ref.watch(gameDriverNotifierProvider).currentHandle;
