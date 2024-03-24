import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../application/driver/player_handle.dart';
import '../application/providers/game_driver_provider.dart';
import 'widgets/board_widget.dart';

class GameWidget extends ConsumerWidget {
  final PlayerHandle playerHandle;
  final PlayerHandle enemyHandle;

  const GameWidget({
    required this.playerHandle,
    required this.enemyHandle,
    super.key,
  });

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final gameDriver = ref.watch(gameDriverNotifierProvider);

    return BoardWidget(board: gameDriver.board);
  }
}
