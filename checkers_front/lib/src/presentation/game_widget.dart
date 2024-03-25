import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../application/driver/handles/real_player_handle.dart';
import '../application/driver/player_handle.dart';
import '../application/providers/game_driver_provider.dart';
import 'widgets/board_widget.dart';

class GameWidget extends ConsumerStatefulWidget {
  const GameWidget({super.key});

  @override
  ConsumerState<GameWidget> createState() => _GameWidgetState();
}

class _GameWidgetState extends ConsumerState<GameWidget> {
  late final gameDriver = ref.read(gameDriverNotifierProvider);

  @override
  void initState() {
    super.initState();

    gameDriver.onStepEnded = onStepEnded;
    gameDriver.step();
  }

  void onStepEnded() {
    setState(() {});
    print('Step ended; asking for the next step');
    gameDriver.step();
  }

  @override
  Widget build(BuildContext context) {
    return BoardWidget(board: gameDriver.board);
  }
}
