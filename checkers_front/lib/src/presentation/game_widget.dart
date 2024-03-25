import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../application/driver/game_driver.dart';
import '../application/providers/game_driver_provider.dart';
import 'widgets/board_widget.dart';

class GameWidget extends ConsumerStatefulWidget {
  const GameWidget({super.key});

  @override
  ConsumerState<GameWidget> createState() => _GameWidgetState();
}

class _GameWidgetState extends ConsumerState<GameWidget> {
  GameDriver get _driver => ref.read(gameDriverProvider);

  @override
  void initState() {
    super.initState();
    final driver = _driver;
    driver.onStepEnded = () {
      WidgetsBinding.instance.addPostFrameCallback((_) => driver.step());
    };
    driver.step();
  }

  @override
  Widget build(BuildContext context) {
    final driver = ref.watch(gameDriverProvider);

    return ListenableBuilder(
      listenable: driver,
      builder: (context, child) => Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          Text('current: ${driver.currentPlayer}'),
          BoardWidget(board: driver.board),
        ],
      ),
    );
  }
}
