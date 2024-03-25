import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../application/driver/game_driver.dart';
import '../application/providers/game_driver_provider.dart';
import '../application/providers/session_settings_notifier.dart';
import 'widgets/board_widget.dart';
import 'widgets/checker_widget.dart';

class GameWidget extends ConsumerStatefulWidget {
  const GameWidget({super.key});

  @override
  ConsumerState<GameWidget> createState() => _GameWidgetState();
}

class _GameWidgetState extends ConsumerState<GameWidget> {
  GameDriver get _driver => ref.read(gameDriverProvider);

  int get _depth =>
      ref.read(sessionSettingsNotifierProvider).aiDifficulties[_driver.currentPlayer]!;

  @override
  void initState() {
    super.initState();

    final driver = _driver;
    driver.onStepEnded = () {
      if (!mounted || driver.isGameOver) {
        return;
      }

      WidgetsBinding.instance.addPostFrameCallback((_) => driver.step(_depth));
    };

    driver.step(_depth);
  }

  @override
  Widget build(BuildContext context) {
    final driver = ref.watch(gameDriverProvider);
    const separator = SizedBox(height: 32);
    final theme = Theme.of(context);

    return ListenableBuilder(
      listenable: driver,
      builder: (context, child) => Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          Row(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              Text('Current turn', style: theme.textTheme.titleLarge),
              const SizedBox(width: 12),
              Column(
                children: [
                  CheckerPiece.color(driver.currentPlayer),
                  const SizedBox(height: 6),
                  Text(
                    driver.currentHandle.name,
                    style: theme.textTheme.titleSmall,
                  ),
                ],
              ),
            ],
          ),
          separator,
          BoardWidget(board: driver.board),
        ],
      ),
    );
  }
}
