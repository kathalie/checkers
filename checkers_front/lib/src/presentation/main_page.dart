import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../application/driver/handles/prolog_handle.dart';
import '../application/driver/player_handle.dart';
import '../application/providers/handles_notifier.dart';
import '../application/providers/prolog_availability_provider.dart';
import '../domain/constraints/checker_color.dart';
import 'game_page.dart';
import 'settings_page.dart';
import 'widgets/player_selector.dart';
import 'widgets/prolog_status.dart';

class MainPage extends ConsumerWidget {
  const MainPage({super.key});

  void _onHandleSelected(HandlesNotifier notifier, PlayerHandle handle) =>
      notifier.updateWith(handle);

  void _startGame(BuildContext context) {
    Navigator.of(context).pushReplacement(
      MaterialPageRoute(
        builder: (context) => const GamePage(),
      ),
    );
  }

  void _openSettings(BuildContext context) {
    Navigator.of(context).push(
      MaterialPageRoute(
        builder: (context) => const SettingsPage(),
      ),
    );
  }

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    const padding = 16.0;
    const safeArea = padding;
    const horizontalPadding = padding * 4;

    final prologAvailabilityValue = ref.watch(prologAvailabilityProvider);
    final isPrologAvailable = switch (prologAvailabilityValue) {
      AsyncData(:final value) => value,
      _ => false,
    };

    final (:white, :black) = ref.watch(handlesNotifierProvider);
    final isPrologSelected = white is PrologHandle || black is PrologHandle;

    final isStartUnlocked = !isPrologSelected || isPrologAvailable;

    return Scaffold(
      floatingActionButton: FloatingActionButton(
        onPressed: () => _openSettings(context),
        child: const Icon(Icons.settings),
      ),
      body: SafeArea(
        minimum: const EdgeInsets.all(safeArea),
        child: Center(
          child: Padding(
            padding: const EdgeInsets.symmetric(horizontal: horizontalPadding),
            child: Column(
              mainAxisSize: MainAxisSize.max,
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                Text(
                  'Checkers',
                  style: Theme.of(context).textTheme.headlineLarge?.copyWith(color: Colors.black),
                ),
                const SizedBox(height: padding * 3),
                PlayerSelector(
                  color: CheckerColor.white,
                  selected: white,
                  onSelected: (handle) =>
                      _onHandleSelected(ref.read(handlesNotifierProvider.notifier), handle),
                ),
                const Padding(
                  padding: EdgeInsets.symmetric(vertical: padding),
                  child: Text('VS'),
                ),
                PlayerSelector(
                  color: CheckerColor.black,
                  selected: black,
                  onSelected: (handle) =>
                      _onHandleSelected(ref.read(handlesNotifierProvider.notifier), handle),
                ),
                const SizedBox(height: padding * 2),
                if (isPrologSelected) ...[
                  const PrologStatus(),
                  const SizedBox(height: padding),
                ],
                ElevatedButton(
                  onPressed: isStartUnlocked ? () => _startGame(context) : null,
                  child: const Text('START'),
                ),
              ],
            ),
          ),
        ),
      ),
    );
  }
}
