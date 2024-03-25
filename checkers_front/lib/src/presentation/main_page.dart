import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../application/driver/handles/mock_handle.dart';
import '../application/driver/handles/real_player_handle.dart';
import '../application/driver/player_handle.dart';
import '../application/providers/handles_notifier.dart';
import '../domain/constraints/checker_color.dart';
import 'settings_page.dart';
import 'widgets/checker_widget.dart';
import 'game_page.dart';

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

    final (:white, :black) = ref.watch(handlesNotifierProvider);

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
                _PlayerSelector(
                  color: CheckerColor.white,
                  selected: white,
                  onSelected: (handle) =>
                      _onHandleSelected(ref.read(handlesNotifierProvider.notifier), handle),
                ),
                const Padding(
                  padding: EdgeInsets.symmetric(vertical: padding),
                  child: Text('VS'),
                ),
                _PlayerSelector(
                  color: CheckerColor.black,
                  selected: black,
                  onSelected: (handle) =>
                      _onHandleSelected(ref.read(handlesNotifierProvider.notifier), handle),
                ),
                const SizedBox(height: padding * 2),
                ElevatedButton(
                  onPressed: () => _startGame(context),
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

class _PlayerSelector extends StatelessWidget {
  final CheckerColor color;
  final void Function(PlayerHandle handle) onSelected;
  final PlayerHandle selected;

  const _PlayerSelector({
    required this.color,
    required this.selected,
    required this.onSelected,
  });

  void _onChanged(PlayerHandle? handle) {
    if (handle == null) {
      return;
    }

    onSelected(handle);
  }

  @override
  Widget build(BuildContext context) {
    final handles = [
      RealPlayerHandle(color),
      MockHandle(color),
    ];

    return Row(
      children: [
        CheckerPiece.color(color),
        Expanded(
          child: Padding(
            padding: const EdgeInsets.only(left: 24),
            child: DropdownButton(
              isExpanded: true,
              onChanged: _onChanged,
              value: selected,
              items: handles
                  .map(
                    (e) => DropdownMenuItem(
                      value: e,
                      child: Text(e.name),
                    ),
                  )
                  .toList(growable: false),
            ),
          ),
        ),
      ],
    );
  }
}
