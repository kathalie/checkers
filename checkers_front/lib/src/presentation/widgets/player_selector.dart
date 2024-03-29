import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../../application/driver/handles/dart_handle.dart';
import '../../application/driver/handles/prolog_handle.dart';
import '../../application/driver/handles/real_player_handle.dart';
import '../../application/driver/player_handle.dart';
import '../../application/providers/session_settings_notifier.dart';
import '../../domain/constraints/checker_color.dart';
import '../../http/prolog_player_service.dart';
import 'checker_widget.dart';

class PlayerSelector extends ConsumerWidget {
  final CheckerColor color;
  final void Function(PlayerHandle handle) onSelected;
  final PlayerHandle selected;

  const PlayerSelector({
    required this.color,
    required this.selected,
    required this.onSelected,
    super.key,
  });

  void _onChanged(PlayerHandle? handle) {
    if (handle == null) {
      return;
    }

    onSelected(handle);
  }

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final settings = ref.watch(sessionSettingsNotifierProvider);

    const prologService = PrologPlayerService();

    final handles = [
      RealPlayerHandle(color),
      PrologHandle(color, service: prologService),
      DartHandle(color),
    ];

    final selected = this.selected;

    final child = Row(
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

    if (selected is RealPlayerHandle) {
      return child;
    }

    const depths = [2, 4, 6];
    const labels = ['Beginner', 'Master', 'Pro'];
    final currentValue = settings.aiDifficulties[color]!;

    final index = depths.indexOf(currentValue);

    return Column(
      mainAxisSize: MainAxisSize.min,
      children: [
        child,
        const SizedBox(height: 8),
        Text(labels[index]),
        SizedBox(
          height: 50,
          child: Slider(
            divisions: 2,
            min: 0,
            max: (depths.length - 1).toDouble(),
            value: index.toDouble(),
            onChanged: (value) {
              final index = value.round();
              ref
                  .read(sessionSettingsNotifierProvider.notifier)
                  .setAiDifficulty(color, depths[index]);
            },
          ),
        ),
      ],
    );
  }
}
