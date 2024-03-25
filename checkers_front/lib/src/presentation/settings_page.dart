import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../application/providers/session_settings_notifier.dart';

class SettingsPage extends ConsumerWidget {
  const SettingsPage({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    return Scaffold(
      appBar: AppBar(),
      body: SafeArea(
        minimum: const EdgeInsets.all(8),
        child: Column(
          children: [
            CheckboxListTile(
              title: const Text('Enable movement assist highlight'),
              subtitle: const Text('Highlight the available moves on checker drag'),
              value: ref.watch(sessionSettingsNotifierProvider).movesHighlight,
              onChanged: (newValue) {
                if (newValue == null) {
                  return;
                }

                ref.read(sessionSettingsNotifierProvider.notifier).toggleHighlight();
              },
            ),
          ],
        ),
      ),
    );
  }
}
