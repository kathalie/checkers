import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import 'game_widget.dart';

class MainPage extends ConsumerWidget {
  const MainPage({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    return const Scaffold(
      body: SafeArea(
        minimum: EdgeInsets.all(16),
        child: Center(
          child: GameWidget(),
        ),
      ),
    );
  }
}
