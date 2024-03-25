import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../application/providers/game_driver_provider.dart';
import 'game_widget.dart';
import 'main_page.dart';

class GamePage extends ConsumerWidget {
  const GamePage({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    const safeArea = 16.0;

    return Scaffold(
      floatingActionButton: FloatingActionButton(
        onPressed: () {
          WidgetsBinding.instance.addPostFrameCallback((timeStamp) {
            ref.invalidate(gameDriverProvider);
          });

          Navigator.of(context).pushReplacement(
            MaterialPageRoute(builder: (context) => const MainPage()),
          );
        },
        child: const Icon(Icons.home),
      ),
      body: const SafeArea(
        minimum: EdgeInsets.all(safeArea),
        child: Center(
          child: GameWidget(),
        ),
      ),
    );
  }
}
