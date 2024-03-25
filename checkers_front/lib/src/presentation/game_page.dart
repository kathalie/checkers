import 'package:flutter/material.dart';

import 'game_widget.dart';

class GamePage extends StatelessWidget {
  const GamePage({super.key});

  @override
  Widget build(BuildContext context) {
    const safeArea = 16.0;

    return const Scaffold(
      body: SafeArea(
        minimum: EdgeInsets.all(safeArea),
        child: Center(
          child: GameWidget(),
        ),
      ),
    );
  }
}
