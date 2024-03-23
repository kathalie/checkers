import 'package:flutter/material.dart';

import '../application/board/board_generator.dart';
import '../application/board/board_impl.dart';
import 'widgets/board_widget.dart';

class MainPage extends StatelessWidget {
  const MainPage({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SafeArea(
        minimum: const EdgeInsets.all(16),
        child: Center(
          child: BoardWidget(
            board: BoardImpl(generateInitialBoard()),
          ),
        ),
      ),
    );
  }
}
