import 'package:flutter/material.dart';

import 'widgets/board.dart';

class MainPage extends StatelessWidget {
  const MainPage({super.key});

  @override
  Widget build(BuildContext context) {
    return const Scaffold(
      body: SafeArea(
        minimum: EdgeInsets.all(16),
        child: Center(
          child: Board(),
        ),
      ),
    );
  }
}
