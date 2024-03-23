import 'package:flutter/material.dart';

import '../application/board/board_impl.dart';
import '../application/checker.dart';
import '../domain/constraints/checker_color.dart';
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
            board: BoardImpl(<List<Checker?>>[
              [null, const Checker(color: CheckerColor.black), null, null, null, null, null, null],
              [null, null, null, null, const Checker(color: CheckerColor.white), null, null, null],
              [null, null, null, null, null, null, null, null],
              [null, null, const Checker(color: CheckerColor.black), null, null, null, null, null],
              [null, null, null, null, null, null, null, null],
              [null, null, null, null, null, null, null, null],
              [null, null, null, null, null, null, null, null],
              [null, null, null, null, null, null, null, null],
            ]),
          ),
        ),
      ),
    );
  }
}
