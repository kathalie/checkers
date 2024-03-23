import 'package:flutter/material.dart';

import '../../presentation/visuals.dart';

enum CheckerColor {
  white(whitePieceColors),
  black(blackPieceColors);

  /// (Outer, inner) colors of a checker.
  final (Color, Color) displayColors;

  /// In which row the opponent checkers are promoted.
  final int homeRow;

  const CheckerColor(this.displayColors);

  CheckerColor flip() => switch (this) {
        CheckerColor.white => CheckerColor.black,
        CheckerColor.black => CheckerColor.white,
      };
}
