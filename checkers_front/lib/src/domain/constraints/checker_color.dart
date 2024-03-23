import 'package:flutter/material.dart';

import '../../presentation/visuals.dart';

enum CheckerColor {
  white(whitePieceColors),
  black(blackPieceColors);

  /// (Outer, inner) colors of a checker.
  final (Color, Color) displayColors;

  const CheckerColor(this.displayColors);

  CheckerColor flip() => switch (this) {
        CheckerColor.white => CheckerColor.black,
        CheckerColor.black => CheckerColor.white,
      };
}
