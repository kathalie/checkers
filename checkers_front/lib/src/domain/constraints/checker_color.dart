import 'package:flutter/material.dart';

import '../../presentation/visuals.dart';
import '../constants.dart';

enum CheckerColor {
  white(
    displayColors: whitePieceColors,
    homeRow: lastIndex,
  ),
  black(
    displayColors: blackPieceColors,
    homeRow: 0,
  );

  /// (Outer, inner) colors of a checker.
  final (Color, Color) displayColors;

  /// In which row the opponent checkers are promoted.
  final int homeRow;

  const CheckerColor({
    required this.displayColors,
    required this.homeRow,
  });

  CheckerColor flip() => switch (this) {
        CheckerColor.white => CheckerColor.black,
        CheckerColor.black => CheckerColor.white,
      };
}
