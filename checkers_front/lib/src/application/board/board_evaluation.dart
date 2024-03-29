import 'package:collection/collection.dart';

import '../../domain/constraints/checker_color.dart';
import '../checker.dart';
import 'board.dart';

extension BoardEvaluation on Board {
  int evaluate(CheckerColor player) => [whites, blacks]
      .flattened
      .map((pos) => this[pos]!.evaluate(player))
      .reduce((val, el) => val + el);
}

extension CheckerEvaluation on Checker {
  int evaluate(CheckerColor player) {
    return switch (this) {
      Checker(:final color, :final isKing) when color == player => 1 + (isKing ? 10 : 0),
      Checker(:final isKing) => -2 - (isKing ? 20 : 0),
    };
  }
}
