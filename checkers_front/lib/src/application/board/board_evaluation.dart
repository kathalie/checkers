import 'package:collection/collection.dart';

import '../../domain/constraints/checker_color.dart';
import 'board.dart';
import '../checker.dart';

extension BoardEvaluation on Board {
  int evaluate(CheckerColor player) => [whites, blacks]
      .flattened
      .map((pos) => this[pos]!.evaluate(player))
      .reduce((val, el) => val + el);
}

extension CheckerEvaluation on Checker {
  int evaluate(CheckerColor player) => (color == player ? 1 : -1) * (1 + (isKing ? 1 : 0));
}
