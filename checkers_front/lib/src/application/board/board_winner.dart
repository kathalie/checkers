import '../../domain/constraints/checker_color.dart';
import 'board.dart';

extension BoardWinner on Board {
  CheckerColor? get winner {
    if (whites.isEmpty) {
      return CheckerColor.black;
    }

    if (blacks.isEmpty) {
      return CheckerColor.white;
    }

    return null;
  }
}
