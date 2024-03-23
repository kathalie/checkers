import '../../domain/constants.dart';
import '../../domain/constraints/checker_color.dart';
import '../../domain/position_functions.dart';
import '../checker.dart';

List<List<Checker?>> generateInitialBoard() {
  return List.generate(
    boardSide,
    (row) => List.generate(boardSide, (column) {
      if (row >= filledRows && row < boardSide - filledRows || !isBlackCell((row, column))) {
        return null;
      }

      return Checker(
        color: row < filledRows ? CheckerColor.black : CheckerColor.white,
      );
    }),
  );
}
