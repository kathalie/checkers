import '../../domain/constraints/checker_color.dart';
import 'board.dart';

extension BoardWinner on Board {
  CheckerColor? winner(CheckerColor currentPlayer) {
    if (whites.isEmpty || (currentPlayer == CheckerColor.white && !canMove(CheckerColor.white))) {
      return CheckerColor.black;
    }

    if (blacks.isEmpty || (currentPlayer == CheckerColor.black && !canMove(CheckerColor.black))) {
      return CheckerColor.white;
    }

    return null;
  }

  bool canMove(CheckerColor player) {
    final checkers = player == CheckerColor.white ? whites : blacks;

    return checkers.expand((pos) => possibleMoves(from: pos)).isNotEmpty;
  }
}
