import '../../domain/typedefs.dart';
import 'board.dart';
import 'board_action.dart';

extension BoardChildren on Board {
  Board childFrom(Movement move) => copy()..move(from: move.from, to: move.to);

  Iterable<Board> childrenFrom(Iterable<Movement> movements) => movements.map(childFrom);
}
