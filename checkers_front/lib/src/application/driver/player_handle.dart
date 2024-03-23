import '../../domain/typedefs.dart';
import '../board/board.dart';

abstract interface class PlayerHandle {
  Future<Movement> takeTurn({required Board board, Position? lastMoved});
}
