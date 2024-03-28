import '../../domain/constraints/move_mode.dart';
import '../../domain/typedefs.dart';
import 'board.dart';

extension BoardAction on Board {
  void move({required Position from, required Position to}) {
    final mode = moveMode(from: from, to: to);

    if (mode is! CanMoveOrBeat) {
      return;
    }

    this[mode.to] = this[mode.from];
    this[mode.from] = null;

    if (mode is MustBeat) {
      this[mode.at] = null;
    }
  }
}
