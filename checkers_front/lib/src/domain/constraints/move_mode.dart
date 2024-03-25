import '../typedefs.dart';

sealed class MoveMode {
  const MoveMode();

  bool willHighlight(Position pos) => false;
}

class CannotMove extends MoveMode {
  const CannotMove();

  @override
  bool operator ==(Object other) =>
      identical(this, other) || other is CannotMove && runtimeType == other.runtimeType;

  @override
  int get hashCode => 0;
}

abstract class CanMoveOrBeat extends MoveMode {
  final Position from;
  final Position to;

  const CanMoveOrBeat({required this.from, required this.to});

  @override
  bool willHighlight(Position pos) => to == pos;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is CanMoveOrBeat &&
          runtimeType == other.runtimeType &&
          from == other.from &&
          to == other.to;

  @override
  int get hashCode => from.hashCode ^ to.hashCode;
}

class CanMove extends CanMoveOrBeat {
  const CanMove({required super.from, required super.to});
}

class MustBeat extends CanMoveOrBeat {
  final Position at;

  const MustBeat({required super.from, required super.to, required this.at});

  @override
  bool willHighlight(Position pos) => at == pos || super.willHighlight(pos);
}
