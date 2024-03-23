import '../typedefs.dart';

sealed class MoveMode {
  const MoveMode();
}

class CannotMove extends MoveMode {
  const CannotMove();
}

abstract class _CanMove extends MoveMode {
  final Position from;
  final Position to;

  const _CanMove({required this.from, required this.to});
}

class CanMove extends _CanMove {
  const CanMove({required super.from, required super.to});
}

class MustBeat extends _CanMove {
  final Position beatsAt;

  const MustBeat({required super.from, required super.to, required this.beatsAt});
}
