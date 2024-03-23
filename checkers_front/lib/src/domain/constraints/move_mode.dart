import '../typedefs.dart';

sealed class MoveMode {
  const MoveMode();
}

class CannotMove extends MoveMode {
  const CannotMove();
}

class CanMove extends MoveMode {
  const CanMove();
}

class MustBeat extends MoveMode {
  final Position at;

  const MustBeat({required this.at});
}
