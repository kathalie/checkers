import '../application/driver/player_handle.dart';

/// A record that holds (row, column) describing a position on the game board.
typedef Position = (int, int);

/// A record defining the change of the coordinates between two [Position]s.
typedef Vector = Position;

/// A record defining a movement of a checker between two positions.
typedef Movement = ({Position from, Position to});

/// A record holding the [PlayerHandle]s for both players.
typedef Handles = ({PlayerHandle white, PlayerHandle black});
