/// A record that holds (row, column) describing a position on the game board.
typedef Position = (int, int);

/// A record defining the change of the coordinates between two [Position]s.
typedef Vector = Position;

typedef Movement = ({Position from, Position to});
