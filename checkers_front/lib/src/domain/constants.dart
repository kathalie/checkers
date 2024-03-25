/// A size of the game board (in cells).
const boardSide = 8;

/// The amount of cells the board contains.
const boardCellCount = boardSide * boardSide;

/// The last index in a row or column.
const lastIndex = boardSide - 1;

/// How many rows are filled with checkers from each of the board sides.
const filledRows = 3;

/// The duration of the majority of the in-game animations.
const animationDuration = Duration(milliseconds: 650);

/// A url of the prolog backend service.
const prologUrl = 'http://localhost:8001';
