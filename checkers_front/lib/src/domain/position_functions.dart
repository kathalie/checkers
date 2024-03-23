import 'typedefs.dart';

/// Returns a [Vector] between the [from] and the [to] positions.
Vector vector({required Position from, required Position to}) => (to.$1 - from.$1, to.$2 - from.$2);

/// Whether the given [Vector] changes both coordinates equally.
bool isDiagonal(Vector vec) => vec.$1.abs() == vec.$2.abs();

/// Returns the length of the given [Vector] in cells given the vector is diagonal.
int diagonalVectorLength(Vector vec) =>
    !isDiagonal(vec) ? throw StateError('Vector $vec is not diagonal') : vec.$1.abs();

/// Returns the distance between the two given positions given their vector is diagonal.
int diagonalDistanceBetween(Position pos1, Position pos2) =>
    diagonalVectorLength(vector(from: pos1, to: pos2));

/// Returns a [Vector] pointing at the same direction as the given vector with both coordinates
/// between -1 and 1  inclusive.
Vector directionOf(Vector vec) => (vec.$1.sign, vec.$2.sign);

/// Adds the given [Vector] to the given [Position].
Position add({required Vector vector, required Position toPosition}) =>
    (toPosition.$1 + vector.$1, toPosition.$2 + vector.$2);

/// Multiplies the given [Vector] by the given [modifier].
Vector multiply(Vector vec, int modifier) => (vec.$1 * modifier, vec.$2 * modifier);

/// Whether the given [Position] is a black cell, where the checkers can be placed.
bool isBlackCell(Position pos) => pos.$1.isOdd ? pos.$2.isEven : pos.$2.isOdd;
