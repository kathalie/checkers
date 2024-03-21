import 'package:checkers_front/src/util/coordinates_transform.dart';
import 'package:flutter_test/flutter_test.dart';

/*
 *   0  1  2  3  4  5  6  7
 *   8  9  10 11 12 13 14 15
 *   16 17 18 19 20 21 22 23
 *   24 25 26 27 28 29 30 31
 *   32 33 34 35 36 37 38 39
 *   40 41 42 43 44 45 46 47
 *   48 49 50 51 52 53 54 55
 *   56 57 58 59 60 61 62 63
 */

void main() {
  group('Coordinate transform tests', () {
    test('Index is transformed into Position correctly', () {
      expect(flatToPosition(0), equals((0, 0)));
      expect(flatToPosition(4), equals((0, 4)));
      expect(flatToPosition(25), equals((3, 1)));
      expect(flatToPosition(53), equals((6, 5)));
      expect(flatToPosition(63), equals((7, 7)));
      expect(flatToPosition(15), equals((1, 7)));
      expect(flatToPosition(56), equals((7, 0)));
    });

    test('Position is transformed into index correctly', () {
      expect(positionToFlat((0, 0)), equals(0));
      expect(positionToFlat((0, 4)), equals(4));
      expect(positionToFlat((3, 1)), equals(25));
      expect(positionToFlat((6, 5)), equals(53));
      expect(positionToFlat((7, 7)), equals(63));
      expect(positionToFlat((1, 7)), equals(15));
      expect(positionToFlat((7, 0)), equals(56));
    });

    test('Flat to Position to Flat returns identical index', () {
      expect(positionToFlat(flatToPosition(0)), equals(0));
      expect(positionToFlat(flatToPosition(4)), equals(4));
      expect(positionToFlat(flatToPosition(25)), equals(25));
      expect(positionToFlat(flatToPosition(53)), equals(53));
      expect(positionToFlat(flatToPosition(63)), equals(63));
      expect(positionToFlat(flatToPosition(15)), equals(15));
      expect(positionToFlat(flatToPosition(56)), equals(56));
    });

    test('Position to Flat to Position returns identical Position', () {
      expect(flatToPosition(positionToFlat((0, 0))), equals((0, 0)));
      expect(flatToPosition(positionToFlat((0, 4))), equals((0, 4)));
      expect(flatToPosition(positionToFlat((3, 1))), equals((3, 1)));
      expect(flatToPosition(positionToFlat((6, 5))), equals((6, 5)));
      expect(flatToPosition(positionToFlat((7, 7))), equals((7, 7)));
      expect(flatToPosition(positionToFlat((1, 7))), equals((1, 7)));
      expect(flatToPosition(positionToFlat((7, 0))), equals((7, 0)));
    });
  });
}
