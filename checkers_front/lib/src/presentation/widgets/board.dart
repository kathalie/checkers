import 'package:flutter/material.dart';

import '../../domain/constants.dart';
import '../../domain/piece.dart';
import '../../util/coordinates_transform.dart';

class Board extends StatelessWidget {
  const Board({super.key});

  @override
  Widget build(BuildContext context) {
    return AspectRatio(
      aspectRatio: 1 / 1,
      child: GridView.count(
        crossAxisCount: 8,
        children: List.generate(
          8 * 8,
          (index) => BoardCell(position: flatToPosition(index)),
        ),
      ),
    );
  }
}

class BoardCell extends StatelessWidget {
  final (int, int) position;
  final Piece? pieceContained;

  const BoardCell({
    required this.position,
    this.pieceContained,
    super.key,
  });

  Border get border {
    const side = BorderSide();
    const noSide = BorderSide.none;

    final (row, col) = position;

    return Border(
      top: row.isEven ? side : noSide,
      right: col.isEven || col == lastIndex ? side : noSide,
      bottom: row.isEven || row == lastIndex ? side : noSide,
      left: col.isEven ? side : noSide,
    );
  }

  @override
  Widget build(BuildContext context) {
    return AspectRatio(
      aspectRatio: 1 / 1,
      child: Container(
        decoration: BoxDecoration(border: border),
      ),
    );
  }
}
