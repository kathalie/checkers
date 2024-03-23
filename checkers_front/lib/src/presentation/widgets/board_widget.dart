import 'package:flutter/material.dart';

import '../../application/checker.dart';
import '../../domain/constants.dart';
import '../../domain/constraints/checker_color.dart';
import '../../util/coordinates_transform.dart';
import 'checker_widget.dart';

class BoardWidget extends StatelessWidget {
  const BoardWidget({super.key});

  @override
  Widget build(BuildContext context) {
    return AspectRatio(
      aspectRatio: 1 / 1,
      child: GridView.count(
        physics: const NeverScrollableScrollPhysics(),
        crossAxisCount: 8,
        children: List.generate(
          8 * 8,
          (index) => BoardCell(
            position: flatToPosition(index),
            pieceContained: index % 5 == 0
                ? null
                : Checker(
                    color: index.isEven ? CheckerColor.black : CheckerColor.white,
                    isKing: false,
                  ),
          ),
        ),
      ),
    );
  }
}

class BoardCell extends StatelessWidget {
  final (int, int) position;
  final Checker? pieceContained;

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
    final pieceContained = this.pieceContained;

    return AspectRatio(
      aspectRatio: 1 / 1,
      child: Container(
        decoration: BoxDecoration(border: border),
        child: pieceContained != null
            ? Center(
                child: CheckerWidget(
                  checker: pieceContained,
                ),
              )
            : null,
      ),
    );
  }
}
