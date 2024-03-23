import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';

import '../../application/checker.dart';
import '../../domain/constants.dart';
import '../../domain/constraints/checker_color.dart';

class CheckerWidget extends StatelessWidget {
  final Checker checker;

  const CheckerWidget({
    required this.checker,
    super.key,
  });

  @override
  Widget build(BuildContext context) {
    const outerPiecePadding = EdgeInsets.all(2);
    const innerPieceFraction = 0.7;

    final (outerColor!, innerColor!) = switch (checker.color) {
      CheckerColor.black => (Colors.grey[800], Colors.grey[700]),
      CheckerColor.white => (Colors.grey[500], Colors.grey[400]),
    };

    return AspectRatio(
      aspectRatio: 1 / 1,
      child: Padding(
        padding: outerPiecePadding,
        child: _CheckerPiece(
          outerColor: outerColor,
          innerColor: innerColor,
          innerPieceFraction: innerPieceFraction,
          isKing: checker.isKing,
        ),
      ),
    );
  }
}

class _CheckerPiece extends StatelessWidget {
  final Color outerColor;
  final Color innerColor;
  final double innerPieceFraction;
  final bool isKing;

  const _CheckerPiece({
    required this.outerColor,
    required this.innerColor,
    required this.innerPieceFraction,
    this.isKing = false,
  });

  @override
  Widget build(BuildContext context) {
    return CircleAvatar(
      backgroundColor: outerColor,
      child: LayoutBuilder(
        builder: (context, constraints) {
          final innerRadius = constraints.maxWidth / 2 * innerPieceFraction;

          return CircleAvatar(
            radius: innerRadius,
            backgroundColor: innerColor,
            child: AnimatedOpacity(
              duration: animationDuration,
              opacity: isKing ? 1 : 0,
              child: SvgPicture.asset('assets/crown_icon.svg', width: innerRadius * 1.5),
            ),
          );
        },
      ),
    );
  }
}
