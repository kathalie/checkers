import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:flutter_svg/flutter_svg.dart';

import '../../application/checker.dart';
import '../../application/providers/game_driver_provider.dart';
import '../../application/providers/highlight_notifier.dart';
import '../../domain/constants.dart';
import '../../domain/typedefs.dart';

class CheckerWidget extends ConsumerWidget {
  final Checker checker;
  final Position position;

  const CheckerWidget({
    required this.checker,
    required this.position,
    super.key,
  });

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    const outerPiecePadding = EdgeInsets.all(2);
    const innerPieceFraction = 0.7;
    const opacityWhenDragging = 0.3;

    final currentPlayer = ref.watch(gameDriverNotifierProvider).currentPlayer;

    final (outerColor, innerColor) = checker.color.displayColors;

    final child = Padding(
      padding: outerPiecePadding,
      child: _CheckerPiece(
        outerColor: outerColor,
        innerColor: innerColor,
        innerPieceFraction: innerPieceFraction,
        isKing: checker.isKing,
      ),
    );

    if (checker.color == currentPlayer) {
      return Draggable(
        onDragStarted: () {
          ref.read(highlightNotifierProvider.notifier).updateHighlightFor(position);
        },
        onDragEnd: (details) {
          ref.read(highlightNotifierProvider.notifier).reset();
        },
        feedback: child,
        childWhenDragging: Opacity(
          opacity: opacityWhenDragging,
          child: child,
        ),
        child: child,
      );
    }

    return child;
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
