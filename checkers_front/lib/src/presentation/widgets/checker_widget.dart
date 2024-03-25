import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:flutter_svg/flutter_svg.dart';

import '../../application/checker.dart';
import '../../application/driver/handles/real_player_handle.dart';
import '../../application/providers/game_driver_provider.dart';
import '../../application/providers/possible_moves_notifier.dart';
import '../../domain/constants.dart';
import '../../domain/constraints/checker_color.dart';
import '../../domain/typedefs.dart';

const _innerPieceFraction = 0.7;

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
    const opacityWhenDragging = 0.3;

    final gameDriver = ref.watch(gameDriverNotifierProvider);
    final currentHandle = gameDriver.currentHandle;
    final board = gameDriver.board;

    final child = Padding(
      padding: outerPiecePadding,
      child: CheckerPiece.checker(checker),
    );

    if (currentHandle is RealPlayerHandle &&
        checker.color == currentHandle.color &&
        (board.mustBeatAt(position).isNotEmpty ||
            !board.playerMustBeat(gameDriver.currentPlayer))) {
      return Draggable<Checker>(
        onDragStarted: () {
          ref.read(possibleMovesNotifierProvider.notifier).updateMovesFor(position);
        },
        onDragEnd: (details) {
          ref.read(possibleMovesNotifierProvider.notifier).reset();
        },
        data: checker,
        maxSimultaneousDrags: 1,
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

class CheckerPiece extends StatelessWidget {
  final Color outerColor;
  final Color innerColor;
  final bool isKing;

  const CheckerPiece({
    required this.outerColor,
    required this.innerColor,
    this.isKing = false,
    super.key,
  });

  factory CheckerPiece.checker(Checker checker) => CheckerPiece.color(
        checker.color,
        isKing: checker.isKing,
      );

  factory CheckerPiece.color(CheckerColor color, {bool isKing = false}) {
    final (outerColor, innerColor) = color.displayColors;

    return CheckerPiece(
      outerColor: outerColor,
      innerColor: innerColor,
      isKing: isKing,
    );
  }

  @override
  Widget build(BuildContext context) {
    return CircleAvatar(
      backgroundColor: outerColor,
      child: LayoutBuilder(
        builder: (context, constraints) {
          final innerRadius = constraints.maxWidth / 2 * _innerPieceFraction;

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
