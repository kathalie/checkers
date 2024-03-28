import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../../application/board/board.dart';
import '../../application/driver/game_driver.dart';
import '../../application/providers/game_driver_provider.dart';
import '../../domain/constants.dart';
import '../../domain/typedefs.dart';
import '../../util/coordinates_transform.dart';
import '../animation/position_tween.dart';
import 'board_cell.dart';
import 'checker_widget.dart';

class BoardWidget extends ConsumerStatefulWidget {
  final Board board;

  const BoardWidget({required this.board, super.key});

  @override
  ConsumerState<BoardWidget> createState() => _BoardWidgetState();
}

class _BoardWidgetState extends ConsumerState<BoardWidget> with SingleTickerProviderStateMixin {
  late final AnimationController _animationController = AnimationController(
    duration: pieceMovementDuration,
    vsync: this,
  );

  Movement? _animatingBetween;
  Animation<PartialPosition>? _pieceAnimation;

  @override
  void initState() {
    super.initState();
    _animationController.addStatusListener(_onAnimationStatusChange);

    ref.read(gameDriverProvider).animationCallback = _animatePieceMovement;
  }

  @override
  void dispose() {
    _animationController.dispose();
    super.dispose();
  }

  Future<void> _animatePieceMovement() {
    if (!mounted) {
      return GameDriver.noAnimationCallback();
    }

    _animationController.reset();

    _animatingBetween = widget.board.lastMove;
    final lastMove = _animatingBetween;

    if (lastMove == null) {
      return GameDriver.noAnimationCallback();
    }

    final (:from, :to) = widget.board.lastMove ?? (from: null, to: null);
    final tween = PositionTween.betweenPositions(begin: from, end: to);

    setState(() => _pieceAnimation = _animationController.drive(tween));
    _animationController.forward();

    return Future.delayed(
      _animationController.duration ?? pieceMovementDuration,
    );
  }

  void _onAnimationStatusChange(AnimationStatus status) {
    if (!mounted) {
      return;
    }

    if (status == AnimationStatus.completed) {
      setState(() => _animatingBetween = null);
      _pieceAnimation = null;
    }
  }

  @override
  Widget build(BuildContext context) {
    final (from: animatingFrom, to: animatingTo) = _animatingBetween ?? (from: null, to: null);
    final pieceAnimation = _pieceAnimation;

    return AspectRatio(
      aspectRatio: 1 / 1,
      child: LayoutBuilder(
        builder: (context, constraints) {
          final dimension = constraints.maxWidth;
          final cellDimension = dimension / boardSide;

          return Stack(
            children: [
              GridView.count(
                physics: const NeverScrollableScrollPhysics(),
                crossAxisCount: boardSide,
                children: List.generate(
                  boardCellCount,
                  (index) {
                    final pos = flatToPosition(index);

                    return BoardCell.fromBoard(
                      widget.board,
                      position: pos,
                      displayPiece: (animatingFrom != pos && animatingTo != pos),
                    );
                  },
                ),
              ),
              if (pieceAnimation != null)
                ListenableBuilder(
                  listenable: pieceAnimation,
                  builder: (context, _) => Positioned(
                    top: pieceAnimation.value.$1 * cellDimension,
                    left: pieceAnimation.value.$2 * cellDimension,
                    child: Padding(
                      padding: const EdgeInsets.all(2.5),
                      child: Consumer(
                        builder: (context, ref, child) => CheckerPiece.color(
                          ref.watch(gameDriverProvider).currentPlayer,
                          isKing: animatingTo != null
                              ? widget.board[animatingTo]?.isKing ?? false
                              : false,
                        ),
                      ),
                    ),
                  ),
                ),
            ],
          );
        },
      ),
    );
  }
}
