import 'dart:ui';

import 'package:flutter/animation.dart';

import '../../domain/typedefs.dart';

typedef PartialPosition = (double, double);

class PositionTween extends Tween<PartialPosition> {
  PositionTween({super.begin, super.end});

  factory PositionTween.betweenPositions({Position? begin, Position? end}) {
    final partialBegin = begin != null ? (begin.$1.toDouble(), begin.$2.toDouble()) : null;
    final partialEnd = end != null ? (end.$1.toDouble(), end.$2.toDouble()) : null;

    return PositionTween(begin: partialBegin, end: partialEnd);
  }

  @override
  PartialPosition lerp(double t) {
    final begin = this.begin;
    final end = this.end;

    if (begin == null || end == null) {
      return (0, 0);
    }

    return (
      lerpDouble(begin.$1.toDouble(), end.$1.toDouble(), t) ?? 0,
      lerpDouble(begin.$2.toDouble(), end.$2.toDouble(), t) ?? 0,
    );
  }
}
