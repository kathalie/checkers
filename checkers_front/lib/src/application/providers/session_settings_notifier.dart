import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';

import '../../domain/constraints/checker_color.dart';

part 'session_settings_notifier.g.dart';

@riverpod
class SessionSettingsNotifier extends _$SessionSettingsNotifier {
  @override
  SessionSettings build() {
    ref.keepAlive();

    return SessionSettings(
      movesHighlight: false,
      aiDifficulties: {
        CheckerColor.white: 4,
        CheckerColor.black: 4,
      }.lock,
    );
  }

  void toggleHighlight() => state = state.copyWith(
        movesHighlight: !state.movesHighlight,
      );

  void setAiDifficulty(CheckerColor color, int depth) {
    final Map<CheckerColor, int> newDifficulties = <CheckerColor, int>{
      ...state.aiDifficulties.unlock,
    }..[color] = depth;

    state = state.copyWith(aiDifficulties: newDifficulties.lock);
  }
}

class SessionSettings {
  final bool movesHighlight;

  final IMap<CheckerColor, int> aiDifficulties;

  const SessionSettings({
    required this.movesHighlight,
    required this.aiDifficulties,
  });

  SessionSettings copyWith({
    bool? movesHighlight,
    IMap<CheckerColor, int>? aiDifficulties,
  }) =>
      SessionSettings(
        movesHighlight: movesHighlight ?? this.movesHighlight,
        aiDifficulties: aiDifficulties ?? this.aiDifficulties,
      );

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is SessionSettings &&
          runtimeType == other.runtimeType &&
          movesHighlight == other.movesHighlight &&
          aiDifficulties == other.aiDifficulties;

  @override
  int get hashCode => movesHighlight.hashCode ^ aiDifficulties.hashCode;
}
