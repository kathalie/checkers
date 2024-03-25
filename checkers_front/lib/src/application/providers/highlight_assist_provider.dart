import 'package:riverpod_annotation/riverpod_annotation.dart';

part 'highlight_assist_provider.g.dart';

@riverpod
class HighlightAssistNotifier extends _$HighlightAssistNotifier {
  @override
  bool build() => true;

  void toggle() => state = !state;
}
