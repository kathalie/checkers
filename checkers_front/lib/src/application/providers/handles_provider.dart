import 'package:riverpod_annotation/riverpod_annotation.dart';

import '../driver/player_handle.dart';

part 'handles_provider.g.dart';

@riverpod
class HandlesProviderNotifier extends _$HandlesProviderNotifier {
  @override
  (PlayerHandle, PlayerHandle)? build() => null;
}
