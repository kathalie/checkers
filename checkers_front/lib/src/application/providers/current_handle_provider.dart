import 'package:riverpod_annotation/riverpod_annotation.dart';

import 'game_driver_provider.dart';

final currentHandleProvider = Provider((ref) {
  return ref.watch(gameDriverProvider).currentHandle;
});
