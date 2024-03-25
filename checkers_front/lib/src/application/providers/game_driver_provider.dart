import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../board/board_generator.dart';
import '../board/board_impl.dart';
import '../driver/game_driver.dart';
import 'handles_notifier.dart';

final gameDriverProvider = AutoDisposeChangeNotifierProvider((ref) {
  final (:white, :black) = ref.watch(handlesNotifierProvider);

  final gameDriver = GameDriver(
    BoardImpl(generateInitialBoard()),
    whiteHandle: white,
    blackHandle: black,
  );

  return gameDriver;
});
