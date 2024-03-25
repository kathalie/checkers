import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../board/board_generator.dart';
import '../board/board_impl.dart';
import '../driver/game_driver.dart';
import 'handles_notifier.dart';

final gameDriverProvider = ChangeNotifierProvider((ref) {
  final (:white, :black) = ref.read(handlesNotifierProvider);
  print('white $white black $black');

  final gameDriver = GameDriver(
    BoardImpl(generateInitialBoard()),
    whiteHandle: white,
    blackHandle: black,
  );

  return gameDriver;
});
