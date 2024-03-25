import 'dart:convert';
import 'dart:io';

import 'package:http/http.dart' as http;

import '../application/board/board.dart';
import '../application/board/board_mirror.dart';
import '../domain/constants.dart';
import '../domain/typedefs.dart';
import 'dto/board_state_dto.dart';
import 'movement_from_json.dart';

final _serviceUri = Uri.parse(prologUrl);
const _bestMoveEndpoint = '/best-move';

class PrologPlayerService {
  const PrologPlayerService();

  Future<bool> checkAvailability() async {
    final res = await http.get(_serviceUri);

    return res.statusCode == 200;
  }

  Future<Movement> fetchTurn({
    required Board board,
    required Position? lastMoved,
    required int depth,
  }) async {
    print(BoardStateDto.represent(depth: depth, board: board).toJson());

    final res = await http.post(
      _serviceUri.resolve(_bestMoveEndpoint),
      body: BoardStateDto.represent(depth: depth, board: board).toJson(),
      headers: {HttpHeaders.contentTypeHeader: ContentType.json.mimeType},
    );

    print(res.body);

    if (res.statusCode != 200) {
      print(res.bodyBytes);
      throw 'Something went wrong';
    }

    final body = jsonDecode(utf8.decode(res.bodyBytes));

    if (body is! Map) {
      print(body);
      throw 'Decoded incorrect response body';
    }

    return decodeMovement(body.cast<String, dynamic>());
  }
}
