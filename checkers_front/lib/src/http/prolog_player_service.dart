import 'dart:convert';
import 'dart:io';

import 'package:http/http.dart' as http;

import '../application/board/board.dart';
import '../domain/typedefs.dart';
import 'dto/board_state_dto.dart';
import 'http_values.dart';
import 'movement_from_json.dart';

class PrologPlayerService {
  const PrologPlayerService();

  Future<Movement> fetchTurn({
    required Board board,
    required Position? lastMoved,
    required int depth,
  }) async {
    print(BoardStateDto.represent(depth: depth, board: board).toJson());

    final res = await http.post(
      serviceUri.resolve(bestMoveEndpoint),
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
