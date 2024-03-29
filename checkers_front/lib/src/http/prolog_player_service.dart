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
    final res = await http.post(
      serviceUri.resolve(bestMoveEndpoint),
      body: BoardStateDto.represent(depth: depth, board: board).toJson(),
      headers: {HttpHeaders.contentTypeHeader: ContentType.json.mimeType},
    );

    if (res.statusCode != 200) {
      print(res.body);
      throw 'Server replied with the status code of ${res.statusCode}';
    }

    final body = jsonDecode(utf8.decode(res.bodyBytes));

    if (body is! Map) {
      print(body);
      throw 'Decoded incorrect response body';
    }

    return decodeMovement(body.cast<String, dynamic>());
  }
}
