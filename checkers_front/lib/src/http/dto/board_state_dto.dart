import 'dart:convert';

import '../../application/board/board.dart';
import '../../application/checker.dart';
import '../../domain/constraints/checker_color.dart';
import '../../domain/typedefs.dart';

/*
  {
    "depth": 3,
    "board": [
      {
        "key": 0,
        "type": "b",
        "row": 0,
        "col": 0
      }
    ]
  }
 */

class BoardStateDto {
  final int depth;

  final List<CheckerDto> board;

  const BoardStateDto({
    required this.depth,
    required this.board,
  });

  factory BoardStateDto.represent({
    required Board board,
    required int depth,
  }) {
    final checkerDtos = [
      ...board.whites,
      ...board.blacks,
    ].map((pos) => CheckerDto.represent(board[pos]!, pos));

    return BoardStateDto(
      depth: depth,
      board: checkerDtos.toList(growable: false),
    );
  }

  String toJson() => jsonEncode({
        'depth': depth,
        'board': board.map((e) => e.toJson()).toList(growable: false),
      });
}

class CheckerDto {
  final int key;
  final String type;
  final int row;
  final int col;

  const CheckerDto({
    required this.key,
    required this.type,
    required this.row,
    required this.col,
  });

  factory CheckerDto.represent(Checker checker, Position position) {
    final type = '${checker.color == CheckerColor.white ? 'w' : 'b'}'
        '${checker.isKing ? 'q' : ''}';

    final (row, col) = position;

    return CheckerDto(
      key: checker.key,
      type: type,
      row: row,
      col: col,
    );
  }

  Map<String, dynamic> toJson() => {
        'key': key,
        'type': type,
        'row': row,
        'col': col,
      };
}
