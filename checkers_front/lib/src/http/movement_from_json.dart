import '../domain/typedefs.dart';

Movement decodeMovement(Map<String, dynamic> json) {
  final from = (json['xFrom'] as int, json['yFrom'] as int);
  final to = (json['xTo'] as int, json['yTo'] as int);

  return (from: from, to: to);
}
