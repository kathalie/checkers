import '../domain/typedefs.dart';

Movement decodeMovement(Map<String, dynamic> json) {
  final from = (json['rFrom'] as int, json['cFrom'] as int);
  final to = (json['rTo'] as int, json['cTo'] as int);

  return (from: from, to: to);
}
