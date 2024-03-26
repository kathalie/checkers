import 'package:http/http.dart' as http;
import 'package:riverpod_annotation/riverpod_annotation.dart';

import '../../http/http_values.dart';

part 'prolog_availability_provider.g.dart';

@riverpod
Future<bool> prologAvailability(PrologAvailabilityRef ref) => http
    .get(
      serviceUri.resolve(heartbeatEndpoint),
    )
    .then((res) => res.statusCode == 200)
    .onError((_, __) => false);
