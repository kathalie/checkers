import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../../application/providers/prolog_availability_provider.dart';

class PrologStatus extends ConsumerWidget {
  const PrologStatus({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final value = ref.watch(prologAvailabilityProvider);

    switch (value) {
      case AsyncData(value: var isAvailable as bool?) || AsyncError(value: var isAvailable):
        isAvailable ??= false;

        return Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            Text(
              'Prolog is ${isAvailable ? 'available' : 'unavailable'}',
              style: TextStyle(
                color: isAvailable ? Colors.green : Theme.of(context).colorScheme.error,
              ),
            ),
            if (!isAvailable)
              ElevatedButton(
                onPressed: () => ref.invalidate(prologAvailabilityProvider),
                child: const Text(
                  'REFRESH',
                  style: TextStyle(fontWeight: FontWeight.bold),
                ),
              ),
          ],
        );
      default:
        return const SizedBox.square(
          dimension: 16,
          child: CircularProgressIndicator(),
        );
    }
  }
}
