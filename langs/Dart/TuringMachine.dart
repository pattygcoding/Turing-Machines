import 'dart:io';

// The canonical machine: a binary incrementer.  Reading the tape from the
// right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
// stops; if the carry runs off the tape a new leading 1 is added.
String increment(String line) {
  final chars = line.split('');
  var index = chars.length - 1;
  while (index >= 0) {
    if (chars[index] == '1') {
      chars[index] = '0';
      index -= 1;
    } else {
      chars[index] = '1';
      return chars.join();
    }
  }
  chars.insert(0, '1');
  return chars.join();
}

void main() {
  final line = (stdin.readLineSync() ?? '').replaceAll('\r', '');

  if (line.split('').any((character) => character != '0' && character != '1')) {
    print('Invalid input: expected a binary string.');
    return;
  }

  print('Initial tape: $line');
  print('Final tape: ${increment(line)}');
}
