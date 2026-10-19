import os

// The canonical machine: a binary incrementer.  Reading the tape from the
// right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
// stops; if the carry runs off the tape a new leading 1 is added.
fn increment(line string) string {
	mut index := line.len - 1
	for index >= 0 {
		if line[index] == `1` {
			index--
		} else {
			return line[..index] + '1' + '0'.repeat(line.len - index - 1)
		}
	}
	return '1' + '0'.repeat(line.len)
}

fn main() {
	line := os.get_line().trim_right('\r\n')
	for character in line {
		if character != `0` && character != `1` {
			println('Invalid input: expected a binary string.')
			return
		}
	}
	println('Initial tape: ' + line)
	println('Final tape: ' + increment(line))
}
