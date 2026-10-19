use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    let mut raw = String::new();
    let _ = stdin.lock().read_line(&mut raw);
    let line = raw.trim_end_matches(['\r', '\n']).to_string();

    if line.chars().any(|character| character != '0' && character != '1') {
        println!("Invalid input: expected a binary string.");
        return;
    }

    // The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to
    // 0 and the head moves left (carry); a 0 becomes 1 and the machine halts;
    // a blank becomes 1 (a new leading digit) and the machine halts.
    let mut tape: Vec<char> = line.chars().collect();
    let mut head: isize = tape.len() as isize - 1;
    loop {
        let symbol = if head >= 0 { tape[head as usize] } else { '_' };
        if symbol == '1' {
            tape[head as usize] = '0';
            head -= 1;
        } else if symbol == '0' {
            tape[head as usize] = '1';
            break;
        } else {
            tape.insert(0, '1');
            break;
        }
    }

    println!("Initial tape: {}", line);
    println!("Final tape: {}", tape.into_iter().collect::<String>());
}
