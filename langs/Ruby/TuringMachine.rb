line = gets
line = "" if line.nil?
line = line.chomp

if line.match?(/[^01]/)
  puts "Invalid input: expected a binary string."
  exit 0
end

# The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to 0 and
# the head moves left (carry); a 0 becomes 1 and the machine halts; a blank
# becomes 1 (a new leading digit) and the machine halts.
tape = line.chars
head = tape.length - 1

loop do
  symbol = head >= 0 ? tape[head] : "_"
  if symbol == "1"
    tape[head] = "0"
    head -= 1
  elsif symbol == "0"
    tape[head] = "1"
    break
  else
    tape.unshift("1")
    break
  end
end

puts "Initial tape: #{line}"
puts "Final tape: #{tape.join}"
