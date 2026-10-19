import std/strutils

# The canonical machine: a binary incrementer.  Reading the tape from the
# right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
# stops; if the carry runs off the tape a new leading 1 is added.
proc increment(line: string): string =
    var index = line.len - 1
    while index >= 0:
        if line[index] == '1':
            dec index
        else:
            return line[0 ..< index] & '1' & repeat('0', line.len - index - 1)
    '1' & repeat('0', line.len)

proc main() =
    var line = ""
    try:
        line = stdin.readLine()
    except EOFError:
        line = ""
    if line.len > 0 and line[^1] == '\r':
        line.setLen(line.len - 1)

    for character in line:
        if character != '0' and character != '1':
            echo "Invalid input: expected a binary string."
            return

    echo "Initial tape: " & line
    echo "Final tape: " & increment(line)

when isMainModule:
    main()
