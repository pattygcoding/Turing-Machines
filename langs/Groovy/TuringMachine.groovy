def line = System.in.newReader().readLine()
if (line == null) {
    line = ""
}
line = line.replaceAll(/[\r\n]+$/, "")

if (line.find(/[^01]/)) {
    println "Invalid input: expected a binary string."
    return
}

// The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to 0 and
// the head moves left (carry); a 0 becomes 1 and the machine halts; a blank
// becomes 1 (a new leading digit) and the machine halts.
def characters = line.toList()
int index = characters.size() - 1
while (index >= 0) {
    if (characters[index] == "1") {
        characters[index] = "0"
        index--
    } else {
        characters[index] = "1"
        break
    }
}
if (index < 0) {
    characters.add(0, "1")
}

println "Initial tape: ${line}"
println "Final tape: ${characters.join()}"
