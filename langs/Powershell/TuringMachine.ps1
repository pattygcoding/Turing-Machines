$ErrorActionPreference = "Stop"

$line = [Console]::In.ReadLine()
if ($null -eq $line) { $line = "" }

if ($line -notmatch '^[01]*$') {
    Write-Output "Invalid input: expected a binary string."
    exit 0
}

# The tape starts as the input bits; the head starts on the last bit.
$tape = New-Object System.Collections.Generic.List[char]
foreach ($character in $line.ToCharArray()) { $tape.Add($character) }
$head = $tape.Count - 1

# The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to 0 and
# the head moves left (carry); a 0 becomes 1 and the machine halts; a blank
# becomes 1 (a new leading digit) and the machine halts.
while ($true) {
    $symbol = if ($head -ge 0) { [string]$tape[$head] } else { "_" }
    if ($symbol -eq "1") {
        $tape[$head] = [char]"0"
        $head -= 1
    } elseif ($symbol -eq "0") {
        $tape[$head] = [char]"1"
        break
    } else {
        $tape.Insert(0, [char]"1")
        break
    }
}

Write-Output ("Initial tape: " + $line)
Write-Output ("Final tape: " + (-join $tape))
