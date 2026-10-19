<?php

$line = fgets(STDIN);
if ($line === false) {
    $line = "";
}
$line = rtrim($line, "\r\n");

if (strspn($line, "01") !== strlen($line)) {
    echo "Invalid input: expected a binary string.\n";
    exit(0);
}

// The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to 0 and
// the head moves left (carry); a 0 becomes 1 and the machine halts; a blank
// becomes 1 (a new leading digit) and the machine halts.
$tape = $line === "" ? [] : str_split($line);
$head = count($tape) - 1;

while (true) {
    $symbol = $head >= 0 ? $tape[$head] : "_";
    if ($symbol === "1") {
        $tape[$head] = "0";
        $head--;
    } elseif ($symbol === "0") {
        $tape[$head] = "1";
        break;
    } else {
        array_unshift($tape, "1");
        break;
    }
}

echo "Initial tape: " . $line . "\n";
echo "Final tape: " . implode("", $tape) . "\n";
