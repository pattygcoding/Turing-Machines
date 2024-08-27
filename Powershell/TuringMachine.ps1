$tape = @('1', '1', '0', '1')  # Initial input on the tape
$head = 0                      # Initial head position
$state = 'q0'                  # Initial state

function Determine-Action {
    param ($state, $symbol)

    switch ("$state`_$symbol") {
        'q0_1' { return 'q1', '1', 1 }
        'q0_0' { return 'q2', '1', 1 }
        'q1_1' { return 'q0', '1', 1 }
        'q1_0' { return 'q1', '1', 1 }
        'q2_1' { return 'q2', '1', 1 }
        'q2_0' { return 'HALT', '0', 0 }
        default { return 'HALT', '0', 0 }
    }
}

# Main loop to run the Turing machine
while ($true) {
    $symbol = if ($head -lt $tape.Length) { $tape[$head] } else { '0' }

    $result = Determine-Action -state $state -symbol $symbol
    $state, $writeSymbol, $direction = $result

    $tape[$head] = $writeSymbol
    $head += $direction

    # Output the current state of the tape and head position
    Write-Output "State: $state, Tape: $($tape -join ''), Head: $head"

    if ($state -eq 'HALT') { break }
}

# Output the final tape
Write-Output "Final Tape: $($tape -join '')"
