#!/usr/bin/perl
use strict;
use warnings;

# Define the tape (with some initial input)
my @tape = split //, "1101";  # Initial input
my $head = 0;                 # Initial head position
my $state = 'q0';             # Initial state

# Define the transition table
my %transitions = (
    'q0_1' => ['q1', '1', 1],  # On state q0 and reading 1 -> move to q1, write 1, move right
    'q0_0' => ['q2', '1', 1],  # On state q0 and reading 0 -> move to q2, write 1, move right
    'q1_1' => ['q0', '1', 1],  # On state q1 and reading 1 -> move to q0, write 1, move right
    'q1_0' => ['q1', '1', 1],  # On state q1 and reading 0 -> stay in q1, write 1, move right
    'q2_1' => ['q2', '1', 1],  # On state q2 and reading 1 -> stay in q2, write 1, move right
    'q2_0' => ['HALT', '0', 0],# On state q2 and reading 0 -> halt
);

# Simulate the Turing machine
while (1) {
    # Get the current symbol on the tape
    my $symbol = $tape[$head] // '0';  # Default to '0' if beyond tape bounds

    # Determine the action based on the current state and symbol
    my $action = $transitions{"${state}_${symbol}"};

    # If no action is defined, halt the machine
    last unless $action;

    # Apply the action
    $state = $action->[0];      # Move to the new state
    $tape[$head] = $action->[1];# Write the new symbol
    $head += $action->[2];      # Move the head (left: -1, right: +1)

    # Output the current state of the tape and head position
    print "State: $state, Tape: ", join('', @tape), ", Head: $head\n";

    # Halt if we reach the HALT state
    last if $state eq 'HALT';
}

print "Final Tape: ", join('', @tape), "\n";
