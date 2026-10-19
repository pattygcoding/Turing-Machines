use strict;
use warnings;

my $line = <STDIN>;
$line = "" unless defined $line;
$line =~ s/[\r\n]+\z//;

if ($line =~ /[^01]/) {
    print "Invalid input: expected a binary string.\n";
    exit 0;
}

# The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to 0 and
# the head moves left (carry); a 0 becomes 1 and the machine halts; a blank
# becomes 1 (a new leading digit) and the machine halts.
my @tape = split //, $line;
my $head = $#tape;

while (1) {
    my $symbol = $head >= 0 ? $tape[$head] : "_";
    if ($symbol eq "1") {
        $tape[$head] = "0";
        $head -= 1;
    } elsif ($symbol eq "0") {
        $tape[$head] = "1";
        last;
    } else {
        unshift @tape, "1";
        last;
    }
}

print "Initial tape: $line\n";
print "Final tape: ", join("", @tape), "\n";
