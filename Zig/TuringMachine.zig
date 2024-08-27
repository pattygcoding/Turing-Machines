const std = @import("std");

fn determineAction(state: []const u8, symbol: u8) []const u8 {
    switch (state) {
        "q0" => {
            switch (symbol) {
                '1' => return "q1 1 1",
                '0' => return "q2 1 1",
                else => return "HALT 0 0",
            }
        },
        "q1" => {
            switch (symbol) {
                '1' => return "q0 1 1",
                '0' => return "q1 1 1",
                else => return "HALT 0 0",
            }
        },
        "q2" => {
            switch (symbol) {
                '1' => return "q2 1 1",
                '0' => return "HALT 0 0",
                else => return "HALT 0 0",
            }
        },
        else => return "HALT 0 0",
    }
}

pub fn main() void {
    var tape: [10]u8 = "1101     ".[0..10];
    var head: usize = 0;
    var state: []const u8 = "q0";

    while (state != "HALT") {
        const symbol = tape[head];
        const action = determineAction(state, symbol);
        state = action[0..2];
        tape[head] = action[3];
        head += @as(usize, action[5] - '0');

        std.debug.print("State: {}, Tape: {}, Head: {}\n", .{state, tape[0..4], head});
    }

    std.debug.print("Final Tape: {}\n", .{tape[0..4]});
}
