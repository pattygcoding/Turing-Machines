const std = @import("std");

pub fn main() !void {
    var threaded = std.Io.Threaded.init(std.heap.page_allocator, .{});
    defer threaded.deinit();
    const io = threaded.io();

    const stdin = std.Io.File.stdin();
    var input: [4096]u8 = undefined;
    const read = stdin.readStreaming(io, &.{&input}) catch 0;

    var line_end: usize = 0;
    while (line_end < read and input[line_end] != '\n') : (line_end += 1) {}
    var line_len = line_end;
    if (line_len > 0 and input[line_len - 1] == '\r') {
        line_len -= 1;
    }
    const line = input[0..line_len];

    for (line) |character| {
        if (character != '0' and character != '1') {
            try std.Io.File.stdout().writeStreamingAll(
                io,
                "Invalid input: expected a binary string.\n",
            );
            return;
        }
    }

    // The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to
    // 0 and the head moves left (carry); a 0 becomes 1 and the machine halts;
    // a blank becomes 1 (a new leading digit) and the machine halts.
    var tape: [4096]u8 = undefined;
    @memcpy(tape[0..line_len], line);
    var length = line_len;
    var head: isize = @as(isize, @intCast(length)) - 1;
    while (head >= 0) {
        const index: usize = @intCast(head);
        if (tape[index] == '1') {
            tape[index] = '0';
            head -= 1;
        } else {
            tape[index] = '1';
            break;
        }
    }
    if (head < 0) {
        var index = length;
        while (index > 0) : (index -= 1) {
            tape[index] = tape[index - 1];
        }
        tape[0] = '1';
        length += 1;
    }

    var output: [8192]u8 = undefined;
    const text = std.fmt.bufPrint(
        &output,
        "Initial tape: {s}\nFinal tape: {s}\n",
        .{ line, tape[0..length] },
    ) catch return error.OutOfMemory;
    try std.Io.File.stdout().writeStreamingAll(io, text);
}
