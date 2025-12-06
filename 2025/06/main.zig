const std = @import("std");
const print = @import("std").debug.print;

const Input = struct {
    /// Values as flattened array, the row size matches `ops`.
    values: []u64,
    ops: []u8,

    // For part 2 we preserve the raw rows.
    raw: [][]u8,

    pub fn at(self: Input, x: usize, y: usize) u64 {
        return self.values[y * self.ops.len + x];
    }
};

fn readInput(fname: []const u8) !Input {
    const allocator = std.heap.page_allocator;
    var values = try std.ArrayList(u64).initCapacity(allocator, 12);
    var ops = try std.ArrayList(u8).initCapacity(allocator, 4);
    var raw = try std.ArrayList([]u8).initCapacity(allocator, 3);

    const file = try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    // We go line by line, the buffer needs to hold a line until delimiter.
    var buf: [1024 * 16]u8 = undefined;
    var reader = file.reader(&buf);

    while (true) {
        const line = reader.interface.takeDelimiterExclusive('\n') catch break;
        try raw.append(allocator, try allocator.dupe(u8, line));
        var it = std.mem.splitScalar(u8, line, ' ');
        while (true) {
            const part = it.next() orelse break;
            if (part.len == 0) continue;

            // All mathematical operators order before digits in ascii.
            if (part.len == 1 and part[0] < '0') {
                try ops.append(allocator, part[0]);
                continue;
            }

            // If it was not an operator then it should be a value.
            const n = try std.fmt.parseInt(u64, part, 10);
            try values.append(allocator, n);
        }
        _ = reader.interface.discardDelimiterInclusive('\n') catch break;
    }

    return Input{ .values = values.items, .ops = ops.items, .raw = raw.items };
}

fn evalPart1(input: Input) !u64 {
    var result: u64 = 0;
    const h = input.values.len / input.ops.len;
    for (0..input.ops.len) |x| {
        var acc = input.at(x, 0);
        for (1..h) |y| {
            const rhs = input.at(x, y);
            acc = switch (input.ops[x]) {
                '+' => acc + rhs,
                '*' => acc * rhs,
                else => return error.InvalidInput,
            };
        }
        result += acc;
    }
    return result;
}

fn evalPart2(input: Input) !u64 {
    const h = input.raw.len - 1;
    var w = input.raw[0].len;
    for (input.raw) |line| {
        if (line.len > w) w = line.len;
    }

    var result: u64 = 0;
    var op: u8 = ' ';
    var acc: u64 = 0;

    // We'll walk the input left to right, column by column. Every time we see
    // a new operator, we know it's the start of a new problem, and we can flush
    // the problem's accumulator into the global accumulator (`result`).
    for (0..w) |x| {
        if (x < input.raw[h].len and input.raw[h][x] != ' ') {
            op = input.raw[h][x];
            acc = switch (op) {
                '+' => 0,
                '*' => 1,
                else => return error.InvalidInput,
            };
        }

        var num: u64 = 0;
        var empty = true;
        for (0..h) |y| {
            if (x >= input.raw[y].len) continue;
            const ch = input.raw[y][x];
            if (ch == ' ') continue;
            const d: u64 = @intCast(ch - '0');
            num = num * 10 + d;
            empty = false;
        }

        if (empty) {
            result += acc;
            continue;
        }

        switch (op) {
            '+' => acc = acc + num,
            '*' => acc = acc * num,
            else => return error.InvalidInput,
        }
    }

    // Don't forget to flush the final accumulator.
    return result + acc;
}

pub fn main() !void {
    const data = try readInput("input.txt");
    const part1 = try evalPart1(data);
    print("Part 1: {}\n", .{part1});
    const part2 = try evalPart2(data);
    print("Part 2: {}\n", .{part2});
}
