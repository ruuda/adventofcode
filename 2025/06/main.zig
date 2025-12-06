const std = @import("std");
const print = @import("std").debug.print;

const Input = struct {
    /// Values as flattened array, the row size matches `ops`.
    values: []u64,
    ops: []u8,

    pub fn at(self: Input, x: usize, y: usize) u64 {
        return self.values[y * self.ops.len + x];
    }
};

fn readInput(fname: []const u8) !Input {
    const allocator = std.heap.page_allocator;
    var values = try std.ArrayList(u64).initCapacity(allocator, 12);
    var ops = try std.ArrayList(u8).initCapacity(allocator, 4);

    const file = try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    // We go line by line, the buffer needs to hold a line until delimiter.
    var buf: [1024 * 16]u8 = undefined;
    var reader = file.reader(&buf);

    while (true) {
        const line = reader.interface.takeDelimiterExclusive('\n') catch break;
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

    return Input{ .values = values.items, .ops = ops.items };
}

fn eval(input: Input) !u64 {
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

pub fn main() !void {
    const data = try readInput("input.txt");
    const part1 = try eval(data);
    print("Part 1: {}\n", .{part1});
}
