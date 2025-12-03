const std = @import("std");
const print = @import("std").debug.print;

fn read_input(fname: []const u8) ![][]u8 {
    const allocator = std.heap.page_allocator;
    var data = try std.ArrayList([]u8).initCapacity(allocator, 0);

    const file = try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    // Primary input has 100 characters per line (and then a newline), so ensure
    // we have room for at least two full lines.
    var buf: [202]u8 = undefined;
    var reader = file.reader(&buf);

    while (true) {
        const line = reader.interface.takeDelimiterExclusive('\n') catch break;
        try data.append(allocator, try allocator.dupe(u8, line));
        _ = reader.interface.discardDelimiterInclusive('\n') catch break;
    }

    return data.items;
}

fn max_joltage(comptime n_digits: u64, batteries: []const u8) u64 {
    var start_i: usize = 0;
    var joltage: u64 = 0;

    for (0..n_digits) |digit| {
        var max_x: u64 = 0;
        var max_i: usize = start_i;

        for (start_i..batteries.len - n_digits + digit + 1) |i| {
            const x = @as(u64, batteries[i] - '0');
            if (x <= max_x) continue;
            max_x = x;
            max_i = i;
        }

        joltage = joltage * 10 + max_x;
        start_i = max_i + 1;
    }

    return joltage;
}

pub fn main() !void {
    const data = try read_input("input.txt");

    var part1: u64 = 0;
    var part2: u64 = 0;
    for (data) |battery| {
        const mj1 = max_joltage(2, battery);
        const mj2 = max_joltage(12, battery);
        part1 += mj1;
        part2 += mj2;
        print("Line: {s} -> {} {}\n", .{ battery, mj1, mj2 });
    }

    print("Part 1: {}\n", .{part1});
    print("Part 2: {}\n", .{part2});
}
