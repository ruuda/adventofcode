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

fn max_joltage(batteries: []const u8) u32 {
    var max_first: u32 = 0;
    var max_i: usize = 0;

    for (0..batteries.len - 1) |i| {
        const x = @as(u32, batteries[i] - '0');
        if (x <= max_first) continue;
        max_first = x;
        max_i = i;
    }

    var max_second: u32 = 0;
    for (max_i + 1..batteries.len) |i| {
        const x = @as(u32, batteries[i] - '0');
        if (x <= max_second) continue;
        max_second = x;
    }

    return max_first * 10 + max_second;
}

pub fn main() !void {
    const data = try read_input("input.txt");

    var part1: u32 = 0;
    for (data) |battery| {
        const mj = max_joltage(battery);
        part1 += mj;
        print("Line: {s} -> {}\n", .{ battery, mj });
    }

    print("Part 1: {}\n", .{part1});
}
