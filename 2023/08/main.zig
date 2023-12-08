const std = @import("std");

const Loc = [3]u8;

const Node = struct {
    left: Loc,
    right: Loc,
};

const Input = struct {
    nav: []u8,
    nodes: std.AutoHashMap(Loc, Node),
};

fn read_input(fname: []const u8) !Input {
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;
    var result = std.AutoHashMap(Loc, Node).init(allocator);

    const file = try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    const in_stream = buf_reader.reader();
    var buf: [2048]u8 = undefined;

    const nav = (try in_stream.readUntilDelimiterOrEof(&buf, '\n')).?;
    const nav_owned = try allocator.dupe(u8, nav);

    // Skip over the blank line that follows the nav directions.
    _ = try in_stream.readUntilDelimiterOrEof(&buf, '\n');

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        // Get the identifiers from a line of the form. "AAA = (BBB, CCC)"
        const src = line[0..3].*;
        const left = line[7..10].*;
        const right = line[12..15].*;
        try result.put(src, Node{ .left = left, .right = right });
        try stdout.print("Node {s} -> ({s}, {s}).\n", .{ src, left, right });
    }

    return Input{ .nav = nav_owned, .nodes = result };
}

pub fn part1(fname: []const u8) !void {
    const stdout = std.io.getStdOut().writer();
    const input = try read_input(fname);

    var loc: [3]u8 = "AAA".*;
    var cursor: usize = 0;
    var steps: usize = 0;

    while (!std.mem.eql(u8, &loc, "ZZZ")) {
        const node = input.nodes.get(loc).?;
        if (input.nav[cursor] == 'L') {
            loc = node.left;
        } else {
            loc = node.right;
        }
        steps = steps + 1;
        cursor = (cursor + 1) % input.nav.len;
        try stdout.print("Step {d} to {s}.\n", .{ steps, loc });
    }
}

pub fn part2(fname: []const u8) !void {
    const stdout = std.io.getStdOut().writer();
    const input = try read_input(fname);
    const allocator = std.heap.page_allocator;

    var keys = input.nodes.keyIterator();
    while (keys.next()) |k| {
        if (k[2] != 'A') continue;

        var path = std.ArrayList([3]u8).init(allocator);
        var cursor: usize = 0;
        var loc: [3]u8 = k.*;
        try path.append(loc);

        while (true) {
            const node = input.nodes.get(loc).?;
            if (input.nav[cursor] == 'L') {
                loc = node.left;
            } else {
                loc = node.right;
            }
            cursor = (cursor + 1) % input.nav.len;

            var found_cycle = false;
            for (path.items, 0..) |prev, i| {
                if (std.mem.eql(u8, &loc, &prev) and cursor == i % input.nav.len) {
                    found_cycle = true;
                    break;
                }
            }

            if (found_cycle) break;

            try path.append(loc);
        }

        try stdout.print("Key {s} has a period of {d}.\n", .{ k, path.items.len });
    }
}

pub fn main() !void {
    // try part1("example1.txt");
    try part2("example3.txt");
    try part2("input.txt");
}
