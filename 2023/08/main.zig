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
    var start_indices = std.ArrayList(usize).init(allocator);
    var all_jumps = std.ArrayList(std.ArrayList(usize)).init(allocator);

    // Loop over all the keys that are starting positions.
    var keys = input.nodes.keyIterator();
    while (keys.next()) |k| {
        if (k[2] != 'A') continue;

        // For this key, we are going to build the path of locations that it
        // visits. Eventually this path repeats, and as soon as we have a cycle,
        // we stop looking further.
        var start_index: usize = 0;
        var path = std.ArrayList([3]u8).init(allocator);
        var jumps = std.ArrayList(usize).init(allocator);
        var cursor: usize = 0;
        var loc: [3]u8 = k.*;
        try path.append(loc);
        try jumps.append(0);

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
                    start_index = i;
                    found_cycle = true;
                    break;
                }
            }

            if (found_cycle) {
                try start_indices.append(start_index);
                break;
            }

            try path.append(loc);
            try jumps.append(0);
        }

        // Now that we have the cycle, identify all end node locations in it,
        // and how far ahead we need to jump to get there.
        for (0..path.items.len) |i| {
            if (i > 0) {
                // If for the previous place we needed to jump j ahead to an end
                // node, then now we need to jump j-1 ahead.
                const prev = jumps.items[i - 1];
                if (prev > 1) {
                    jumps.items[i] = prev - 1;
                    continue;
                }
            }

            for (1..path.items.len) |j| {
                var jmod = i + j;
                if (jmod >= path.items.len) jmod = jmod - path.items.len + start_index;
                if (path.items[j][2] != 'Z') continue;
                jumps.items[i] = @truncate(j);
                break;
            }
        }

        try all_jumps.append(jumps);
        try stdout.print("Key {s} has a period of {d}.\n", .{ k, path.items.len });
    }

    var steps_since_print: usize = 0;
    var steps: usize = 0;
    var cursors = std.ArrayList(usize).init(allocator);
    for (all_jumps.items) |_| {
        try cursors.append(0);
    }

    while (true) {
        if (steps_since_print > 10 * 1000 * 1000 * 1000) {
            steps_since_print = 0;
            try stdout.print("Step {d}.\n", .{steps});
        }
        // If the jump just before the cursor is 1 for all cursors, then the
        // cursors are at an end position.
        if (steps > 0) {
            var is_end = true;
            for (cursors.items, all_jumps.items) |cursor, jumps| {
                if (jumps.items[cursor - 1] > 1) {
                    is_end = false;
                    break;
                }
            }
            if (is_end) {
                try stdout.print("Took {d} steps.\n", .{steps});
                break;
            }
        }

        // Determine how far to jump ahead. Take the maximum jump among all the
        // cursors. If all the cursors want to jump by the same amount, then
        // it's the jump to the end.
        var jump = all_jumps.items[0].items[cursors.items[0]];
        for (cursors.items, all_jumps.items, 0..) |cursor, jumps, i| {
            const j = jumps.items[cursor];
            // try stdout.print("  {d}: {d} +{d}.\n", .{ i, cursor, j });
            if (j == 0) unreachable;
            if (i == 0) continue;
            if (j == jump) continue;
            if (j > jump) jump = j;
        }

        steps += jump;
        steps_since_print += jump;

        for (cursors.items, start_indices.items, all_jumps.items) |*cursor, start_index, jumps| {
            cursor.* = cursor.* + jump;
            while (cursor.* >= jumps.items.len) {
                cursor.* -= jumps.items.len - start_index;
            }
        }
    }
}

pub fn main() !void {
    // try part1("example1.txt");
    try part2("example3.txt");
    try part2("input.txt");
}
