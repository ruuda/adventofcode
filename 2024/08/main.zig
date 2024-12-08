const std = @import("std");

fn read_input(fname: []const u8) ![][]u8 {
    const allocator = std.heap.page_allocator;
    var board = std.ArrayList([]u8).init(allocator);

    const file = try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    const in_stream = buf_reader.reader();
    var buf: [2048]u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try board.append(try allocator.dupe(u8, line));
    }

    return board.items;
}

const Antenna = struct {
    x: i32,
    y: i32,
    frequency: u8,
};

// Locate all the antennas on the map.
fn scan_map(map: [][]u8) ![]Antenna {
    const allocator = std.heap.page_allocator;
    var result = std.ArrayList(Antenna).init(allocator);

    for (0..map.len) |y| {
        const line = map[y];
        for (0..line.len) |x| {
            const cell = line[x];
            if (cell == '.') continue;
            const antenna = Antenna {
                .x = @as(i32, @intCast(x)),
                .y = @as(i32, @intCast(y)),
                .frequency = cell,
            };
            try result.append(antenna);
        }
    }

    return result.items;
}

const Antinode = struct { x: i32, y: i32 };

pub fn find_antinodes(mapw: usize, maph: usize, antennas: []Antenna) !std.AutoHashMap(Antinode, bool) {
    const allocator = std.heap.page_allocator;
    // I can't find a set type in the stdlib, we'll abuse a hash map as the set.
    var result = std.AutoHashMap(Antinode, bool).init(allocator);

    for (0..antennas.len) |i| {
        const ai = antennas[i];
        for (i + 1..antennas.len) |j| {
            const aj = antennas[j];
            // Only antennas with the same frequency interact.
            if (ai.frequency != aj.frequency) continue;
            const dx = aj.x - ai.x;
            const dy = aj.y - ai.y;
            const node = Antinode { .x = dx, .y = dy };
            // TODO: Check in bounds
            if (dx > mapw) continue;
            if (dy > maph) continue;
            try result.put(node, true);
        }
    }

    return result;
}

pub fn part1(fname: []const u8) !void {
    const stdout = std.io.getStdOut().writer();
    const map = try read_input(fname);
    const antennas = try scan_map(map);
    const antinodes = try find_antinodes(map[0].len, map.len, antennas);

    var an_iter = antinodes.iterator();
    while (an_iter.next()) |an| {
        try stdout.print("{} {}\n", .{an.key_ptr.x, an.key_ptr.y});
    }
}

pub fn main() !void {
    try part1("example.txt");
}
