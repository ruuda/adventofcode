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

fn in_bounds(mapw: usize, maph: usize, node: Antinode) bool {
    return (node.x >= 0) and (node.y >= 0) and (node.x < mapw) and (node.y < maph);
}

pub fn find_antinodes(
    mapw: usize,
    maph: usize,
    antennas: []Antenna,
    comptime part: u8,
) !std.AutoHashMap(Antinode, bool) {
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

            if (part == 1) {
                const nij = Antinode { .x = ai.x - dx, .y = ai.y - dy };
                const nji = Antinode { .x = aj.x + dx, .y = aj.y + dy };
                if (in_bounds(mapw, maph, nij)) try result.put(nij, true);
                if (in_bounds(mapw, maph, nji)) try result.put(nji, true);
            }

            if (part == 2) {
                var ni = Antinode { .x = ai.x, .y = ai.y };
                while (in_bounds(mapw, maph, ni)) {
                    try result.put(ni, true);
                    ni.x -= dx;
                    ni.y -= dy;
                }

                var nj = Antinode { .x = aj.x, .y = aj.y };
                while (in_bounds(mapw, maph, nj)) {
                    try result.put(nj, true);
                    nj.x += dx;
                    nj.y += dy;
                }
            }
        }
    }

    return result;
}

pub fn count_antinodes(fname: []const u8, comptime part: u8) !void {
    const stdout = std.io.getStdOut().writer();
    const map = try read_input(fname);
    const antennas = try scan_map(map);
    const antinodes = try find_antinodes(map[0].len, map.len, antennas, part);

    const debug_print = false;
    if (debug_print) {
        var an_iter = antinodes.iterator();
        while (an_iter.next()) |an| {
            try stdout.print("{} {}\n", .{an.key_ptr.x, an.key_ptr.y});
        }
    }
    try stdout.print("Part {}: {} unique anti-nodes\n", .{part, antinodes.count()});
}

pub fn main() !void {
    try count_antinodes("input.txt", 1);
    try count_antinodes("input.txt", 2);
}
