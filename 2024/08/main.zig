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
    x: usize,
    y: usize,
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
                .x = x,
                .y = y,
                .frequency = cell,
            };
            try result.append(antenna);
        }
    }

    return result.items;
}

pub fn part1(fname: []const u8) !void {
    const stdout = std.io.getStdOut().writer();
    const map = try read_input(fname);
    const antennas = try scan_map(map);
    for (antennas) |a| {
        try stdout.print("{} {} {}\n", .{a.x, a.y, a.frequency});
    }
}

pub fn main() !void {
    try part1("example.txt");
}
