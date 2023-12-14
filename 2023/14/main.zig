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

pub fn part1(fname: []const u8) !void {
    const stdout = std.io.getStdOut().writer();
    const board = try read_input(fname);

    try stdout.print("Board has {d} rows and {d} cols.\n", .{ board.len, board[0].len });
}

pub fn main() !void {
    try part1("example.txt");
}
