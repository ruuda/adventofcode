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

pub fn move_north(board: [][]u8) void {
    for (0..board[0].len) |x| {
        var y0: usize = 0;
        while (y0 < board.len) {
            var n: usize = 0;
            var y1 = board.len;
            for (y0..board.len) |y| {
                switch (board[y][x]) {
                    'O' => n += 1,
                    '#' => {
                        y1 = y;
                        break;
                    },
                    else => {},
                }
            }
            for (y0..y0 + n) |y| board[y][x] = 'O';
            for (y0 + n..y1) |y| board[y][x] = '.';
            y0 = y1 + 1;
        }
    }
}

pub fn part1(fname: []const u8) !void {
    const stdout = std.io.getStdOut().writer();
    var board = try read_input(fname);
    try stdout.print("Board has {d} rows and {d} cols.\n", .{ board.len, board[0].len });

    move_north(board);
    for (board) |line| {
        try stdout.print("{s}.\n", .{line});
    }

    var total_load: usize = 0;
    for (board, 0..) |line, i| {
        const weight = board.len - i;
        for (line) |ch| {
            if (ch == 'O') total_load += weight;
        }
    }

    try stdout.print("Part 1: Total load: {d}.\n", .{total_load});
}

pub fn main() !void {
    try part1("input.txt");
}
