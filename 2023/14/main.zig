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

pub fn move_north_south(board: [][]u8, comptime dir: u8) void {
    for (0..board[0].len) |x| {
        var y0: usize = 0;
        while (y0 < board.len) {
            var n: usize = 0;
            var y1 = board.len;
            for (y0..board.len) |y| {
                const yy = if (dir == 'N') y else board.len - 1 - y;
                switch (board[yy][x]) {
                    'O' => n += 1,
                    '#' => {
                        y1 = y;
                        break;
                    },
                    else => {},
                }
            }
            for (y0..y0 + n) |y| {
                const yy = if (dir == 'N') y else board.len - 1 - y;
                board[yy][x] = 'O';
            }
            for (y0 + n..y1) |y| {
                const yy = if (dir == 'N') y else board.len - 1 - y;
                board[yy][x] = '.';
            }
            y0 = y1 + 1;
        }
    }
}

pub fn move_east_west(board: [][]u8, comptime dir: u8) void {
    for (0..board.len) |y| {
        var x0: usize = 0;
        while (x0 < board[y].len) {
            var n: usize = 0;
            var x1 = board[y].len;
            for (x0..board[y].len) |x| {
                const xx = if (dir == 'E') x else board[y].len - 1 - x;
                switch (board[y][xx]) {
                    'O' => n += 1,
                    '#' => {
                        x1 = x;
                        break;
                    },
                    else => {},
                }
            }
            for (x0..x0 + n) |x| {
                const xx = if (dir == 'E') x else board.len - 1 - x;
                board[y][xx] = 'O';
            }
            for (x0 + n..x1) |x| {
                const xx = if (dir == 'E') x else board.len - 1 - x;
                board[y][xx] = '.';
            }
            x0 = x1 + 1;
        }
    }
}

pub fn total_load_north_support(board: [][]u8) usize {
    var total_load: usize = 0;
    for (board, 0..) |line, i| {
        const weight = board.len - i;
        for (line) |ch| {
            if (ch == 'O') total_load += weight;
        }
    }
    return total_load;
}

pub fn print_board(board: [][]u8) !void {
    // Would be better to pass in stdout, but I don't know what type it has.
    const stdout = std.io.getStdOut().writer();
    for (board) |line| {
        try stdout.print("{s}.\n", .{line});
    }
}

pub fn part1(fname: []const u8) !void {
    const stdout = std.io.getStdOut().writer();
    var board = try read_input(fname);
    try stdout.print("Board has {d} rows and {d} cols.\n", .{ board.len, board[0].len });

    move_north_south(board, 'N');
    try print_board(board);

    const total_load = total_load_north_support(board);
    try stdout.print("Part 1: Total load: {d}.\n", .{total_load});
}

pub fn part2(fname: []const u8) !void {
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;
    var board = try read_input(fname);
    var history = std.ArrayList([][]u8).init(allocator);
    try history.append(board);

    for (0..5) |round| {
        var new_board = try allocator.alloc([]u8, board.len);
        for (board, 0..) |line, i| {
            new_board[i] = try allocator.dupe(u8, line);
        }
        try stdout.print("\nRound {d}:\n", .{round});
        try print_board(new_board);

        move_north_south(new_board, 'N');
        try stdout.print("\nRound {d} N:\n", .{round});
        try print_board(new_board);

        move_east_west(new_board, 'W');
        try stdout.print("\nRound {d} W:\n", .{round});
        try print_board(new_board);

        move_north_south(new_board, 'S');
        try stdout.print("\nRound {d} S:\n", .{round});
        try print_board(new_board);

        move_east_west(new_board, 'E');
        try stdout.print("\nRound {d} E:\n", .{round});
        try print_board(new_board);
    }
}

pub fn main() !void {
    //try part1("input.txt");
    try part2("example.txt");
}