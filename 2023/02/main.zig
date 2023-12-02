const std = @import("std");

const Draw = struct {
    red: u32,
    green: u32,
    blue: u32,
};

const Game = struct {
    id: u32,
    draws: [3]Draw,
};

pub fn read_input() !std.ArrayList(Game) {
    const allocator = std.heap.page_allocator;
    var result = std.ArrayList(Game).init(allocator);

    const file = try std.fs.cwd().openFile("example.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    const in_stream = buf_reader.reader();
    var buf: [512]u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var iter = std.mem.splitAny(u8, line, ":;");

        // Get the game id, cut off the "Game " prefix, convert to int.
        const id_str = iter.next().?;
        const id = try std.fmt.parseInt(u32, id_str["Game ".len..], 10);

        // const draw0_str = iter.next().?;
        // const draw1_str = iter.next().?;
        // const draw2_str = iter.next().?;

        const draw0 = Draw{ .red = 0, .green = 0, .blue = 0 };
        const draw1 = Draw{ .red = 0, .green = 0, .blue = 0 };
        const draw2 = Draw{ .red = 0, .green = 0, .blue = 0 };

        const game = Game{
            .id = id,
            .draws = [_]Draw{ draw0, draw1, draw2 },
        };

        try result.append(game);
    }

    return result;
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var games = try read_input();

    for (games.items) |game| {
        try stdout.print("Game id was {d}!\n", .{game.id});
    }
}
