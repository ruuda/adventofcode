const std = @import("std");

const Draw = struct {
    red: u32,
    green: u32,
    blue: u32,
};

const Game = struct {
    id: u32,
    draws: []Draw,
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

        var draws = std.ArrayList(Draw).init(allocator);

        while (iter.next()) |draw_str| {
            var draw = Draw{ .red = 0, .green = 0, .blue = 0 };
            var colors = std.mem.splitAny(u8, draw_str, ",");
            while (colors.next()) |color| {
                const trimmed = std.mem.trim(u8, color, " ");
                std.debug.print("Trimmed: {s}\n", .{trimmed});
                const split = std.mem.indexOfAny(u8, trimmed, " ").?;
                const count_str = trimmed[0..split];
                const color_str = trimmed[split + 1 ..];
                const count = try std.fmt.parseInt(u32, count_str, 10);
                if (std.mem.eql(u8, color_str, "red")) draw.red = count;
                if (std.mem.eql(u8, color_str, "green")) draw.green = count;
                if (std.mem.eql(u8, color_str, "blue")) draw.blue = count;
            }
            try draws.append(draw);
        }
        const game = Game{
            .id = id,
            .draws = try draws.toOwnedSlice(),
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
