const std = @import("std");

const Loc = [3]u8;

const Node = struct {
    left: Loc,
    right: Loc,
};

const Input = struct {
    nav: std.ArrayList(u8),
    nodes: std.AutoHashMap(Loc, Node),
};

fn read_input() !Input {
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;
    var result = std.AutoHashMap(Loc, Node).init(allocator);

    const file = try std.fs.cwd().openFile("example1.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    const in_stream = buf_reader.reader();
    var buf: [2048]u8 = undefined;

    const nav = (try in_stream.readUntilDelimiterOrEof(&buf, '\n')).?;
    var nav_arr = std.ArrayList(u8).init(allocator);
    try nav_arr.appendSlice(nav);

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var iter = std.mem.splitAny(u8, line, " =(,)");

        const src = iter.next().?;
        const nn = iter.next().?;
        try stdout.print("Game {s} then {s}.\n", .{ src, nn });
    }

    return Input{ .nav = nav_arr, .nodes = result };
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const input = try read_input();

    try stdout.print("Have input {d}.\n", .{input.nodes.count()});
}
