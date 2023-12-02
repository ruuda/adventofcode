const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const file = try std.fs.cwd().openFile("example.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    const in_stream = buf_reader.reader();
    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try stdout.print("Line was {s}!\n", .{line});
    }
}
