const std = @import("std");
const print = @import("std").debug.print;

fn readInput(fname: []const u8) ![][]u8 {
    const allocator = std.heap.page_allocator;
    var data = try std.ArrayList([]u8).initCapacity(allocator, 0);

    const file = try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    // We go line by line, the buffer needs to hold a line until delimiter.
    var buf: [512]u8 = undefined;
    var reader = file.reader(&buf);

    while (true) {
        const line = reader.interface.takeDelimiterExclusive('\n') catch break;
        try data.append(allocator, try allocator.dupe(u8, line));
        _ = reader.interface.discardDelimiterInclusive('\n') catch break;
    }

    return data.items;
}

pub fn main() !void {
    const data = try readInput("example.txt");
    for (data) |line| {
        print("{s}\n", .{line});
    }
}
