const std = @import("std");
const print = @import("std").debug.print;

fn read_input(fname: []const u8) ![][]u8 {
    const allocator = std.heap.page_allocator;
    var data = try std.ArrayList([]u8).initCapacity(allocator, 1);

    const file = try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    // At least two lines of content.
    var buf: [280]u8 = undefined;
    var reader = file.reader(&buf);

    while (true) {
        const line = reader.interface.takeDelimiterExclusive('\n') catch break;
        try data.append(allocator, try allocator.dupe(u8, line));
        _ = reader.interface.discardDelimiterInclusive('\n') catch break;
    }

    return data.items;
}

pub fn main() !void {
    const data = try read_input("example.txt");

    print("data: {s}\n", .{data[0]});
}
