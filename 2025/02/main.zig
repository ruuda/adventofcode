const std = @import("std");
const print = @import("std").debug.print;

const Range = struct {
    low: u64,
    high: u64,
};

fn read_input(fname: []const u8) ![]Range {
    const allocator = std.heap.page_allocator;
    var data = try std.ArrayList(Range).initCapacity(allocator, 128);

    const file = try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    var buf: [256]u8 = undefined;
    var reader = file.reader(&buf);

    while (true) {
        const part = try reader.interface.takeDelimiterExclusive(',');
        const part_trim = std.mem.trim(u8, part, "\n ");

        if (part_trim.len == 0) break;

        var part_iter = std.mem.splitScalar(u8, part_trim, '-');
        const low = try std.fmt.parseInt(u64, part_iter.first(), 10);
        const high = try std.fmt.parseInt(u64, part_iter.next().?, 10);

        try data.append(allocator, Range{ .low = low, .high = high });

        _ = reader.interface.discardDelimiterInclusive(',') catch break;
    }

    return data.items;
}

pub fn main() !void {
    const data = try read_input("example.txt");
    for (data) |item| {
        print("{}-{}\n", .{ item.low, item.high });
    }
}
