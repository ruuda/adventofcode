const std = @import("std");
const print = @import("std").debug.print;

const Point = struct {
    coord: [3]f32,
};

fn readInput(fname: []const u8) ![]Point {
    const allocator = std.heap.page_allocator;
    var data = try std.ArrayList(Point).initCapacity(allocator, 0);

    const file = try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    // We go line by line, the buffer needs to hold a line until delimiter.
    var buf: [512]u8 = undefined;
    var reader = file.reader(&buf);

    while (true) {
        const line = reader.interface.takeDelimiterExclusive('\n') catch break;
        var coord = [3]f32{ 0.0, 0.0, 0.0 };
        var it = std.mem.splitScalar(u8, line, ',');
        inline for (0..3) |i| {
            const sx = it.next() orelse break;
            coord[i] = try std.fmt.parseFloat(f32, sx);
        }
        try data.append(allocator, Point{ .coord = coord });
        _ = reader.interface.discardDelimiterInclusive('\n') catch break;
    }

    return data.items;
}

pub fn main() !void {
    const data = try readInput("example.txt");
    for (data) |p| {
        print("{} {} {}\n", .{ p.coord[0], p.coord[1], p.coord[2] });
    }
}
