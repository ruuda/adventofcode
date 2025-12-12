const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Shape = [3][3]u8;

const Region = struct { w: u32, h: u32, counts: [6]u8 };

const Input = struct {
    shapes: [6]Shape,
    regions: []Region,
};

fn readInput(alloc: Allocator, fname: []const u8) !Input {
    var shapes: [6]Shape = undefined;
    var regions = try std.ArrayList(Region).initCapacity(alloc, 0);

    const file = try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    // With a smaller buffer size, sometimes `wStr` and `hStr` below contain a
    // space. I don't understand how this happens, sidestep the problem by just
    // fitting the entire file into the buffer.
    var buf: [4096 * 10]u8 = undefined;
    var r = file.reader(&buf);

    inline for (0..6) |i| {
        // The shape number indication is pointless.
        _ = try r.interface.discardDelimiterInclusive('\n');
        inline for (0..3) |y| {
            const line = try r.interface.takeDelimiterInclusive('\n');
            inline for (0..3) |x| shapes[i][y][x] = line[x];
        }
        // After every shape is a blank line.
        _ = try r.interface.discardDelimiterInclusive('\n');
    }

    while (true) {
        r.interface.fill(7) catch break;
        const wStr = try r.interface.takeDelimiterExclusive('x');
        try r.interface.discardAll(1);
        const hStr = try r.interface.takeDelimiterExclusive(':');
        try r.interface.discardAll(2);

        var counts: [6]u8 = undefined;
        inline for (0..6) |i| {
            const delimiter = if (i < 5) ' ' else '\n';
            const nStr = try r.interface.takeDelimiterExclusive(delimiter);
            counts[i] = try std.fmt.parseInt(u8, nStr, 10);
            try r.interface.discardAll(1);
        }

        const region = Region{
            .w = try std.fmt.parseInt(u32, wStr, 10),
            .h = try std.fmt.parseInt(u32, hStr, 10),
            .counts = counts,
        };
        try regions.append(alloc, region);
    }

    return Input{ .shapes = shapes, .regions = regions.items };
}

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const input = try readInput(alloc, "input.txt");

    // var part1: u32 = 0;
    for (input.regions) |r| {
        print("{}x{}: {any}\n", .{ r.w, r.h, r.counts });
    }
    // print("Part 1: {}\n", .{part1});
}
