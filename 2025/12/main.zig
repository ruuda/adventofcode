const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Shape = [3][3]u8;

const Region = struct { w: u32, h: u32, counts: [5]u8 };

const Input = struct {
    shapes: [5]Shape,
    regions: []Region,
};

fn readInput(alloc: Allocator, fname: []const u8) !Input {
    var shapes: [5]Shape = undefined;
    var regions = try std.ArrayList(Region).initCapacity(alloc, 0);

    const file = try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    var buf: [512]u8 = undefined;
    const reader = file.reader(&buf);
    var r = reader.interface;

    inline for (0..5) |i| {
        print("Skip {}\n", .{i});
        // The shape number indication is pointless.
        _ = try r.discardDelimiterInclusive('\n');
        print("Skip {}\n", .{i});
        inline for (0..3) |y| {
            const line = try r.takeDelimiterInclusive('\n');
            inline for (0..3) |x| shapes[i][y][x] = line[x];
        }
        // After every shape is a blank line.
        _ = try r.discardDelimiterInclusive('\n');
    }

    while (true) {
        const wStr = r.takeDelimiterExclusive('x') catch break;
        r.toss(1);
        const hStr = r.takeDelimiterExclusive(':') catch break;
        r.toss(2);

        var counts: [5]u8 = undefined;
        inline for (0..5) |i| {
            const nStr = r.takeDelimiterExclusive(' ') catch break;
            counts[i] = try std.fmt.parseInt(u8, nStr, 10);
        }

        const region = Region{
            .w = try std.fmt.parseInt(u32, wStr, 10),
            .h = try std.fmt.parseInt(u32, hStr, 10),
            .counts = counts,
        };
        try regions.append(alloc, region);
        _ = r.discardDelimiterInclusive('\n') catch break;
    }

    return Input{ .shapes = shapes, .regions = regions.items };
}

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const input = try readInput(alloc, "example.txt");

    // var part1: u32 = 0;
    for (input.regions) |r| {
        print("{}x{}: {any}\n", .{ r.w, r.h, r.counts });
    }
    // print("Part 1: {}\n", .{part1});
}
