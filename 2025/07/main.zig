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

fn countSplits(map: [][]u8) u64 {
    var nSplits: u64 = 0;

    for (1..map.len) |y| {
        const rowPrev = map[y - 1];
        var rowCurr = map[y];
        for (0..map[y].len) |x| {
            const above = rowPrev[x];
            const cell = rowCurr[x];
            if (above == 'S' or above == '|') {
                // There is a beam, extend it downwards.
                switch (cell) {
                    '.' => rowCurr[x] = '|',
                    '^' => {
                        rowCurr[x - 1] = '|';
                        rowCurr[x + 1] = '|';
                        nSplits += 1;
                    },
                    '|' => {
                        // There is already a beam, nothing to do here.
                    },
                    else => print("Unexpected cell at {}, {}: {c}\n", .{ x, y, cell }),
                }
            }
        }
    }

    return nSplits;
}

pub fn main() !void {
    const data = try readInput("input.txt");
    const part1 = countSplits(data);
    print("Part 1: {}\n", .{part1});
}
