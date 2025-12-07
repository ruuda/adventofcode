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

const Result = struct {
    nSplits: u64,
    nTimelines: u64,
};

fn countSplits(map: [][]u8) !Result {
    var nSplits: u64 = 0;

    // For every beam location, we will track the number of distinct
    // ways to get there.
    const allocator = std.heap.page_allocator;
    var nPaths = try std.ArrayList([]u64).initCapacity(allocator, map.len);

    // The size of `nPaths` must match `map` so put something on the first line,
    // we don't reference it.
    nPaths.appendAssumeCapacity(try allocator.alloc(u64, map[0].len));

    for (1..map.len) |y| {
        const rowPrev = map[y - 1];
        var rowCurr = map[y];

        var paths = try allocator.alloc(u64, rowCurr.len);
        @memset(paths, 0);
        nPaths.appendAssumeCapacity(paths);

        for (0..map[y].len) |x| {
            const above = rowPrev[x];
            const cell = rowCurr[x];
            if (above == 'S' or above == '|') {
                // There is a beam, extend it downwards.
                const p = if (above == 'S') 1 else nPaths.items[y - 1][x];
                switch (cell) {
                    '.' => {
                        rowCurr[x] = '|';
                        paths[x] += p;
                    },
                    '^' => {
                        rowCurr[x - 1] = '|';
                        rowCurr[x + 1] = '|';
                        paths[x - 1] += p;
                        paths[x + 1] += p;
                        nSplits += 1;
                    },
                    '|' => {
                        // The beam is already there because of a split in this
                        // row, but we need to add all possible ways of reaching
                        // this cell where the beam came from above.
                        paths[x] += p;
                    },
                    else => print("Unexpected cell at {}, {}: {c}\n", .{ x, y, cell }),
                }
            }
        }
        var k: u64 = 0;
        for (paths) |pp| {
            if (pp > 0) {
                print("{:3} ", .{pp});
            } else {
                print("    ", .{});
            }
            k += pp;
        }
        print(" -> {}\n", .{k});
    }

    // For each of the reachable final beam positions, sum the number of ways to
    // reach it, which is the answer to part 2.
    var nTimelines: u64 = 0;
    for (nPaths.items[nPaths.items.len - 1]) |count| nTimelines += count;

    return Result{ .nSplits = nSplits, .nTimelines = nTimelines };
}

pub fn main() !void {
    const data = try readInput("input.txt");
    const result = try countSplits(data);
    print("Part 1: {}\n", .{result.nSplits});
    print("Part 2: {}\n", .{result.nTimelines});
}
