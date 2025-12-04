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

fn remove_adjacent(map: [][]u8) u32 {
    const w = map[0].len;
    const h = map.len;
    const dp = [3]i32{ -1, 0, 1 };
    var count: u32 = 0;

    for (0..h) |y| {
        const iy: i32 = @intCast(y);
        for (0..w) |x| {
            const ix: i32 = @intCast(x);

            // We are only interested in cells that contain a roll ('@'),
            // empty cells ('.') we can skip.
            if (map[y][x] != '@') continue;

            var num_adjacent: u32 = 0;

            for (dp) |dx| {
                for (dp) |dy| {
                    if (dx == 0 and dy == 0) continue;
                    const px = ix + dx;
                    const py = iy + dy;
                    var p: u8 = '.';
                    if (py >= 0 and py < map.len) {
                        const line = map[@intCast(py)];
                        if (px >= 0 and px < line.len) {
                            p = line[@intCast(px)];
                        }
                    }

                    num_adjacent += if (p != '.') 1 else 0;
                }
            }

            // If the roll can be removed, count it, but also mark it for
            // removal.
            if (num_adjacent < 4) {
                count += 1;
                map[y][x] = 'x';
            }
        }
    }

    // After marking rolls for removal, we remove them.
    for (map, 0..) |line, y| {
        for (line, 0..) |cell, x| {
            if (cell == 'x') map[y][x] = '.';
        }
    }

    return count;
}

pub fn main() !void {
    const data = try read_input("input.txt");

    var count = remove_adjacent(data);
    print("Part 1: {}\n", .{count});

    while (true) {
        const n = remove_adjacent(data);
        print("Removing {} more ...\n", .{n});
        if (n == 0) break;
        count += n;
    }

    print("Part 2: {}\n", .{count});
}
