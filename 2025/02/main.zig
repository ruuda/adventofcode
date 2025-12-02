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

fn count_duplicates(comptime max_parts: u32, low: u64, high: u64) !u64 {
    print("{} - {}\n", .{ low, high });
    var n_parts: u32 = 2;
    var sum: u64 = 0;

    // Now that we check multiple parts, we may visit duplicates, so we need to
    // exclude those.
    var buf: [256]u64 = undefined;
    var discoveries = std.ArrayList(u64).initBuffer(&buf);

    while (n_parts <= max_parts) {
        const min_digits: u64 = (std.math.log10_int(low) + 1) / n_parts;
        const max_digits: u64 = (std.math.log10_int(high) + 1) / n_parts;

        var n_digits = if (min_digits == 0) 1 else min_digits;

        while (n_digits <= max_digits) {
            const i_min = std.math.pow(u64, 10, n_digits -| 1);
            const i_max = std.math.pow(u64, 10, n_digits);
            print("     {} parts {} digits, checking: {} - {}\n", .{ n_parts, n_digits, i_min, i_max });
            var i = i_min;

            while (i < i_max) {
                // Repeat the number n_parts times in decimal.
                var dupdup = i;
                var j: u32 = 1;
                while (j < n_parts) {
                    dupdup = dupdup * i_max + i;
                    j += 1;
                }
                if (dupdup < low) {
                    i += 1;
                    continue;
                }
                if (dupdup > high) break;

                var is_new = true;
                for (discoveries.items) |other| {
                    if (other == dupdup) {
                        is_new = false;
                        break;
                    }
                }

                if (is_new) {
                    sum += dupdup;
                    try discoveries.appendBounded(dupdup);
                    print("  => {} in {}-{}\n", .{ dupdup, low, high });
                } else {
                    print("  =x {} in {}-{} (skip duplicate)\n", .{ dupdup, low, high });
                }

                i += 1;
            }

            n_digits += 1;
        }

        n_parts += 1;
    }

    return sum;
}

pub fn main() !void {
    const data = try read_input("input.txt");

    if (false) {
        var part1: u64 = 0;
        for (data) |item| {
            part1 += try count_duplicates(2, item.low, item.high);
        }
        print("Part 1 sum: {}\n", .{part1});
    }

    var part2: u64 = 0;
    for (data) |item| {
        // u64::MAX has 20 decimal digits, so we can have at most 20 parts.
        part2 += try count_duplicates(20, item.low, item.high);
    }
    print("Part 2 sum: {}\n", .{part2});
}
