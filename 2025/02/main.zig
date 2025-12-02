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

fn count_duplicates(low: u64, high: u64) u64 {
    const min_digits: u64 = (std.math.log10_int(low) + 1) / 2;
    const max_digits: u64 = (std.math.log10_int(high) + 1) / 2;

    var n_digits = min_digits;
    var sum: u64 = 0;

    while (n_digits <= max_digits) {
        const i_min = std.math.pow(u64, 10, n_digits -| 1);
        const i_max = std.math.pow(u64, 10, n_digits);
        print("  {} digits, checking: {} - {}\n", .{ n_digits, i_min, i_max });
        var i = i_min;

        // TODO: We can tighten the lower bound.

        while (i < i_max) {
            const dupdup = i + i * i_max;
            if (dupdup < low) {
                i += 1;
                continue;
            }
            if (dupdup > high) break;
            print("  found {} in {}-{}\n", .{ dupdup, low, high });
            sum += dupdup;
            i += 1;
        }

        n_digits += 1;
    }

    return sum;
}

pub fn main() !void {
    const data = try read_input("input.txt");
    var sum: u64 = 0;
    for (data) |item| {
        print("{}-{}\n", .{ item.low, item.high });
        sum += count_duplicates(item.low, item.high);
    }
    print("Sum: {}\n", .{sum});
}
