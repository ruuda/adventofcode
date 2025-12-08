const std = @import("std");
const print = @import("std").debug.print;

const Point = struct {
    coord: [3]f32,

    fn sqr_dist(self: Point, other: Point) f32 {
        var res: f32 = 0.0;
        inline for (0..3) |i| {
            const d = self.coord[i] - other.coord[i];
            res += d * d;
        }
        return res;
    }
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

const Pair = struct {
    d2: f32,
    ia: usize,
    ib: usize,
};

fn ltPair(ctx: u64, lhs: Pair, rhs: Pair) bool {
    _ = ctx;
    if (lhs.d2 < rhs.d2) return true;
    if (lhs.d2 > rhs.d2) return false;
    if (lhs.ia < rhs.ia) return true;
    if (lhs.ia > rhs.ia) return false;
    return lhs.ib < lhs.ib;
}

fn countClusters(comptime nWires: u32, points: []Point) !u64 {
    const allocator = std.heap.page_allocator;
    // Yep it's quadratic, but for only 1000 pairs we don't really need to bring
    // out the spatial data structures.
    var pairs = try std.ArrayList(Pair).initCapacity(allocator, points.len * points.len / 2);
    for (0..points.len) |i| {
        for (i + 1..points.len) |j| {
            const pair = Pair{
                .d2 = points[i].sqr_dist(points[j]),
                .ia = i,
                .ib = j,
            };
            pairs.appendAssumeCapacity(pair);
        }
    }

    const dummyContext: u64 = 0;
    std.sort.pdq(Pair, pairs.items, dummyContext, ltPair);

    // Now assign every point to its own cluster. Clusters are identified by
    // an integer id.
    var ids = try allocator.alloc(u32, points.len);
    for (0..points.len) |i| {
        ids[i] = @truncate(i);
    }

    // For the `nWires` closest points, we put them in the same cluster by
    // turning the id into the id of the
    var wiresSpent: u32 = 0;
    for (0..pairs.items.len) |pp| {
        const pair = pairs.items[pp];
        const c1 = ids[pair.ia];
        const c2 = ids[pair.ib];

        // If they are already part of the same cluster then we don't connect
        // them.
        if (c1 == c2) continue;

        // If they are not part of the same cluster, then we join them to the
        // same cluster, arbitrarily picking c1 as the id for both.
        for (0..points.len) |i| {
            if (ids[i] == c2) ids[i] = c1;
        }

        const p1 = points[pair.ia];
        const p2 = points[pair.ib];
        print("Connecting {} {} {} to {} {} {}\n", .{
            p1.coord[0], p1.coord[1], p1.coord[2],
            p2.coord[0], p2.coord[1], p2.coord[2],
        });

        for (ids) |c| {
            print("{} ", .{c});
        }
        print("\n", .{});

        wiresSpent += 1;
        if (wiresSpent >= nWires) break;
    }

    // Count how many members every cluster has.
    var counts = try allocator.alloc(u32, points.len);
    @memset(counts, 0);

    for (ids) |id| {
        counts[id] += 1;
    }

    for (counts) |c| {
        print("{} ", .{c});
    }
    print("\n", .{});

    return 0 + nWires;
}

pub fn main() !void {
    const data = try readInput("example.txt");
    const part1 = try countClusters(10, data);
    print("Part 1: {}\n", .{part1});
}
