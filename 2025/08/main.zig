const std = @import("std");
const print = @import("std").debug.print;

const Point = struct {
    coord: [3]f64,

    fn sqr_dist(self: Point, other: Point) f64 {
        var res: f64 = 0.0;
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
        var coord = [3]f64{ 0.0, 0.0, 0.0 };
        var it = std.mem.splitScalar(u8, line, ',');
        inline for (0..3) |i| {
            const sx = it.next() orelse break;
            coord[i] = try std.fmt.parseFloat(f64, sx);
        }
        try data.append(allocator, Point{ .coord = coord });
        _ = reader.interface.discardDelimiterInclusive('\n') catch break;
    }

    return data.items;
}

const Pair = struct {
    d2: f64,
    ia: usize,
    ib: usize,
};

fn ltPair(ctx: void, lhs: Pair, rhs: Pair) bool {
    _ = ctx;
    if (lhs.d2 < rhs.d2) return true;
    if (lhs.d2 > rhs.d2) return false;
    if (lhs.ia < rhs.ia) return true;
    if (lhs.ia > rhs.ia) return false;
    return lhs.ib < lhs.ib;
}

fn countClusters(comptime nWires: u32, points: []Point) !void {
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

    std.sort.pdq(Pair, pairs.items, {}, ltPair);

    // Now assign every point to its own cluster. Clusters are identified by
    // an integer id.
    var ids = try allocator.alloc(u32, points.len);
    for (0..points.len) |i| ids[i] = @truncate(i);

    // For the `nWires` closest points, we put them in the same cluster by
    // turning the id into the id of the
    var wiresSpent: u32 = 0;
    for (0..pairs.items.len) |pp| {
        // At first I misunderstood the exercise, but we count a wire when we
        // *inspect* the closest pair, even if we end up not connecting them.
        // Once we are at that point, part 1 triggers.
        if (wiresSpent == nWires) {
            // Count how many members every cluster has.
            var counts = try allocator.alloc(u32, points.len);
            @memset(counts, 0);
            for (ids) |id| counts[id] += 1;

            // Sort cluster member counts descending.
            std.mem.sort(u32, counts, {}, std.sort.desc(u32));

            var result: u32 = 1;
            for (0..3) |i| result *= counts[i];
            print("Part 1: {}\n", .{result});
        }

        wiresSpent += 1;

        const pair = pairs.items[pp];
        const c1 = ids[pair.ia];
        const c2 = ids[pair.ib];

        // If they are already part of the same cluster then we don't connect
        // them.
        if (c1 == c2) continue;

        // If they are not part of the same cluster, then we join them to the
        // same cluster, arbitrarily picking c1 as the id for both.
        var isComplete = true;
        for (0..points.len) |i| {
            if (ids[i] == c2) ids[i] = c1;
            if (ids[i] != c1) isComplete = false;
        }

        if (isComplete) {
            const p1 = points[pair.ia];
            const p2 = points[pair.ib];
            const result = p1.coord[0] * p2.coord[0];
            print("Part 2: {} at wire {} \n", .{ result, wiresSpent });
            break;
        }
    }
}

pub fn main() !void {
    const data = try readInput("input.txt");
    try countClusters(1000, data);
}
