const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

// A count for each of the at most 13 lights. Fits in fewer than 128 bits!
const Count = @Vector(13, u8);

// Count with n-th index set to 1 and others set to 0.
fn nth(n: usize) Count {
    var result: Count = @splat(0);
    result[n] = 1;
    return result;
}

fn descPopCount(context: void, lhs: Count, rhs: Count) bool {
    _ = context;
    const nlhs = @reduce(.Add, @popCount(lhs));
    const nrhs = @reduce(.Add, @popCount(rhs));
    return nlhs > nrhs;
}

// For part 1 initially I used a u16 with a bitmask per indicator light, but
// for part 2 we want to sum too, so now we use u128 with one byte per light.
// The input has at most 13 lights per machine, so this fits. SIMD for free!
const Machine = struct {
    // For part 1, target on/off bit per light.
    target: Count,

    // The buttons, each as bitmask of the lights they touch.
    buttons: []Count,

    // Joltages per light.
    joltage: Count,

    // Sum of the per-light joltages.
    totalJoltage: u16,
};

// Mental gymnastics needed to parse this input in Python: Call str.split a few times.
// Mental gymanstics needed to parse this in Zig: Hold my beer.
// Here we go.
const Parser = struct {
    input: []const u8,
    cursor: usize,

    fn take(self: *Parser) u8 {
        const result = self.input[self.cursor];
        self.cursor += 1;
        return result;
    }

    fn expect(self: *Parser, char: u8) !void {
        if (self.take() == char) return;
        return error.InvalidInput;
    }

    fn parseMachine(self: *Parser, alloc: Allocator) !Machine {
        var buttons = try std.ArrayList(Count).initCapacity(alloc, 0);
        try self.expect('[');
        const target = try self.parseTarget();
        try self.expect(' ');

        while (true) {
            switch (self.take()) {
                '(' => try buttons.append(alloc, try self.parseButton()),
                '{' => break,
                else => return error.InvalidInput,
            }
        }

        const joltage = self.parseJoltage();
        var totalJoltage: u16 = 0;
        for (0..13) |i| totalJoltage += joltage[i];

        return Machine{
            .target = target.target,
            .buttons = buttons.items,
            .joltage = joltage,
            .totalJoltage = totalJoltage,
        };
    }

    fn parseTarget(self: *Parser) !struct { target: Count, n: usize } {
        var target: Count = @splat(0);
        var i: usize = 0;
        while (true) {
            switch (self.take()) {
                '.' => {},
                '#' => target[i] = 1,
                ']' => break,
                else => return error.InvalidInput,
            }
            i += 1;
        }
        return .{ .target = target, .n = i };
    }

    fn parseButton(self: *Parser) !Count {
        var target: Count = @splat(0);

        while (true) {
            const n: u8 = self.take() - '0';
            target[n] = 1;
            switch (self.take()) {
                ',' => continue,
                ')' => break,
                else => return error.InvalidInput,
            }
        }

        // A button is always followed by a space.
        try self.expect(' ');

        return target;
    }

    fn parseJoltage(self: *Parser) Count {
        var target: Count = @splat(0);
        var i: u8 = 0;
        var n: u8 = 0;

        while (true) {
            const ch = self.take();
            switch (ch) {
                ',' => {
                    target[i] = n;
                    n = 0;
                    i += 1;
                },
                '}' => {
                    target[i] = n;
                    break;
                },
                else => {
                    n = n * 10 + (ch - '0');
                },
            }
        }

        return target;
    }
};

fn readInput(alloc: Allocator, fname: []const u8) ![]Machine {
    var machines = try std.ArrayList(Machine).initCapacity(alloc, 0);

    const file = try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    // We go line by line, the buffer needs to hold a line until delimiter.
    var buf: [512]u8 = undefined;
    var reader = file.reader(&buf);

    while (true) {
        const line = reader.interface.takeDelimiterExclusive('\n') catch break;
        var parser = Parser{ .cursor = 0, .input = line };
        try machines.append(alloc, try parser.parseMachine(alloc));
        _ = reader.interface.discardDelimiterInclusive('\n') catch break;
    }

    return machines.items;
}

// Solve part 1 per machine. We explore the full state space of combination of
// button presses, the input is small enough that it's feasible.
fn fewestPresses1(m: Machine) u32 {
    // The possible on-off states are the integers 0 through n-1.
    const n: u16 = @as(u16, 1) << @truncate(m.buttons.len);
    var i: u16 = 0;

    // Track the best score so far (fewest buttons enabled).
    var fewest: u32 = @truncate(m.buttons.len);
    var state: Count = @splat(0);

    while (i < n - 1) {
        for (0..m.buttons.len) |k| {
            state ^= m.buttons[k];
            const bit: u16 = @as(u16, 1) << @truncate(k);
            if (i & bit == 0) break;
        }

        i += 1;

        if (@reduce(.And, state == m.target)) {
            const pc = @popCount(i);
            print("  {:2} {b}\n", .{ pc, i });
            if (pc < fewest) fewest = pc;
        }
    }

    return fewest;
}

fn fewestPresses2(m: Machine) u32 {
    // Sort buttons by descending number of lights they activate. We want to try
    // pressing the buttons that activate most lights first.
    std.sort.pdq(Count, m.buttons, {}, descPopCount);

    // Observation: Based on which lights a button toggles, it has a maximum
    // number of presses. TODO: Set to 0 for non-existing buttons.
    var maxima: Count = @splat(0xff);

    for (0..m.buttons.len) |b| {
        const button = m.buttons[b];

        // The light with the lowest joltage that this button connects to,
        // is the maximum number of times we can press it before overflow.
        for (0..13) |k| {
            if (button[k] == 0) continue;
            const j = m.joltage[k];
            if (j < maxima[b]) maxima[b] = j;
        }
    }

    print("{} | {} => {}\n", .{ maxima, m.joltage, m.totalJoltage });

    // An upper bound on the number of presses is the total joltage, when every
    // button presses a single light.
    var fewest: u16 = @truncate(m.totalJoltage);

    const initCount: u16 = 0;
    const initState: Count = @splat(0);
    fewestPressesRec(&m, maxima, initCount, initState, 0, &fewest);

    return fewest;
}

fn fewestPressesRec(
    m: *const Machine,
    maxima: Count,
    partialCount: u16,
    partialState: Count,
    b: usize,
    fewest: *u16,
) void {
    var totalCount: u16 = partialCount;
    var state = partialState;
    var count: u8 = 0;

    // Base case: all buttons have a count assigned, is the solution valid?
    if (b == m.buttons.len) {
        //print("  ? {:2} {}\n", .{ totalCount, state });
        if (@reduce(.And, state == m.joltage)) {
            print("  {:2} {}\n", .{ totalCount, state });
            std.debug.assert(totalCount < fewest.*);
            fewest.* = totalCount;
        }
        return;
    }

    // Recursion case: walk through all possible count assignments for button b.
    while (count <= maxima[b]) {
        fewestPressesRec(m, maxima, totalCount, state, b + 1, fewest);
        count += 1;
        state += m.buttons[b];
        totalCount += 1;

        // If this branch is at this point already not going to yield a better
        // solution, abandon and prune.
        if (totalCount >= fewest.*) return;

        // If we overshoot, then no more solutions are possible for this count
        // assignment, and we can prune the entire branch of the search space.
        if (@reduce(.Or, state > m.joltage)) return;
    }
}

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const machines = try readInput(alloc, "input.txt");

    var part1: u32 = 0;
    var part2: u32 = 0;
    for (machines) |m| {
        print("{} {}\n", .{ m.target, m.buttons.len });
        part1 += fewestPresses1(m);
        part2 += fewestPresses2(m);
    }
    print("Part 1: {}\n", .{part1});
    print("Part 2: {}\n", .{part2});
}
