const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Machine = struct {
    // For part 1 initially I used a u16 with a bitmask per indicator light, but
    // for part 2 we want to sum too, so now we use u128 with one byte per light.
    // The input has at most 13 lights per machine, so this fits. SIMD for free!
    target: u128,
    buttons: []u128,
    joltage: []u8,

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
        var buttons = try std.ArrayList(u128).initCapacity(alloc, 0);
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

        var joltage = try std.ArrayList(u8).initCapacity(alloc, target.n);
        self.parseJoltages(&joltage);

        var totalJoltage: u16 = 0;
        for (joltage.items) |j| totalJoltage += j;

        return Machine{
            .target = target.target,
            .buttons = buttons.items,
            .joltage = joltage.items,
            .totalJoltage = totalJoltage,
        };
    }

    fn parseTarget(self: *Parser) !struct { target: u128, n: usize } {
        var target: u128 = 0;
        var bit: u128 = 1;
        var n: usize = 0;
        while (true) {
            switch (self.take()) {
                '.' => {},
                '#' => target = target | bit,
                ']' => break,
                else => return error.InvalidInput,
            }
            bit = bit << 8;
            n += 1;
        }
        return .{ .target = target, .n = n };
    }

    fn parseButton(self: *Parser) !u128 {
        var target: u128 = 0;

        while (true) {
            const n: u8 = self.take() - '0';
            const bit = @as(u128, 1) << @truncate(8 * n);
            target = target | bit;
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

    fn parseJoltages(self: *Parser, out: *std.ArrayList(u8)) void {
        var n: u8 = 0;
        while (true) {
            const ch = self.take();
            switch (ch) {
                ',' => {
                    out.appendAssumeCapacity(n);
                    n = 0;
                },
                '}' => {
                    out.appendAssumeCapacity(n);
                    break;
                },
                else => {
                    n = n * 10 + (ch - '0');
                },
            }
        }
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

    var state: u128 = 0;

    while (i < n - 1) {
        for (0..16) |k| {
            state = state ^ m.buttons[k];
            const bit: u128 = @as(u16, 1) << @truncate(k);
            if (i & bit == 0) break;
        }

        i += 1;

        if (state == m.target) {
            const pc = @popCount(i);
            print("  {:2} {b}\n", .{ pc, i });
            if (pc < fewest) fewest = pc;
        }
    }

    return fewest;
}

fn fewestPresses2(m: Machine) u32 {
    // Observation: Based on which lights a button toggles, it has a maximum
    // number of presses.
    var maxima: [16]u8 = undefined;
    @memset(&maxima, 0xff);

    for (0..m.buttons.len) |b| {
        const button = m.buttons[b];
        for (0..m.joltage.len) |k| {
            const bit = @as(u128, 1) << @truncate(8 * k);
            if (button & bit == 0) continue;
            const j = m.joltage[k];
            if (j < maxima[b]) maxima[b] = j;
        }
    }

    for (maxima) |mm| print("{} ", .{mm});
    print("\n", .{});

    // We are going to try all possible options that sum to `n` presses, by
    // increasing `n`, so when we find a solution, it is the minimal one. This
    // is very wasteful, we could probably find a better lower bound, but if the
    // stupid thing works, then let's not do the smart thing.
    //for (1..m.totalJoltage + 1) |n| {}
    return 0;
}

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const machines = try readInput(alloc, "input.txt");

    var part1: u32 = 0;
    var part2: u32 = 0;
    for (machines) |m| {
        print("{x} {}\n", .{ m.target, m.buttons.len });
        part1 += fewestPresses1(m);
        part2 += fewestPresses2(m);
    }
    print("Part 1: {}\n", .{part1});
    print("Part 2: {}\n", .{part2});
}
