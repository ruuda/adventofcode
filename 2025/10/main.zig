const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

// Mental gymnastics needed to parse this input in Python: Call str.split a few times.
// Mental gymanstics needed to parse this in Zig: Hold my beer.
// Here we go.
const Machine = struct {
    target: u16,
    buttons: []u16,
};

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
        var buttons = try std.ArrayList(u16).initCapacity(alloc, 0);
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

        return Machine{ .target = target, .buttons = buttons.items };
    }

    fn parseTarget(self: *Parser) !u16 {
        var target: u16 = 0;
        var bit: u16 = 1;
        while (true) {
            switch (self.take()) {
                '.' => {},
                '#' => target = target | bit,
                ']' => break,
                else => return error.InvalidInput,
            }
            bit = bit << 1;
        }
        return target;
    }

    fn parseButton(self: *Parser) !u16 {
        var target: u16 = 0;

        while (true) {
            const bit = @as(u16, 1) << @truncate(self.take() - '0');
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
fn fewestPresses(m: Machine) u32 {
    // The possible on-off states are the integers 0 through n-1.
    const n: u16 = @as(u16, 1) << @truncate(m.buttons.len);
    var i: u16 = 0;

    // Track the best score so far (fewest buttons enabled).
    var fewest: u32 = @truncate(m.buttons.len);

    var state: u16 = 0;

    while (i < n - 1) {
        for (0..16) |k| {
            state = state ^ m.buttons[k];
            const bit: u16 = @as(u16, 1) << @truncate(k);
            if (i & bit == 0) break;
        }

        if (state == m.target) {
            const pc = @popCount(i);
            print("  {:2} {b}\n", .{ pc, i });
            if (pc < fewest) fewest = pc;
        }

        i += 1;
    }

    return fewest;
}

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const machines = try readInput(alloc, "example.txt");

    var part1: u32 = 0;
    for (machines) |m| {
        print("{b} {}\n", .{ m.target, m.buttons.len });
        part1 += fewestPresses(m);
    }
    print("Part 1: {}\n", .{part1});
}
