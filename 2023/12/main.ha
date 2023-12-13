// Run with `hare run main.ha` or `hare build -o main main.ha && ./main`.
use fmt;
use io;
use os;
use strings;
use strconv;

// TODO: Implement using dynamic programming instead of recursion ...
fn count_options(map: []u8, lens: []u32, expect_end: u8) u32 = {
	const m = len(map);
	const n = len(lens);
	assert(m > 0, "Should not call this on empty input because we expect a particular end.");

	const at_end = map[m - 1];
	const end_matches = (at_end == expect_end) || (at_end == '?') || (expect_end == '?');

	if (!end_matches) return 0;
	if (n > m) return 0;
	if (n > 0 && lens[0] > m) return 0;

	if (m == 1 && n == 1 && expect_end == '.') return 0;
	if (m == 1 && n == 0 && at_end == '.') return 1;
	if (m == 1 && n == 0 && at_end == '?') return 1;
	if (m == 1 && n == 0 && at_end == '#') return 0;
	if (m == 1 && n == 1 && at_end == '.') return 0;
	if (m == 1 && n == 1 && at_end == '?') return 1;
	if (m == 1 && n == 1 && at_end == '#') return 1;
	assert(m > 1, "The cases above are exhaustive.");

	// Counts assuming that the character at_end is . and # respectively.
	let cnt_hole: u32 = 0;
	let cnt_full: u32 = 0;

	if (at_end == '.' || (at_end == '?' && expect_end != '#')) {
		// Assuming that the last character is a ., then the number of
		// options for this map template is the same as the shorter one.
		const k = count_options(map[..m - 1], lens, '?');
		cnt_hole += count_options(map[..m - 1], lens, '?');
	};

	if ((at_end == '#' || (at_end == '?' && expect_end != '.')) && n > 0) {
		// Assuming that the last character is a #, then we consumed one
		// from the final group, so reduce the count there, or remove
		// the group entirely if that makes the group empty.
		if (lens[n - 1] == 1) {
			cnt_full += count_options(map[..m - 1], lens[..n - 1], '.');
		} else {
			assert(lens[n - 1] > 1, "Length should be at least 1.");
			lens[n - 1] -= 1;
			cnt_full += count_options(map[..m - 1], lens, '#');
			lens[n - 1] += 1;
		};
	};

	const debug_print = false;
	if (debug_print) {
		for (let k: size = 0; k < len(map); k += 1) {
			switch (map[k]) {
			case '.' => fmt::printf(".")!;
			case '?' => fmt::printf("?")!;
			case '#' => fmt::printf("#")!;
			};
		};
		switch (expect_end) {
		case '.' => fmt::printf(" [.]")!;
		case '?' => fmt::printf(" [?]")!;
		case '#' => fmt::printf(" [#]")!;
		};
		for (let k: size = 0; k < len(lens); k += 1) fmt::printf(" {}", lens[k])!;
		fmt::printf(" -> .={} + #={} = {}\n", cnt_hole, cnt_full, cnt_hole + cnt_full)!;
	};

	return cnt_hole + cnt_full;
};

export fn main() void = {
	const part = 2;
	const source = os::open("example.txt")!;
	const source = io::drain(source)!;
	const source = strings::fromutf8(source)!;
	const lines: []str = strings::split(source, "\n");
	let result: u32 = 0;

	for (let i: size = 0; i < len(lines); i += 1) {
		if (lines[i] == "") continue;
		const parts: []str = strings::split(lines[i], " ");
		let map: []u8 = strings::toutf8(parts[0]);
		let lenstrs = strings::split(parts[1], ",");

		let lens: []u32 = alloc([0...], len(lenstrs));
		for (let j: size = 0; j < len(lenstrs); j += 1) {
			lens[j] = strconv::stou32(lenstrs[j])!;
		};

		let result_i: u32 = 0;
		if (part == 1) {
			result_i = count_options(map, lens, '?');
		};

		if (part == 2) {
			// For part 2, first we compute the segments repeated
			// 1..5 times, with a # in between. When there's a . in
			// between, those we can compute from the smaller parts
			// because it makes the two sections independent.
			let cnt: [5]u32 = [0...];
			const n = len(map);
			const m = len(lens);
			let map5: []u8 = alloc([0...], n * 5 + 4);
			let lens5: []u32 = alloc([0...], m * 5);
			for (let k: size = 0; k < 5; k += 1) {
				for (let j: size = 0; j < n; j += 1) {
					map5[(n + 1) * k + j] = map[j];
				};
				// For counting, we split only by #, the case
				// where the ? is a . we can easily compute.
				if (k > 0) map5[(n + 1) * k - 1] = '#';
				for (let j: size = 0; j < m; j += 1) {
					lens5[m * k + j] = lens[j];
				};
				cnt[k] = count_options(
					map5[..(n + 1) * (k + 1) - 1],
					lens5[..m * (k + 1)],
					'?',
				);
				fmt::printf("{}: x {}: {}\n", i, k + 1, cnt[k])!;
			};
			// Now it's down to counting the number of ways to
			// combine the above. We have 4 '?'s to place that can
			// be '.' or '#', so as a sanity check, the sum of the
			// factors should be 2^4 = 16.
			const c1 = cnt[0];
			const c2 = cnt[1];
			const c3 = cnt[2];
			const c4 = cnt[3];
			const c5 = cnt[4];
			result_i = (
				0
				+ 1 * c1 * c1 * c1 * c1 * c1
				+ 4 * c2 * c1 * c1 * c1
				+ 3 * c2 * c2 * c1
				+ 3 * c3 * c1 * c1
				+ 2 * c3 * c2
				+ 2 * c4 * c1
				+ 1 * c5
			);
		};

		fmt::printf("{}: {}\n", i, result_i)!;
		result += result_i;
	};
	fmt::printf("Total: {}\n", result)!;
};
