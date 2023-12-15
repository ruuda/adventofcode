// Run with `hare run main.ha` or `hare build -o main main.ha && ./main`.
use fmt;
use io;
use os;
use strings;
use strconv;

fn count_options_dp(map: []u8, lens: []u32) u32 = {
	// We take the dynamic programming approach to computing the number of
	// possibilities. The buffer at location [n, m, c] holds the number of
	// possibilities to fill the map up to length m, by spending n # chars
	// from the start of `lens`, when the last character in `map[..m]` is
	// '.' (c=0) or '#' (c=1) respectively.
	let s: size = 0;
	for (let i: size = 0; i < len(lens); i += 1) {
		s += lens[i];
	};
	const lm = len(map) + 1;
	let buf: []u32 = alloc([0xbadbeef...], (s + 1) * lm * 2);
	defer free(buf);

	const ends: [2]u8 = ['.', '#'];

	// `n` iterates over the number of springs left to place.
	for (let n: size = 0; n <= s; n += 1) {
		// The springs have to be placed in groups, if we are placing
		// the `n`-th spring now, how many other springs are there in
		// the group that it is part of?
		let n_in_group = n;
		for (let i: size = 0; i < len(lens); i += 1) {
			if (n_in_group > lens[i]) {
				n_in_group -= lens[i];
			} else {
				break;
			};
		};
		assert(n_in_group > 0 || n == 0, "Group can only be empty if we have no springs left.");

		// `m` iterates over the size of the map.
		for (let m: size = 1; m <= len(map); m += 1) {
			// `c` iterates over the possible endings of the map.
			for (let c: size = 0; c < len(ends); c += 1) {
				const end = ends[c];
				const k = n * lm * 2 + m * 2 + c;
				const at_end = map[m - 1];
				const end_matches = (at_end == end) || (at_end == '?');

				// Base cases where we cannot place anything.
				if (!end_matches) { buf[k] = 0; continue; };
				if (n > m) { buf[k] = 0; continue; };
				if (n == 0 && end == '#') { buf[k] = 0; continue; };

				if (n == 0 && end == '#') { buf[k] = 0; continue; };
				if (n == 0 && end == '.' && m == 1) { buf[k] = 1; continue; };
				if (n == 1 && end == '.' && m == 1) { buf[k] = 0; continue; };
				if (n == 1 && end == '#' && m == 1) { buf[k] = 1; continue; };
				assert(m > 1, "The cases above are exhaustive for m <= 1.");

				// We have no springs left to place, but that's
				// okay because the ending is '.'.
				if (n == 0 && end == '.') {
					const r_empty = buf[0 * lm * 2 + (m - 1) * 2 + 0];
					//fmt::printf("m={}: n={} c={} -> {}\n", m, n, c, r_empty)!;
					buf[k] = r_empty;
					continue;
				};

				assert(n_in_group >= 1, "At this point we handled all cases of empty groups.");

				// If there is no group here, then both endings
				// are allowed for the recursive case.
				if (end == '.') {
					const r_empty = buf[n * lm * 2 + (m - 1) * 2 + 0];
					const r_full = buf[n * lm * 2 + (m - 1) * 2 + 1];
					buf[k] = r_empty + r_full;
					continue;
				};

				assert(end == '#', "At this point we handled all the cases that end in '.'.");

				// If this is the first '#' in the group, then
				// before it has to be a '.'. But if this is not
				// the first one, then before it has to be a
				// '#'.
				if (n_in_group == 1) {
					const r_empty = buf[(n - 1) * lm * 2 + (m - 1) * 2 + 0];
					buf[k] = r_empty;
				} else {
					const r_full = buf[(n - 1) * lm * 2 + (m - 1) * 2 + 1];
					buf[k] = r_full;
				};
			};
		};
	};

	const debug_print = false;
	if (debug_print) {
		for (let m: size = 1; m <= len(map); m += 1) {
			for (let c: size = 0; c < len(ends); c += 1) {
				for (let i: size = 0; i < len(map); i += 1) {
					if (i >= m) { fmt::printf("_")!; continue; };
					if (map[i] == '.') fmt::printf(".")!;
					if (map[i] == '#') fmt::printf("#")!;
					if (map[i] == '?') fmt::printf("?")!;
				};
				if (c == 0) fmt::printf(" [.]")!;
				if (c == 1) fmt::printf(" [#]")!;
				for (let n: size = 0; n <= s; n += 1) {
					const k = n * lm * 2 + m * 2 + c;
					fmt::printf("   {}: {}", n, buf[k])!;
				};
				fmt::printf("\n")!;
			};
		};
	};
	const r_empty = buf[s * lm * 2 + len(map) * 2 + 0];
	const r_full = buf[s * lm * 2 + len(map) * 2 + 1];
	return r_empty + r_full;
};

export fn main() void = {
	const part = 2;
	const source = os::open("test.txt")!;
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
			result_i = count_options_dp(map, lens);
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
				cnt[k] = count_options_dp(
					map5[..(n + 1) * (k + 1) - 1],
					lens5[..m * (k + 1)],
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
			fmt::printf("{}: (combi) {}\n", i, result_i)!;
			for (let k: size = 1; k < 5; k += 1) {
				map5[(n + 1) * k - 1] = '?';
			};
			result_i = count_options_dp(map5, lens5);
			fmt::printf("{}: (brute) {}\n", i, result_i)!;
		};
		fmt::printf("{}: {}\n", i, result_i)!;

		result += result_i;
	};
	fmt::printf("Total: {}\n", result)!;
};
