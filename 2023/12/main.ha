// Run with `hare run main.ha` or `hare build -o main main.ha && ./main`.
use fmt;
use io;
use os;
use strings;
use strconv;

fn count_options(map: []u8, lens: []u32) u32 = {
	let result_i: u32 = 0;

	// The starting positions of each of the springs.
	let start_positions: []u32 = alloc([0...], len(lens));
	let cursor: u32 = 0;

	for (true) {
		const stpos = start_positions[cursor];
		const s_len = lens[cursor];

		let is_good = true;
		let is_fatal = false;

		let remaining_len = lens[cursor];
		for (let c = cursor + 1; c < len(lens); c += 1) {
			remaining_len += 1 + lens[c];
		};
		// If this spring and the ones following it do not fit
		// here, then we are done at this level.
		if (stpos + remaining_len > len(map)) {
			is_fatal = true;
		} else {
			// From the end of the previous spring until the start
			// of this one, there needs to be a gap. If there was
			// not, then we shouldn't even bother trying from here.
			let gap_start: u32 = 0;
			if (cursor > 0) gap_start = start_positions[cursor - 1] + lens[cursor - 1];
			for (let k: u32 = gap_start; k < stpos; k += 1) {
				if (map[k] == '#') {
					is_good = false;
					is_fatal = false;
					break;
				};
			};
		};

		if (is_fatal) {
			// If this was spring 0, then nothing can fit
			// after it, so we are done entirely.
			if (cursor == 0) break;

			// If we were at a later spring, and it does not
			// fit, then we exhausted the possible positions
			// for this one, and we can try moving an
			// earlier one.
			cursor -= 1;
			start_positions[cursor] += 1;
			continue;
		};

		// Then this spring itself should not contain any gaps.
		// We check in reverse to be able to make a larger jump.
		if (is_good) {
			for (let k: u32 = 0; k < s_len; k += 1) {
				const n = stpos + s_len - k - 1;
				if (map[n] == '.') {
					is_good = false;
					// We can jump ahead past the gap.
					start_positions[cursor] = n;
					break;
				};
			};
		};

		// After the final cursor, there should also only be a
		// gap. Again, we iterate backwards so we can skip over as much
		// as possible.
		if (is_good && cursor + 1 == len(lens)) {
			for (let k = len(map) - 1; k >= stpos + s_len; k -= 1) {
				if (map[k] == '#') {
					is_good = false;
					// Subtract one more because we
					// increment at the end of the loop.
					const jump_start = (k: u32) - s_len - 1;
					if (jump_start > start_positions[cursor]) start_positions[cursor] = jump_start;
					break;
				};
			};
		};

		if (is_good) {
			if (cursor + 1 == len(lens)) {
				// This was the last spring we had to
				// place, and it fits. Count one. Next
				// iteration we can still try to move
				// this spring.
				result_i += 1;
			} else {
				// This spring fits, but we have more
				// to place. The next spring has to
				// start after a gap after this one. The
				// end of the loop already increments so
				// we don't add one for the gap here.
				cursor += 1;
				start_positions[cursor] = stpos + s_len;
			};
		};
		// This spring does not fit here, we'll try to
		// move it in the next iteration, and we already
		// moved it over the gap if we encountered one.
		start_positions[cursor] += 1;
	};

	return result_i;
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
			result_i = count_options(map, lens);
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
