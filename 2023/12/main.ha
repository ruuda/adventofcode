// Run with `hare run main.ha` or `hare build -o main main.ha && ./main`.
use fmt;
use io;
use os;
use strings;
use strconv;

export fn main() void = {
	const source = os::open("example.txt")!;
	const source = io::drain(source)!;
	const source = strings::fromutf8(source)!;
	const lines: []str = strings::split(source, "\n");
	let result: u32 = 0;
	let result_i: u32 = 0;

	for (let i: size = 0; i < len(lines); i += 1) {
		if (lines[i] == "") continue;
		const parts: []str = strings::split(lines[i], " ");
		const map: []u8 = strings::toutf8(parts[0]);
		const lenstrs = strings::split(parts[1], ",");
		let lens: []u32 = alloc([0...], len(lenstrs));
		for (let j: size = 0; j < len(lenstrs); j += 1) {
			lens[j] = strconv::stou32(lenstrs[j])!;
		};

		// The starting positions of each of the springs.
		let start_positions: []u32 = alloc([0...], len(lens));
		let cursor: u32 = 0;

		for (true) {
			const stpos = start_positions[cursor];
			const s_len = lens[cursor];
			let is_good = true;

			if (stpos + s_len > len(map)) {
				// This spring does not fit here, we are done
				// at this level. If this was spring 0, then
				// nothing can fit after it, so we are done
				// entirely.
				if (cursor == 0) break;

				// If we were at a later spring, and it does not
				// fit, then we exhausted the possible positions
				// for this one, and we can try moving an
				// earlier one.
				cursor -= 1;
				start_positions[cursor] += 1;
				continue;
			};

			// Loop over the presumed position of the spring that
			// the cursor points at. If we find a # or ? that's
			// okay, but if we find a '.', then the spring is not
			// here, and the next place it could be is after.
			for (let k: u32 = stpos; k < stpos + s_len; k += 1) {
				if (map[k] == '.') {
					is_good = false;
					start_positions[cursor] = k;
					break;
				};
			};
			// After the spring, there needs to be a gap, or we need
			// to be at the end. The gap might be a ?.
			if ((len(map) < stpos + s_len) && (map[stpos + s_len] == '#')) {
				is_good = false;
				start_positions[cursor] = stpos + s_len;
			};

			if (is_good) {
				if (cursor + 1 == len(lens)) {
					// This was the last spring we had to
					// place, and it fits, so count one way
					// in which we can place the springs.
					// In the next iteration, we can try to
					// move this spring still.
					result_i += 1;
					fmt::printf("{}: Found:", i)!;
					for (let z: size = 0; z < len(lens); z += 1) {
						fmt::printf(" {}", start_positions[z])!;
					};
					fmt::printf("\n")!;
				} else {
					// This spring fits, but we have more
					// to place. The next spring has to
					// start after a gap after this one. The
					// start of the loop increments so we
					// don't add one for the gap here.
					cursor += 1;
					start_positions[cursor] = stpos + s_len;
				};
			};
			// This spring does not fit here, we'll try to
			// move it in the next iteration, and we already
			// moved it over the gap if we encountered one.
			start_positions[cursor] += 1;
		};

		fmt::printf("{}: {}\n", i, result_i)!;
		result += result_i;
		result_i = 0;
	};
	fmt::printf("Total: {}\n", result)!;
};
