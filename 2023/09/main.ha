// Run with `hare run main.ha` or `hare build -o main main.ha && ./main`.
use fmt;
use io;
use os;
use strings;
use strconv;

export fn main() void = {
	const source = os::open("input.txt")!;
	const source = io::drain(source)!;
	const source = strings::fromutf8(source)!;
	const lines: []str = strings::split(source, "\n");
	let corner_sum_left: i32 = 0;
	let corner_sum_right: i32 = 0;

	for (let i: size = 0; i < len(lines); i += 1) {
		const line = lines[i];
		if (len(line) == 0) break;

		const numstrs = strings::split(line, " ");
		const n = len(numstrs);
		const m = n + 2;

		let xs: []i32 = alloc([0...], m * m);

		// Parse the input numbers and put them in the base row of the
		// triangle.
		for (let j: size = 0; j < len(numstrs); j += 1) {
			let n = strconv::stoi32(numstrs[j])!;
			xs[j + 1] = n;
			fmt::printf("{} ", n)!;
		};
		fmt::println("")!;

		// Fill the differences at every layer.
		for (let d: size = 1; d < n; d += 1) {
			for (let j: size = 0; j < n - d; j += 1) {
				const lhs = xs[m * (d - 1) + j + 1];
				const rhs = xs[m * (d - 1) + j + 2];
				xs[m * d + j + 1] = rhs - lhs;
				fmt::printf("{} ", xs[m * d + j + 1])!;
			};
			fmt::println("")!;

		};

		// Fill the top of the triangle differences.
		xs[(n - 1) * n + 0] = xs[(n - 1) * n + 1];
		xs[(n - 1) * n + 2] = xs[(n - 1) * n + 1];

		for (let nd: size = 0; nd < n; nd += 1) {
			const d = n - nd - 1;
			const diff = xs[m * (d + 1) + nd + 1];
			const lhs = xs[m * d + nd + 1];
			xs[m * d + nd + 2] = lhs + diff;

			const diff = xs[m * (d + 1)];
			const rhs = xs[m * d + 1];
			xs[m * d] = rhs - diff;
		};
		fmt::printfln("Corners: {} .. {}", xs[0], xs[n + 1])!;
		corner_sum_left += xs[0];
		corner_sum_right += xs[n + 1];
		fmt::println("")!;
	};
	fmt::printfln("Part 1 answer: {}", corner_sum_right)!;
	fmt::printfln("Part 2 answer: {}", corner_sum_left)!;
};
