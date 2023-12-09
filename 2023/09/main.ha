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
	let corner_sum: i32 = 0;

	for (let i: size = 0; i < len(lines); i += 1) {
		const line = lines[i];
		if (len(line) == 0) break;

		const numstrs = strings::split(line, " ");
		const n = len(numstrs);

		let xs: []i32 = alloc([0...], (n + 1) * (n + 1));

		// Parse the input numbers and put them in the base row of the
		// triangle.
		for (let j: size = 0; j < len(numstrs); j += 1) {
			let n = strconv::stoi32(numstrs[j])!;
			xs[j] = n;
			fmt::printf("{} ", n)!;
		};
		fmt::println("")!;

		// Fill the differences at every layer.
		for (let d: size = 1; d < n; d += 1) {
			for (let j: size = 0; j < n - d; j += 1) {
				const lhs = xs[n * (d - 1) + j];
				const rhs = xs[n * (d - 1) + j + 1];
				xs[n * d + j] = rhs - lhs;
				fmt::printf("{} ", xs[n * d + j])!;
			};
			fmt::println("")!;

		};

		xs[(n - 1) * n + 1] = xs[(n - 1) * n];

		for (let nd: size = 0; nd < n; nd += 1) {
			const d = n - nd - 1;
			const diff = xs[n * (d + 1) + nd];
			const lhs = xs[n * d + nd];
			xs[n * d + nd + 1] = lhs + diff;
		};
		fmt::printfln("Corner: {}", xs[n])!;
		corner_sum += xs[n];
		fmt::println("")!;
	};
	fmt::printfln("Part 1 answer: {}", corner_sum)!;
};
