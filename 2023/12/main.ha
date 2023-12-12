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

	for (let i: size = 0; i < len(lines); i += 1) {
		const parts: []str = strings::split(lines[i], " ");
		const map: str = parts[0];
		const lenstrs = strings::split(parts[1], ",");
		let lens: []u32 = alloc([0...], len(lenstrs));
		for (let j: size = 0; j < len(lenstrs); j += 1) {
			lens[j] = strconv::stou32(lenstrs[j])!;
		};

		fmt::printf("{} {}\n", map, len(lens))!;
	};
};
