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
		const line = lines[i];
		if (len(line) == 0) break;
		const numstrs = strings::split(line, " ");
		let nums: []u32 = alloc([0...], len(numstrs));
		for (let j: size = 0; j < len(numstrs); j += 1) {
			let n = strconv::stou32(numstrs[j])!;
			nums[j] = n;
			fmt::printf("{} ", n)!;
		};
		fmt::printfln("")!;
	};
};
