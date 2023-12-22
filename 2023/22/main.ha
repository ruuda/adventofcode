// Run with `hare run main.ha` or `hare build -o main main.ha && ./main`.
use fmt;
use io;
use os;
use strings;
use strconv;

type block = struct {
	x0: u32,
	y0: u32,
	z0: u32,
	x1: u32,
	y1: u32,
	z1: u32,
};

export fn main() void = {
	const source = os::open("example.txt")!;
	const source = io::drain(source)!;
	const source = strings::fromutf8(source)!;
	const lines: []str = strings::split(source, "\n");
	
	let b0: block = block {
		x0 = 0, y0 = 0, z0 = 0, x1 = 0, y1 = 0, z1 = 0,
	};
	let blocks: []block = alloc([b0...], len(lines) - 1);
	let max_x: u32 = 0;
	let max_y: u32 = 0;
	let max_z: u32 = 0;

	for (let i: size = 0; i < len(lines) - 1; i += 1) {
		const coords: []str = strings::split(lines[i], "~");
		const coords0 = strings::split(coords[0], ",");
		const coords1 = strings::split(coords[1], ",");
		blocks[i].x0 = strconv::stou32(coords0[0])!;
		blocks[i].y0 = strconv::stou32(coords0[1])!;
		blocks[i].z0 = strconv::stou32(coords0[2])!;
		blocks[i].x1 = strconv::stou32(coords1[0])!;
		blocks[i].y1 = strconv::stou32(coords1[1])!;
		blocks[i].z1 = strconv::stou32(coords1[2])!;
		if (blocks[i].x0 > max_x) max_x = blocks[i].x0;
		if (blocks[i].x1 > max_x) max_x = blocks[i].x1;
		if (blocks[i].y0 > max_y) max_y = blocks[i].y0;
		if (blocks[i].y1 > max_y) max_y = blocks[i].y1;
		if (blocks[i].z0 > max_z) max_z = blocks[i].z0;
		if (blocks[i].z1 > max_z) max_z = blocks[i].z1;
	};

	for (let i: size = 0; i < len(blocks); i += 1) {
		let b = blocks[i];
		fmt::printf("{},{},{}~{},{},{}\n", b.x0, b.y0, b.z0, b.x1, b.y1, b.z1)!;
	};
};
