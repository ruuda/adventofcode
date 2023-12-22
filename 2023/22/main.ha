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

// Ensure that the 0 coordinates are no greater than the 1 coordinates.
fn reorient(b: *block) void = {
	if (b.x0 > b.x1) { const tmp = b.x1; b.x1 = b.x0; b.x0 = tmp; };
	if (b.y0 > b.y1) { const tmp = b.y1; b.y1 = b.y0; b.y0 = tmp; };
	if (b.z0 > b.z1) { const tmp = b.z1; b.z1 = b.z0; b.z0 = tmp; };
};

export fn main() void = {
	const source = os::open("example.txt")!;
	const source = io::drain(source)!;
	const source = strings::fromutf8(source)!;
	const lines: []str = strings::split(source, "\n");
	
	let b0: block = block {
		x0 = 0, y0 = 0, z0 = 0, x1 = 0, y1 = 0, z1 = 0,
	};
	let blocks: []block = alloc([b0...], len(lines));
	let max_x: u32 = 0;
	let max_y: u32 = 0;
	let max_z: u32 = 0;

	for (let i: size = 0; i < len(lines) - 1; i += 1) {
		// We start counting blocks at 1 so we can use index 0 as a
		// sentinel for "no block".
		const j = i + 1;
		const coords: []str = strings::split(lines[i], "~");
		const coords0 = strings::split(coords[0], ",");
		const coords1 = strings::split(coords[1], ",");
		blocks[j].x0 = strconv::stou32(coords0[0])!;
		blocks[j].y0 = strconv::stou32(coords0[1])!;
		blocks[j].z0 = strconv::stou32(coords0[2])!;
		blocks[j].x1 = strconv::stou32(coords1[0])!;
		blocks[j].y1 = strconv::stou32(coords1[1])!;
		blocks[j].z1 = strconv::stou32(coords1[2])!;
		reorient(&blocks[j]);
		if (blocks[j].x1 > max_x) max_x = blocks[j].x1;
		if (blocks[j].y1 > max_y) max_y = blocks[j].y1;
		if (blocks[j].z1 > max_z) max_z = blocks[j].z1;
	};

	for (let i: size = 1; i < len(blocks); i += 1) {
		let b = blocks[i];
		fmt::printf("{},{},{}~{},{},{}\n", b.x0, b.y0, b.z0, b.x1, b.y1, b.z1)!;
	};
	fmt::printf("max x, y, z: {}, {}, {}\n", max_x, max_y, max_z)!;

	// Make a 3D array that for every position in space holds the index of
	// the block that's there.
	const w = max_x + 1;
	const h = max_y + 1;
	const d = max_z + 1;
	const wh = w * h;
	let tower: []u16 = alloc([0...], w * h * d);

	for (let i: u16 = 1; i < len(blocks); i += 1) {
		let b = blocks[i];
		let dx: u32 = if (b.x1 > b.x0) 1 else 0;
		let dy: u32 = if (b.y1 > b.y0) 1 else 0;
		let dz: u32 = if (b.z1 > b.z0) 1 else 0;
		let x = b.x0;
		let y = b.y0;
		let z = b.z0;
		tower[z * wh + y * w + x] = i;
		for (true) {
			x += dx;
			y += dy;
			z += dz;
			tower[z * wh + y * w + x] = i;
			if (x == b.x1 && y == b.y1 && z == b.z1) break;
		};
	};
};
