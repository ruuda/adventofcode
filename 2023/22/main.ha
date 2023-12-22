// Run with `hare run main.ha` or `hare build -o main main.ha && ./main`.
use fmt;
use io;
use os;
use strings;
use strconv;

type brick = struct {
	x0: u32,
	y0: u32,
	z0: u32,
	x1: u32,
	y1: u32,
	z1: u32,
};

// Ensure that the 0 coordinates are no greater than the 1 coordinates.
fn reorient(b: *brick) void = {
	if (b.x0 > b.x1) { const tmp = b.x1; b.x1 = b.x0; b.x0 = tmp; };
	if (b.y0 > b.y1) { const tmp = b.y1; b.y1 = b.y0; b.y0 = tmp; };
	if (b.z0 > b.z1) { const tmp = b.z1; b.z1 = b.z0; b.z0 = tmp; };
};

fn dx_dy_dz(b: brick) (u32, u32, u32) = {
	let dx: u32 = if (b.x1 > b.x0) 1 else 0;
	let dy: u32 = if (b.y1 > b.y0) 1 else 0;
	let dz: u32 = if (b.z1 > b.z0) 1 else 0;
	return (dx, dy, dz);
};

// Return whether the brick can move down one position.
fn can_down(wh: u32, w: u32, tower: []u16, b: brick) bool = {
	let (dx, dy, dz) = dx_dy_dz(b);
	let (x, y, z) = (b.x0, b.y0, b.z0);
	let is_empty = (tower[(z - 1) * wh + y * w + x] == 0);

	// For a brick standing up, we would find the cells to not be empty,
	// because the brick itself is there, but it doesn't matter, we only
	// need to check under it.
	if (dz == 1) return is_empty;

	for (is_empty) {
		x += dx;
		y += dy;
		is_empty = (tower[(z - 1) * wh + y * w + x] == 0);
		if (x == b.x1 && y == b.y1) break;
	};

	return is_empty;
};

// Write `mark` to the cells that the brick occupies in the tower.
fn mark_brick(wh: u32, w: u32, tower: []u16, b: brick, mark: u16) void = {
	let (dx, dy, dz) = dx_dy_dz(b);
	let (x, y, z) = (b.x0, b.y0, b.z0);
	tower[z * wh + y * w + x] = mark;
	for (true) {
		x += dx;
		y += dy;
		z += dz;
		tower[z * wh + y * w + x] = mark;
		if (x == b.x1 && y == b.y1 && z == b.z1) break;
	};
};

export fn main() void = {
	const source = os::open("example.txt")!;
	const source = io::drain(source)!;
	const source = strings::fromutf8(source)!;
	const lines: []str = strings::split(source, "\n");
	
	let b0: brick = brick {
		x0 = 0, y0 = 0, z0 = 0, x1 = 0, y1 = 0, z1 = 0,
	};
	let bricks: []brick = alloc([b0...], len(lines));
	let max_x: u32 = 0;
	let max_y: u32 = 0;
	let max_z: u32 = 0;

	for (let i: size = 0; i < len(lines) - 1; i += 1) {
		// We start counting bricks at 1 so we can use index 0 as a
		// sentinel for "no brick".
		const j = i + 1;
		const coords: []str = strings::split(lines[i], "~");
		const coords0 = strings::split(coords[0], ",");
		const coords1 = strings::split(coords[1], ",");
		bricks[j].x0 = strconv::stou32(coords0[0])!;
		bricks[j].y0 = strconv::stou32(coords0[1])!;
		bricks[j].z0 = strconv::stou32(coords0[2])!;
		bricks[j].x1 = strconv::stou32(coords1[0])!;
		bricks[j].y1 = strconv::stou32(coords1[1])!;
		bricks[j].z1 = strconv::stou32(coords1[2])!;
		reorient(&bricks[j]);
		if (bricks[j].x1 > max_x) max_x = bricks[j].x1;
		if (bricks[j].y1 > max_y) max_y = bricks[j].y1;
		if (bricks[j].z1 > max_z) max_z = bricks[j].z1;
	};

	for (let i: size = 1; i < len(bricks); i += 1) {
		let b = bricks[i];
		fmt::printf("{},{},{}~{},{},{}\n", b.x0, b.y0, b.z0, b.x1, b.y1, b.z1)!;
	};
	fmt::printf("max x, y, z: {}, {}, {}\n", max_x, max_y, max_z)!;

	// Make a 3D array that for every position in space holds the index of
	// the brick that's there.
	const w = max_x + 1;
	const h = max_y + 1;
	const d = max_z + 1;
	const wh = w * h;
	let tower: []u16 = alloc([0...], w * h * d);

	for (let i: u16 = 1; i < len(bricks); i += 1) {
		let b = bricks[i];
		mark_brick(wh, w, tower, b, i);
	};

	// Iterate over all coordinates in the tower, from low to high, and if
	// we find an empty cell, try to move bricks above it down.
	for (let z: u32 = 1; z < d; z += 1) {
		fmt::printf("Looking at layer z={}.\n", z)!;
		for (let y: u32 = 0; y < h; y += 1) {
			for (let x: u32 = 0; x < w; x += 1) {
				// Is this space empty? Could something above us
				// move into it? If not, move on.
				const bi = tower[wh * z + w * y + x];
				if (bi > 0) continue;

				// Look up. Is there a brick there that should
				// fall down?
				for (let z1: u32 = z + 1; z1 < d; z1 += 1) {
					const bi = tower[wh * z1 + w * y + x];
					if (bi == 0) continue;
					let b: *brick = &bricks[bi];
					fmt::printf("  Found brick {} at {}, {}, {}.\n", bi, x, y, z1)!;

					// When we find a brick, move it down
					// until it can no longer move down.
					for (can_down(wh, w, tower, *b)) {
						mark_brick(wh, w, tower, *b, 0);
						b.z0 -= 1;
						b.z1 -= 1;
						mark_brick(wh, w, tower, *b, bi);
						fmt::printf("    Moved down to z={}.\n", b.z0)!;
					};
				};
			};
		};
	};
};