#!/usr/bin/env python3

from typing import List, Iterable, Tuple

grid = [
    [ord(x) - ord("0") for x in line.strip()]
    for line in open("input.txt", "r", encoding="utf-8")
]

h = len(grid)
w = len(grid[0])

Point = Tuple[int, int]


def iter_visible(dy: int, dx: int, inits: List[Point]) -> Iterable[Point]:
    for y, x in inits:
        z = -1
        while y >= 0 and x >= 0 and y < h and x < w:
            if grid[y][x] > z:
                z = grid[y][x]
                yield y, x
            y += dy
            x += dx


vis_x_pos = iter_visible(0, 1, [(y, 0) for y in range(h)])
vis_x_neg = iter_visible(0, -1, [(y, w - 1) for y in range(h)])
vis_y_pos = iter_visible(1, 0, [(0, x) for x in range(w)])
vis_y_neg = iter_visible(-1, 0, [(h - 1, x) for x in range(w)])

all_visible = set(vis_x_pos) | set(vis_x_neg) | set(vis_y_pos) | set(vis_y_neg)
print("Part 1:", len(all_visible))


def viewing_distance(p: Point, dy: int, dx: int) -> int:
    y, x = p
    n = 0
    z = grid[y][x]
    while True:
        y += dy
        x += dx
        if y < 0 or x < 0 or y >= h or x >= w:
            return n
        n += 1
        if grid[y][x] >= z:
            return n


def scenic_score(p: Point) -> int:
    return (
        1
        * viewing_distance(p, -1, 0)
        * viewing_distance(p, 0, -1)
        * viewing_distance(p, 0, 1)
        * viewing_distance(p, 1, 0)
    )


max_score = max(scenic_score((y, x)) for y in range(h) for x in range(w))
print("Part 2:", max_score)
