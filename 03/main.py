#!/usr/bin/env python3

import functools

offs = [1, 3, 5, 7, 0.5]
xs = [0 for _ in offs]
ns = [0 for _ in offs]

for line in open('input.txt', 'r', encoding='ascii'):
    for i, x in enumerate(xs):
        ns[i] += int(x) == x and line[int(x)] == '#'

    xs = [(x + off) % len(line.strip()) for x, off in zip(xs, offs)]

for off, n in zip(offs, ns):
    print(off, n)

print('Product:', functools.reduce(lambda x, y: x * y, ns))
