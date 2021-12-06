#!/usr/bin/env python3

pairs = {}
numbers = [int(line) for line in open('input.txt', 'r', encoding='ascii')]

for n in numbers:
    for k in numbers:
        pairs[n + k] = n * k

for n in numbers:
    m = 2020 - n
    if m in pairs:
        print(n * pairs[m])
