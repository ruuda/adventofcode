#!/usr/bin/env

x = 0
slope = 3
n = 0

for line in open('input.txt', 'r', encoding='ascii'):
    n += line[x] == '#'
    x = (x + slope) % len(line.strip())

print(n)
