#!/usr/bin/env python3

numbers = set()

for line in open('input.txt', 'r', encoding='ascii'):
    n = int(line)
    m = 2020 - n
    if m in numbers:
        print(n * m)
    else:
        numbers.add(n)
