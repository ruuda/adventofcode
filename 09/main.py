#!/usr/bin/env python

past = []
bad_num: int

numbers = [int(line) for line in open('input.txt', 'r', encoding='ascii')]

# Part 1
for n in numbers:
    is_ok = False

    for k in past:
        z = n - k
        if k != z and z in past:
            is_ok = True
            break

    if not is_ok and len(past) == 25:
        bad_num = n
        print(n)

    past.append(n)
    past = past[-25:]

# Part 2
for i, n in enumerate(numbers):
    s = 0
    for j, k in enumerate(numbers[i:]):
        s += k
        if s == bad_num and j > 0:
            rg = numbers[i:i + j]
            print(min(rg) + max(rg))
