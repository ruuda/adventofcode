#!/usr/bin/env python

past = []

for line in open('input.txt', 'r', encoding='ascii'):
    n = int(line)

    is_ok = False

    for k in past:
        z = n - k
        if k != z and z in past:
            is_ok = True
            break

    if not is_ok and len(past) == 25:
        print(n)

    past.append(n)
    past = past[-25:]
