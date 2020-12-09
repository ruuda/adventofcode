#!/usr/bin/env python3

from typing import Dict

passports = []
passport = {}

for line in open('input.txt', 'r', encoding='ascii'):
    if line == '\n':
        passports.append(passport)
        passport = {}
    else:
        for pair in line.split():
            k, v = pair.split(':', maxsplit=1)
            passport[k] = v

passports.append(passport)

for p in passports:
    print(p)
