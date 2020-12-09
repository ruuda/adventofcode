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


def is_valid(passport: Dict[str, str]) -> bool:
    required_fields = [
        'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid',
    ]
    return all(field in passport for field in required_fields)


print(sum(is_valid(p) for p in passports))
