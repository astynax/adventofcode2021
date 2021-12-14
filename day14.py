#!/usr/bin/env python3

from collections import defaultdict

example = """NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
"""

def main():
    e = read(example.splitlines())
    # print(calc(solve(e, 4)))
    # print(calc(freqs('NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB')))
    with open("Day14.input") as f:
        i = read(f.readlines())
    print(calc(solve(i, 40)))


def read(xs):
    pattern, _, *rs = (x.rstrip() for x in xs)
    return pattern, {k : v for k, v in map(decode, rs)}


def decode(l):
    return l.rstrip().split(" -> ", 1)


def calc(d):
    vs = d.values()
    return max(vs) - min(vs)


def to_keys(s):
    return list(zip(map(''.join, zip(s, s[1:])), [False for _ in s[2:]] + [True]))


def merge(d1, d2, c):
    res = defaultdict(int)
    for k, v in d1.items():
        res[k] += v
    for k, v in d2.items():
        res[k] += v
    if c is not None:
        res[c] -= 1
    return res


def freqs(s):
    res = defaultdict(int)
    for c in s:
        res[c] += 1
    return res


def solve(inp, steps):
    pattern, rs = inp
    cache = {}
    def step(k, n):
        print(' '*n + k)
        if n == 0:
            return freqs(k)
        ss = cache.setdefault(k, {})
        if n in ss:
            return ss[n]
        c1, c2 = k
        c = rs[k]
        fs1 = step(c1 + c, n - 1)
        fs2 = step(c + c2, n - 1)
        fs = merge(fs1, fs2, c)
        ss[n] = fs
        return fs
    res = {}
    for k, fs, is_last in ((k, step(k, steps), l) for k, l in to_keys(pattern)):
        res = merge(res, fs, k[1] if not is_last else None)
    return res


if __name__ == '__main__':
    main()
