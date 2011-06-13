#!/usr/bin/env python

from math import floor 

red = 0
black = 1

left = 0
right = 1

def repeat(c, n):
    s = ""
    while n > 0:
        s += c
        n -= 1
    return s

def pad(s, l, c):
    e = l - len(s)
    return s + repeat(c, e)

def padcenter(s, l, c):
    e = l - len(s)
    left = floor(e/2)
    right = e - left
    return repeat(c, left) + s + repeat(c, right)

def join(l, r, w):
    l = l.split("\n")
    r = r.split("\n")
    height = max(len(l), len(r))
    o = ""
    for i in range(height):
        o += pad((l[i] if i < len(l) else ""), w, " ")
        o += pad((r[i] if i < len(r) else ""), w, " ")
        o += "\n"
    return o

def turn(d):
    if d == left:
        return right
    else:
        return left

def col(c):
    if c == red:
        return "R"
    else:
        return "B"


class Node:
    def __init__(self, color, value):
        self.color = color
        self.value = value
        self.link = [sentinel, sentinel]

    def insert(self, value):
        # returns a tree that may have a red violation at the top.

        d = right
        if value < self.value:
            d = left

        self.link[d] = self.link[d].insert(value)

        if self.link[d].color == black:
            # if the color is black we're fine,
            return self
        else:
            # but here we have possible red violations

            if self.link[turn(d)].color == red:
                # uncle is red; we are black; color swap
                self.color = red
                self.link[left].color = self.link[right].color = black
                return self
            else:
                # parent red; uncle black.
                if self.link[d].link[d].color == red:
                    return self.rotate(turn(d))
                elif self.link[d].link[turn(d)].color == red:
                    return self.double(turn(d))
                return self


    def rotate(self, d):
        """Rotate in direction d; return new root."""
        newroot = self.link[turn(d)]

        self.link[turn(d)] = newroot.link[d]
        newroot.link[d] = self

        self.color = red
        newroot.color = black

        return newroot

    def double(self, d):
        self.link[turn(d)] = self.link[turn(d)].rotate(turn(d));
        return self.rotate(d);

    def __str__(self):
        return self.tr(32)

    def tr(self, n):
        return repeat(" ", n) + padcenter(col(self.color) + "," + str(self.value), n*2, "-") + "\n" + join(self.link[left].tr(n/2), self.link[right].tr(n/2), n*2)
    

class Sentinel:
    def __init__(self):
        self.color = black

    def insert(self, value):
        return Node(red, value)

    def __str__(self):
        return "o"

    def tr(self, n):
        return padcenter("o", n*4, " ")


sentinel = Sentinel()


class Tree:
    def __init__(self):
        self.root = sentinel

    def insert(self, value):
        self.root = self.root.insert(value)
        self.root.color = black

    def __str__(self):
        return self.root.__str__()


a = Tree()
for i in [5,3,1,4,3.5,3.7]:
    a.insert(i)
print a
