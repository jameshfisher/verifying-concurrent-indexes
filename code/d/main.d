#!/usr/bin/rdmd -debug

module main;

import std.stdio;
import std.conv;

static import test;

static import rb.index;
static import array.index;

static import rb.print;
static import array.print;

void main() {
  test.testAgainstReference!
    (array.index.ArrayIndex, rb.index.RbIndex)
    (100, 1000, true, &array.print.print, &rb.print.print);
}
