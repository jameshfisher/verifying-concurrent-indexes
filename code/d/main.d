#!/usr/bin/rdmd -debug

module main;

import std.stdio;
import std.conv;

static import test;

static import rb.index;
static import rb.print;
static import rb.validate;


void main() {
  test.testAgainstReference!
    (rb.index.RbIndex)
    (&rb.print.print, &rb.validate.validate,
     100, 1000, true);
}
