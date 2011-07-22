module array.print;

import array.index;

import std.stdio;


void print(ArrayIndex i) {
  write("{ ");
  foreach (k; i.contents()) {
    writef("%d ", k);
  }
  writeln("}");
}
