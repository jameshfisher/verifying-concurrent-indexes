/*
  Braindead implementation of an index (in a given range)
  that's pretty much guaranteed to be correct.
  Used for testing against.
*/

module array.index;

import index;

import std.stdio;
import std.algorithm;

class ArrayIndex : index.Index {
  bool[int] arr;
  this() {}

  bool search(int value) {
    return arr.get(value, false);
  }

  void insert(int value) {
    arr[value] = true;
  }

  void remove(int value) {
    arr.remove(value);
  }

  int[] contents() {
    auto keys = arr.keys;
    sort!((a,b){return a < b;})(keys);
    return keys;
  }
}
