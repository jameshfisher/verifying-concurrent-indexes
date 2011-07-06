module rb.print;

import rb.node;
import rb.index;

import std.stdio;
import std.c.stdio;

void printNode(Node * node, int depth = 0, string pre = "──") {

  void writeColor(string s, string mode) {

    void ansi(string m) {
      std.c.stdio.printf("%c", 27);
      writef("[%sm", m);
    }

    ansi(mode);
    write(s);
    ansi("0");
  }

  if (node != null) {
    printNode(node.c[1], depth+1, "╭╴");
    for (int i = depth; i > 0; i--) { write("  "); }
    write(pre);
    writeColor(std.conv.text(node.value), (node.black ? "0": "31"));
    writeln();
    printNode(node.c[0], depth+1, "╰╴");
  }
}

void print(RbIndex i) {
  printNode(i.root);
}
