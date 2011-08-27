module ll.print;

static import ll.node;
static import ll.index;

import std.stdio;

void printNode(ll.node.Node* n) {
  if (n) {
    write("─→");
    write(n.value);
    printNode(n.tail);
  }
}

void print(ll.index.LlIndex i) {
  printNode(i.root);
  writeln();
}