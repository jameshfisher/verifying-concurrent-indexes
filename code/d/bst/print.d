module bst.print;

static import bst.node;
static import bst.index;

import std.stdio;

void printNode(bst.node.Node * node, int depth = 0, string pre = "─→") {
  if (node != null) {
    printNode(node.c[1], depth+1, "╭→");
    for(int i = depth; i > 0; i--) { write("  "); }
    write(pre);
    writeln(std.conv.text(node.value));
    printNode(node.c[0], depth+1, "╰→");
  }
}

void print(bst.index.BstIndex i) {
  printNode(i.root);
}
