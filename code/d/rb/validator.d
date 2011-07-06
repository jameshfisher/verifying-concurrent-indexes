
enum TreeType { Black, Red, Invalid }

struct TreeInfo {
  TreeType type;
  int height;

  string toString() {
    auto s = (type == TreeType.Black ? "Black" : type == TreeType.Red ? "Red" : "Invalid");
    if(type != TreeType.Invalid) s ~= ", " ~ std.conv.text(height);
    return s;
  }

  void print() { writeln(toString()); }
}

TreeInfo treeType(Node * node) {
  if (node == null) return TreeInfo(TreeType.Black, 0);
  if (!node.black && (red(node.c[0]) || red(node.c[1]))) return TreeInfo(TreeType.Invalid, 0);
  TreeInfo left = treeType(node.c[0]);
  TreeInfo right = treeType(node.c[1]);
  if(left.height != right.height) return TreeInfo(TreeType.Invalid, 0);
  if(node.black) return TreeInfo(TreeType.Black, left.height+1);
  else return TreeInfo(TreeType.Red, left.height);
}

