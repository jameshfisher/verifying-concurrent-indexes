module rb.validate;

import rb.node;
import rb.index;

enum TreeType { Black, Red, Invalid }

struct TreeInfo {
  TreeType type;
  int height;

  /*
  string toString() {
    auto s = (type == TreeType.Black ? "Black" : type == TreeType.Red ? "Red" : "Invalid");
    if(type != TreeType.Invalid) s ~= ", " ~ std.conv.text(height);
    return s;
  }

  void print() { writeln(toString()); }*/
}


TreeInfo treeType(Node * node, int min, int max) {
  TreeInfo o;

  if (node == null) {
    o = TreeInfo(TreeType.Black, 0);
  }
  else {
    if (node.value < min || node.value > max) {
      o = TreeInfo(TreeType.Invalid, 0);
    }
    else {
      if (!node.black && (red(node.c[0]) || red(node.c[1]))) {
        o = TreeInfo(TreeType.Invalid, 0);
      }
      else {
        TreeInfo left = treeType(node.c[0], min, node.value-1);
        TreeInfo right = treeType(node.c[1], node.value+1, max);

        if(left.height != right.height) {
          o = TreeInfo(TreeType.Invalid, 0);
        }
        else {
          if(node.black) {
            o = TreeInfo(TreeType.Black, left.height+1);
          }
          else {
            o = TreeInfo(TreeType.Red, left.height);
          }
        }
      }
    }
  }

  return o;
}


bool validate(rb.index.RbIndex i) {
  TreeInfo info = treeType(i.root, int.min, int.max);
  return info.type == TreeType.Black;
}
