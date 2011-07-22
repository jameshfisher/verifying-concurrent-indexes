module rb.node;

struct Node {
  int value;
  Node*[2] c;
  bool black;

  this(int value) {
    this.value = value;
    this.c[0] = this.c[1] = null;
    this.black = false;
  }
}


bool red(Node * node) {
  return node != null && node.black == false;
}

