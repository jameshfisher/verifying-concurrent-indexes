module bst.node;

struct Node {
  int value;
  Node*[2] c;

  this(int value) {
    this.value = value;
  }
}
