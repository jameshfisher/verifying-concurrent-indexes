module rb.index;

static import index;

static import rb.node;
static import rb.search;
static import rb.insert;
static import rb.remove;
static import rb.contents;

class RbIndex : index.Index {
  rb.node.Node * root;

  bool search(int value) {
    return rb.search.search_rec(this.root, value);
  }

  void insert(int value) {
    this.root = rb.insert.insert_top_down_iter(this.root, value);
    this.root.black = true;
  }

  void remove(int value) {
    this.root = rb.remove.remove_push_down_iter(this.root, value);
  }

  int[] contents() {
    return rb.contents.contents(root);
  }
}
