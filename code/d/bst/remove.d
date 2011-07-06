module bst.remove;

import bst.node;

import std.typecons;

struct RemoveMaxRet {
  int max;
  Node* root;
}

RemoveMaxRet removeMax(Node* root) {
  assert(root != null);

  auto c = root.c[1];

  if(!c) {
    RemoveMaxRet o = { max: root.value, root: root.c[0] };
    return o;
  }
  else {
    Node* g = root;
    Node* p = c;
    Node* i = c.c[1];

    while (i) {
      g = p;
      p = i;
      i = i.c[1];
    }
    assert(g);
    assert(p);
    assert(!i);
    assert(g.c[1] == p);
    assert(p.c[1] == i);

    g.c[1] = p.c[0];
    int max = p.value;
    //delete p;
    RemoveMaxRet o = { max: max, root: root };
    return o;
  }
}

unittest {
  auto a = new Node(5);
  auto b = removeMax(a);
  assert(b.max == 5);
  assert(b.root == null);
}

unittest {
  auto c = new Node(5);
  c.c[0] = new Node(3);
  auto d = removeMax(c);
  assert(d.max == 5);
  assert(d.root != null);
  assert(d.root.value == 3);
  assert(d.root.c[0] == null);
  assert(d.root.c[1] == null);
}

unittest {
  auto a = new Node(0);
  a.c[1] = new Node(4);
  a.c[1].c[0] = new Node(3);

  auto after = removeMax(a);
  assert(a);
  assert(a.value == 0);
  assert(!a.c[0]);
  assert(a.c[1]);
  assert(a.c[1].value == 3);
  assert(!a.c[1].c[0]);
  assert(!a.c[1].c[1]);
}


Node* removeRoot(Node* root) {
  assert(root != null);

  if (root.c[0] && root.c[1]) {
    RemoveMaxRet r = removeMax(root.c[0]);
    root.value = r.max;
    root.c[0] = r.root;
  }
  else {
    assert(!root.c[0] || !root.c[1]);
    Node* rem = root;
    if (root.c[0]) {
      root = root.c[0];
    }
    else {
      root = root.c[1];
    }
    // delete rem;
  }
  return root;
}

unittest {
  auto a = new Node(5);
  a = removeRoot(a);
  assert(a == null);
}

unittest {
  auto a = new Node(5);
  a.c[0] = new Node(4);
  a = removeRoot(a);
  assert(a);
  assert(a.value == 4);
  assert(!a.c[0]);
  assert(!a.c[1]);
}

unittest {
  auto a = new Node(5);
  a.c[0] = new Node(4);
  a.c[1] = new Node(6);
  a = removeRoot(a);

  assert(a);
  assert(a.value == 4);
  assert(a.c[0] == null);
  assert(a.c[1]);
  assert(a.c[1].value == 6);
  assert(!a.c[1].c[0]);
  assert(!a.c[1].c[1]);
}


Node * remove(Node * root, int value) {
  Node * o;
  if (!root) {
    o = null;
  }
  else if (value == root.value) {
    o = removeRoot(root);
  }
  else {
    o = root;

    Node* p = o;
    int dir = value > p.value;
    Node* i = p.c[dir];

    // iterate until i is the node to delete
    // then set link in p to removeRoot(i).
    while (i && i.value != value) {
      p = i;
      dir = value > p.value;
      i = i.c[dir];
    }
    assert(!i || i.value == value);
    assert(dir == (value > p.value));

    if (i) {
      p.c[dir] = removeRoot(i);
    }
  }

  return o;
}

unittest {
  assert(remove(null, 5) == null);
}
unittest {
  auto a = new Node(5);
  a = remove(a, 5);
  assert(a == null);
}
unittest {
  auto a = new Node(5);
  a = remove(a, 6);
  assert(a);
  assert(a.value == 5);
  assert(!a.c[0]);
  assert(!a.c[1]);
}
unittest {
  auto a = new Node(5);
  a.c[1] = new Node(7);
  a.c[1].c[0] = new Node(6);
  a = remove(a, 7);
  assert(a);
  assert(a.value == 5);
  assert(!a.c[0]);
  assert(a.c[1]);
  assert(a.c[1].value == 6);
  assert(!a.c[1].c[0]);
  assert(!a.c[1].c[1]);
}
