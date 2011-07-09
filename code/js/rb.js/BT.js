
var LEFT = true;
var RIGHT = false;

function Node(value, left, right, parent) {
  this.value = value;
  this.children = {
    true: left || null,  // all values < value
    false: right || null // all values < value
  };
  this.parent = parent;
}

exports.Tree = function Tree() {
  this.nil = new Node(0, null, null, null);
  this.root = this.nil;
}


function nodearr(node, tree) {
  if(node === tree.nil || node === null) {
    return [];
  }
  else {
    return nodearr(node.children[LEFT], tree).concat([node.value]).concat(nodearr(node.children[RIGHT], tree));
  }
}

exports.arr = arr = function(tree) {
  return nodearr(tree.root, tree);
}


function searchNode(tree, value) {
  var node = tree.root;

  while(node !== tree.nil && node.value !== value) {
    node = node.children[value < node.value];
  }

  return node;
}


exports.search = function(tree, value) {
  return searchNode(tree, value) !== tree.nil;
}

exports.insert = function insert(tree, value) {
  var parent = tree.nil;
  var x = tree.root;
  while (x !== tree.nil && x.value !== value) {
    parent = x;
    x = x.children[value < x.value];
  }
  // x === tree.nil || x.value === value
  if(x !== tree.nil) {
    // x.value === value; do nothing
  }
  else {
    // x === tree.nil
    if (parent === tree.nil) {
      // The tree was empty
      tree.root = new Node(value, tree.nil, tree.nil, tree.nil);
    }
    else {
      parent.children[value < parent.value] = new Node(value, tree.nil, tree.nil, parent);
    }
  }
}


function minimum(tree, node) {
  while(node.children[LEFT] !== tree.nil) {
    node = node.children[LEFT];
  }
  return node;
}


function successor(tree, node) {
  var o;
  if(node.children[RIGHT] !== tree.nil) {
    o = minimum(tree, node.children[RIGHT]);
  }
  else {
    o = node.parent;
    while (o !== tree.nil && node === o.children[RIGHT]) {
      node = o;
      o = o.parent;
    }
  }
  return o;
}


function removeNode(tree, node) {
  var d;
  if(node.children[LEFT] === tree.nil || node.children[RIGHT] === tree.nil) {
    d = node;
  }
  else {
    // node.children[LEFT] !== tree.nil && node.children[RIGHT] !== tree.nil
    d = successor(tree, node);
  }
  // d has at least one tree.nil child

  var x;
  if(d.children[LEFT] !== tree.nil) {
    // d.children[LEFT] !== tree.nil
    x = d.children[LEFT];
    // x !== tree.nil
  }
  else {
    // d.children[LEFT] === tree.nil
    x = d.children[RIGHT];
    // x === d.children[RIGHT]
  }

  // x is the non-nil child of d if there is one; else tree.nil

  if(x !== tree.nil) {
    // x is the non-nil child of d
    // splice x into place of d
    x.parent = d.parent;
  }
  
  if(d.parent === tree.nil) {
    // removing root
    tree.root = x; // splice x into place of d
    // "what pointed to d now points to x"
  }
  else {
    // d.p !== tree.nil
    // not removing root
    if (d === d.parent.children[LEFT]) {
      // d is left child
      d.parent.children[LEFT] = x;  // splice x into place of d
    }
    else {
      // d !== d.p.children[LEFT]
      // d is right child
      d.parent.children[RIGHT] = x;
    }
    // "what pointed to d now points to x"
  }
  // "what pointed to d now points to x"

  if (d !== node) {
    // we removed `node`'s successor; not `node` itself;
    // copy from `d` to `node`
    node.value = d.value;
  }

  return d;
}

exports.remove = function(tree, value) {
  var d = searchNode(tree, value);
  if(d !== tree.nil) removeNode(tree, d);
}
