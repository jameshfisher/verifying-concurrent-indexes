
var LEFT = true;
var RIGHT = false;

function Node(value, left, right) {
  this.value = value;
  this.children = {
    true: left || null,  // all values < value
    false: right || null // all values < value
  };
}

exports.Tree = function Tree() {
  this.nil = new Node(0, null, null);
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


exports.search = function search(tree, value) {
  var node = tree.root;

  while(node !== tree.nil && node.value !== value) {
    node = node.children[value < node.value];
  }

  return node !== tree.nil;
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
      tree.root = new Node(value, tree.nil, tree.nil);
    }
    else {
      parent.children[value < parent.value] = new Node(value, tree.nil, tree.nil);
    }
  }
}

exports.insert2 = function insert2(tree, value) {
  if(tree.root === tree.nil) {
    tree.root = new Node(value, tree.nil, tree.nil);
  }
  else {
    var n = tree.root;
    var found = false;

    while(n !== tree.nil && !found) {
      if(value === n.value) {
        found = true;
      }
      else {

        var dir = value < n.value;

        if(n.children[dir] === tree.nil) {
          n.children[dir] = new Node(value, tree.nil, tree.nil);
          found = true;
        }
        else {
          n = n.children[dir];
        }
      }
    }
  }
}


function removeMax(tree, node) {
  // node != null; node.children[RIGHT] != null

  // Keep going right until n.children[RIGHT].children[RIGHT] === null
  while(node.children[RIGHT].children[RIGHT] !== tree.nil) {
    // node.children[RIGHT].children[RIGHT] !== null
    node = node.children[RIGHT];
  }
  // node.children[RIGHT].children[RIGHT] === null
  var o = node.children[RIGHT].value;
  node.children[RIGHT] = node.children[RIGHT].children[LEFT];
  return o;
}


function step(node, value) {
  if(value === node) return node;
  else return node.children[value < node.value];
}

function rmv(tree, node, parent) {
  var dir = (parent.children[LEFT] === node ? LEFT : RIGHT);

  if(node.children[LEFT] === tree.nil && node.children[RIGHT] === tree.nil) {
    parent.children[dir] = tree.nil;
  }
  else {
    if(node.children[LEFT] === tree.nil) {
      parent.children[dir] = node.children[RIGHT];
    }
    else {
      if(node.children[RIGHT] === tree.nil) {
        parent.children[dir] = node.children[LEFT];
      }
      else {
        if(node.children[LEFT].children[RIGHT] === tree.nil) {
          node.value = node.children[LEFT].value;
          node.children[LEFT] = node.children[LEFT].children[LEFT];
        }
        else {
          node.value = removeMax(tree, node.children[LEFT]);
        }
      }
    }
  }
}

exports.remove = function remove(tree, value) {
  var node = tree.root;

  if(node !== tree.nil) {
    if(value === node.value) {
      if(node.children[LEFT] === tree.nil && node.children[RIGHT] === tree.nil) {
        tree.root = tree.nil;
      }
      else {
        if(node.children[LEFT] === tree.nil) {
          tree.root = node.children[RIGHT];
        }
        else {
          if(node.children[RIGHT] === tree.nil) {
            tree.root = node.children[LEFT];
          }
          else {
            if(node.children[LEFT].children[RIGHT] === tree.nil) {
              node.value = node.children[LEFT].value;
              node.children[LEFT] = node.children[LEFT].children[LEFT];
            }
            else {
              node.value = removeMax(tree, node.children[LEFT]);
            }
          }
        }
      }
    }
    else {
      var n = node;
      var nxt = step(n, value);
      while(nxt !== tree.nil && nxt.value !== value) {
        n = nxt;
        nxt = step(nxt, value);
      }
      if(nxt !== tree.nil) {
        rmv(tree, nxt, n);
      }
    }
  }
}
