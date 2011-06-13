
function Node(value, left, right) {
  this.value = value;
  this.left = left || null;   // all values < value
  this.right = right || null;   // all values < value
}


exports.arr = arr = function(node) {
  if(node === null) {
    return [];
  } else {
    return arr(node.left).concat([node.value]).concat(arr(node.right));
  }
}


exports.search = function search(node, value) {
  var found = false;

  while(node !== null && !found) {
    if(value === node.value) {
      found = true;
    }
    else {
      if(value < node.value) {
        node = node.left;
      }
      else {
        node = node.right;
      }
    }
  }

  return found;
}


exports.insert = function insert(node, value) {
  if(node === null) {
    return new Node(value, null, null);
  }
  else {
    var n = node;
    var found = false;

    while(n !== null && !found) {
      // n !== null && !found

      if(value === n.value) {
        // n !== null && !found && value === n.value
        found = true;
        // n !== null && found && value === n.value
      }
      else {
        // n !== null && !found && value !== n.value
        // traverse further down, or insert

        if(value < n.value) {
          // n !== null && !found && value < n.value
          if(n.left === null) {
            // n !== null && !found && value < n.value && n.left === null
            n.left = new Node(value, null, null);
            found = true;
          }
          else {
            // n !== null && !found && value < n.value && n.left !== null
            n = n.left;
          }
        }

        else {
          // n !== null && !found && value > n.value
          if(n.right === null) {
            n.right = new Node(value, null, null);
            found = true;
          }
          else {
            n = n.right;
          }
        }
      }
    }
    // n === null || found

    return node;
  }
}


function removeMax(node) {
  // node != null; node.right != null

  // Keep going right until n.right.right === null
  while(node.right.right !== null) {
    // node.right.right !== null
    node = node.right;
  }
  // node.right.right === null
  var o = node.right.value;
  node.right = node.right.left;
  return o;
}


function step(node, value) {
  if(value === node) return node;
  else if(value < node.value) return node.left;
  else return node.right;
}

function rmv(node, parent) {
  var dir = (parent.left === node ? "left" : "right");

  if(node.left === null && node.right === null) {
    parent[dir] = null;
  }
  else {
    if(node.left === null) {
      parent[dir] = node.right;
    }
    else {
      if(node.right === null) {
        parent[dir] = node.left;
      }
      else {
        // node.left !== null && node.right !== null
        if(node.left.right === null) {
          // node.left !== null && node.right !== null && node.left.right === null
          node.value = node.left.value;
          node.left = node.left.left;
        }
        else {
          // node.left !== null && node.right !== null && node.left.right !== null
          node.value = removeMax(node.left);
        }
      }
    }
  }
}

exports.remove = function remove(node, value) {
  var o;

  if(node === null) {
    o = null;
  }
  else {
    // Find node n that has value as child.
    if(value === node.value) {
      // value === node.value
      if(node.left == null && node.right == null) {
        return null;
      }
      else {
        // node.left !== null || node.right !== null
        if(node.left == null) {
          // node.right !== null
          return node.right;
        }
        else {
          // node.left !== null
          if(node.right == null) {
            // node.left !== null && node.right == null
            return node.left;
          }
          else {
            // node.left !== null && node.right !== null
            if(node.left.right === null) {
              // node.left !== null && node.right !== null && node.left.right === null
              node.value = node.left.value;
              node.left = node.left.left;
            }
            else {
              // node.left !== null && node.right !== null && node.left.right !== null
              node.value = removeMax(node.left);
            }
          }
        }
      }
    }
    else {
      // value !== node.value
      var n = node;
      var nxt = step(n, value);
      while(nxt !== null && nxt.value !== value) {
        n = nxt;
        nxt = step(nxt, value);
      }
      // nxt === null || nxt.value === value
      if(nxt !== null) {
        // nxt.value === value
        rmv(nxt, n);
      }
    }

    o = node;
  }

  return o;
}
