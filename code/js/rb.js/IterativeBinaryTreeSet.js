
var LEFT = true;
var RIGHT = false;

function Node(value, left, right) {
  this.value = value;
  this.children = {
    true: left || null,  // all values < value
    false: right || null // all values < value
  };
}


exports.arr = arr = function(node) {
  if(node === null) {
    return [];
  } else {
    return arr(node.children[LEFT]).concat([node.value]).concat(arr(node.children[RIGHT]));
  }
}


exports.search = function search(node, value) {
  var found = false;

  while(node !== null && !found) {
    if(value === node.value) {
      found = true;
    }
    else {
      node = node.children[value < node.value];
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

        var dir = value < n.value;

        if(n.children[dir] === null) {
          n.children[dir] = new Node(value, null, null);
          found = true;
        }
        else {
          n = n.children[dir];
        }
      }
    }
    // n === null || found

    return node;
  }
}


function removeMax(node) {
  // node != null; node.children[RIGHT] != null

  // Keep going right until n.children[RIGHT].children[RIGHT] === null
  while(node.children[RIGHT].children[RIGHT] !== null) {
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

function rmv(node, parent) {
  var dir = (parent.children[LEFT] === node ? LEFT : RIGHT);

  if(node.children[LEFT] === null && node.children[RIGHT] === null) {
    parent.children[dir] = null;
  }
  else {
    if(node.children[LEFT] === null) {
      parent.children[dir] = node.children[RIGHT];
    }
    else {
      if(node.children[RIGHT] === null) {
        parent.children[dir] = node.children[LEFT];
      }
      else {
        if(node.children[LEFT].children[RIGHT] === null) {
          node.value = node.children[LEFT].value;
          node.children[LEFT] = node.children[LEFT].children[LEFT];
        }
        else {
          node.value = removeMax(node.children[LEFT]);
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
    if(value === node.value) {
      if(node.children[LEFT] == null && node.children[RIGHT] == null) {
        return null;
      }
      else {
        if(node.children[LEFT] == null) {
          return node.children[RIGHT];
        }
        else {
          if(node.children[RIGHT] == null) {
            return node.children[LEFT];
          }
          else {
            if(node.children[LEFT].children[RIGHT] === null) {
              node.value = node.children[LEFT].value;
              node.children[LEFT] = node.children[LEFT].children[LEFT];
            }
            else {
              node.value = removeMax(node.children[LEFT]);
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
