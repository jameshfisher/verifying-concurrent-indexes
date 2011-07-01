function Node(val, color, parent, paper, dummy) {
  this.val = val;
  this.left = this.right = null;

  this.parent = parent;

  this.color = color;

  this.line = paper.path(parent.line.attr("path")).attr("stroke", "5px black").insertBefore(dummy);
  this.circle = paper.circle(parent.circle.attr("cx"), parent.circle.attr("cy"), 10).attr("fill", "d00").attr("stroke", "none");
  this.text = paper.text(parent.text.attr("x"), parent.text.attr("y"), 20).attr("fill", "white").attr("font", "10px Arial");

}

function insert(node, value, parent, paper, dummy) {
  if (node === null) {
    var o = new Node(value, (Math.random() < 0.5 ? "red" : "black"), parent, paper, dummy);
    return o;
  } else {
    if(value === node.val) {
    }
    else if(value < node.val) {
      node.left = insert(node.left, value, node, paper, dummy);
    } else {
      node.right = insert(node.right, value, node, paper, dummy);
    }
    return node;
  }
}

drawNode = function(node) {
  node.circle.attr("fill", (node.color === "black" ? "000" : "d00"));
  node.text.attr("text", node.val.toString());

  var speed = 500;

  node.circle.animate({cx: node.x, cy: node.y}, speed);
  node.text.animate({x: node.x, y: node.y}, speed);
  //console.log("M "+node.parentX()+" "+node.parentY()+" L " + node.x + " " + node.y);
  node.line.animate({path: "M "+node.parentX()+" "+node.parentY()+" L " + node.x + " " + node.y}, speed);
}


Node.prototype.parentX = function() {
  return (this.parent === null) ? this.x : this.parent.x;
}

Node.prototype.parentY = function() {
  return (this.parent === null) ? this.y-30 : this.parent.y;
}


function depth(node) {
  return (node === null) ? 0 : 1 + Math.max(depth(node.left), depth(node.right));
}


function walk(node, f) {
  if(node !== null) {
    walk(node.left, f);
    f(node);
    walk(node.right, f);
  }
}


function updateX_equidistant(tree, start) {
  var i = 0;
  walk(tree, function(n) { n.x = i; i+=30; });
  var adj = start - tree.x;
  walk(tree, function(n) { n.x += adj; });
}


function updateX_classical(tree, start, shift) {
  if(tree !== null) {
    tree.x = start;
    var nextShift = shift/2;
    updateX_classical(tree.left, start-shift, nextShift);
    updateX_classical(tree.right, start+shift, nextShift);
  }
}


function updateY_depth(tree, start) {
  if(tree !== null) {
    tree.y = start;
    updateY_depth(tree.left, start+30);
    updateY_depth(tree.right, start+30);
  }
}


function updateY_black_height(tree, start) {
  if(tree !== null) {
    var height = (tree.color === "black" ? start+30 : start);
    tree.y = height;
    updateY_black_height(tree.left, height);
    updateY_black_height(tree.right, height);
  }
}


function redraw(tree) {
  walk(tree, function(n) { drawNode(n); });
}


function update(tree) {
  var shift = Math.pow(2, depth(tree)) * 6;

  updateX_equidistant(tree, 800, shift);
  updateY_black_height(tree, 30);

  redraw(tree);
}




window.onload = function() {
  var paper = Raphael("tree", 1600, 1600);

  var dummy = paper.circle(0,0,0);

  var tree = null;

  var sentinel = {
    x:      800,
    y:      30,
    line:   paper.path("M 800 0 L 800 30").hide(),
    circle: paper.circle(800, 0, 10).hide(),
    text:   paper.text(0, 0, 20).hide()
  };

  function populate(tree) {
    window.setTimeout(function() {
      tree = insert(tree, Math.floor(Math.random()*20), sentinel, paper, dummy);
      update(tree);
      populate(tree);
    }, 1000);
  }

  populate(tree);

}
