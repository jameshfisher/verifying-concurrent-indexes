(function() {

var debug = true;

var assert = require("assert");
var util = require("util");
var Set = require("./RBT");

var tree = new Set.Tree();

var reference = {};

for(var i = 0; i < 10; i++) {

  var val = Math.floor(Math.random()*10);

  if(Math.floor(Math.random()*2)) {
    if(debug) console.log("insert " + val);
    Set.insert(tree, val);
    reference[val] = true;
  }
  else {
    if(debug) console.log("remove " + val);
    Set.remove(tree, val);
    delete reference[val];
  }

  if(debug) {
    console.log(util.inspect(tree, false, null));
    console.log(Set.str(tree));
    console.log(reference);
  }

  val = Math.floor(Math.random()*10);
  if(debug) console.log("search " + val);

  assert.equal(
    Set.search(tree, val),
    reference[val] === true
    );
  if(debug) console.log("----");
}

}());
