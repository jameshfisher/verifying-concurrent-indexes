module rb.insert.recursive;

import rb.node;
import rb.blacken;

Node* insert(Node* root, int value) {
  // Function precondition.
  /// <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>

  Node* i = insert_aux(root, value);
  // Specification for <code>insert_aux</code>.
  /// <e:or>
  ///   <e:pred name="RBT"><code>i</code>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union>, <e:var n="h"/></e:pred>
  ///   <e:pred name="RVT"><code>i</code>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union>, <e:var n="h"/></e:pred>
  /// </e:or>

  Node* o = blacken(i);
  // Specification for <code>blacken</code>.
  /// <e:exists>
  ///   <e:vars><e:var n="nh"/></e:vars>
  ///   <e:expr><e:and>
  ///     <e:pred name="BT"><code>o</code>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union>, <e:var n="nh"/></e:pred>
  ///     <e:in>
  ///       <e:var n="nh"/>
  ///       <e:set>
  ///         <e:var n="h"/>,
  ///         <e:plus><e:var n="h"/><e:const n="1"/></e:plus>
  ///       </e:set>
  ///     </e:in>
  ///   </e:and></e:expr>
  /// </e:exists>

  return o;
}