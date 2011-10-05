module rb.insert.insert_aux;

import rb.node;
import rb.insert.balance;

Node* insert_aux(Node* root, int value) {
  Node* o;
  /// <e:or>
  ///   (<e:and>
  ///     <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>
  ///     black
  ///   </e:and>)
  ///   (<e:and>
  ///     <e:pred name="RT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>
  ///     ¬black
  ///   </e:and>)
  /// </e:or>

  if (root == null) {
    /// <e:and>
    ///   <e:pred name="EBT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>
    ///   black
    /// </e:and>
    o = new Node(value);
    /// <e:and>
    ///   <e:pred name="RT"><code>o</code>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union>, <e:var n="h"/></e:pred>
    ///   black
    /// </e:and>

    // <m:orIntro/>.
    /// <e:or>
    ///   (<e:and>
    ///     (<e:or>
    ///       <e:pred name="BT"><code>o</code>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union>, <e:var n="h"/></e:pred>
    ///       <e:pred name="RT"><code>o</code>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union>, <e:var n="h"/></e:pred>
    ///     </e:or>)
    ///     black
    ///   </e:and>)
    ///   <br/>(<e:and>
    ///     (<e:or>
    ///       <e:pred name="RT"><code>o</code>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union>, <e:var n="h"/></e:pred>
    ///       <e:pred name="RVT"><code>o</code>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union>, <e:var n="h"/></e:pred>
    ///     </e:or>)
    ///     ¬black
    ///   </e:and>)
    /// </e:or>
  }
  else {
    if (root.value == value) {
      o = root;
    }
    else {
      int dir = root.value < value;
      root.c[dir] = insert_aux(root.c[dir], value);
      o =  balance(root, dir);
    }
  }
  /// <e:or>
  ///   (<e:and>
  ///     (<e:or>
  ///       <e:pred name="BT"><code>o</code>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union>, <e:var n="h"/></e:pred>
  ///       <e:pred name="RT"><code>o</code>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union>, <e:var n="h"/></e:pred>
  ///     </e:or>)
  ///     black
  ///   </e:and>)
  ///   <br/>(<e:and>
  ///     (<e:or>
  ///       <e:pred name="RT"><code>o</code>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union>, <e:var n="h"/></e:pred>
  ///       <e:pred name="RVT"><code>o</code>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union>, <e:var n="h"/></e:pred>
  ///     </e:or>)
  ///     ¬black
  ///   </e:and>)
  /// </e:or>

  return o;
}
