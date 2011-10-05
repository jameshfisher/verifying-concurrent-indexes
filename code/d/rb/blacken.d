module rb.blacken;

import rb.node;

Node* blacken(Node* root) {
  // Function precondition.
  /// <e:or>
  ///   <e:pred name="RBT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>
  ///   <e:pred name="RVT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>
  /// </e:or>

  if (root == null) {
    /// <e:pred name="ET"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>

    // Weakening
    /// <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>

    // Member of set containing itself
    /// <e:and>
    ///   <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>
    ///   <e:in>
    ///     <e:var n="h"/>
    ///     <e:set>
    ///       <e:var n="h"/>,
    ///       <e:plus><e:var n="h"/><e:const n="1"/></e:plus>
    ///     </e:set>
    ///   </e:in>
    /// </e:and>

    // <m:existsIntro/> on <e:var n="h"/> as <e:var n="nh"/>.
    /// <e:exists>
    ///   <e:vars><e:var n="nh"/></e:vars>
    ///   <e:expr><e:and>
    ///     <e:pred name="BT"><code>o</code>, <e:st n="S"/>, <e:var n="nh"/></e:pred>
    ///     <e:in>
    ///       <e:var n="nh"/>
    ///       <e:set>
    ///         <e:var n="h"/>,
    ///         <e:plus><e:var n="h"/><e:const n="1"/></e:plus>
    ///       </e:set>
    ///     </e:in>
    ///   </e:and></e:expr>
    /// </e:exists>
  }
  else {
    /// <e:or>
    ///   <e:or>
    ///     <e:pred name="RT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>
    ///     <e:pred name="NBT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>
    ///   </e:or>
    ///   <e:pred name="RVT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>
    /// </e:or>

    if (root.black) {
      /// <e:pred name="NBT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>

      // Weakening
      /// <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>

      // Member of set containing itself
      /// <e:and>
      ///   <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>
      ///   <e:in>
      ///     <e:var n="h"/>
      ///     <e:set>
      ///       <e:var n="h"/>,
      ///       <e:plus><e:var n="h"/><e:const n="1"/></e:plus>
      ///     </e:set>
      ///   </e:in>
      /// </e:and>

      // <m:existsIntro/> on <e:var n="h"/> as <e:var n="nh"/>.
      /// <e:exists>
      ///   <e:vars><e:var n="nh"/></e:vars>
      ///   <e:expr><e:and>
      ///     <e:pred name="BT"><code>o</code>, <e:st n="S"/>, <e:var n="nh"/></e:pred>
      ///     <e:in>
      ///       <e:var n="nh"/>
      ///       <e:set>
      ///         <e:var n="h"/>,
      ///         <e:plus><e:var n="h"/><e:const n="1"/></e:plus>
      ///       </e:set>
      ///     </e:in>
      ///   </e:and></e:expr>
      /// </e:exists>
    }
    else {
      /// <e:or>
      ///   <e:pred name="RT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>
      ///   <e:pred name="RVT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>
      /// </e:or>

      root.black = true;
      /// <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:plus><e:var n="h"/><e:const n="1"/></e:plus></e:pred>

      // Member of set containing itself
      /// <e:and>
      ///   <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>
      ///   <e:in>
      ///     <e:var n="h"/>
      ///     <e:set>
      ///       <e:var n="h"/>,
      ///       <e:plus><e:var n="h"/><e:const n="1"/></e:plus>
      ///     </e:set>
      ///   </e:in>
      /// </e:and>

      // <m:existsIntro/> on <e:var n="h"/> as <e:var n="nh"/>.
      /// <e:exists>
      ///   <e:vars><e:var n="nh"/></e:vars>
      ///   <e:expr><e:and>
      ///     <e:pred name="BT"><code>o</code>, <e:st n="S"/>, <e:var n="nh"/></e:pred>
      ///     <e:in>
      ///       <e:var n="nh"/>
      ///       <e:set>
      ///         <e:var n="h"/>,
      ///         <e:plus><e:var n="h"/><e:const n="1"/></e:plus>
      ///       </e:set>
      ///     </e:in>
      ///   </e:and></e:expr>
      /// </e:exists>
    }
    // If-rule.
    /// <e:exists>
    ///   <e:vars><e:var n="nh"/></e:vars>
    ///   <e:expr><e:and>
    ///     <e:pred name="BT"><code>o</code>, <e:st n="S"/>, <e:var n="nh"/></e:pred>
    ///     <e:in>
    ///       <e:var n="nh"/>
    ///       <e:set>
    ///         <e:var n="h"/>,
    ///         <e:plus><e:var n="h"/><e:const n="1"/></e:plus>
    ///       </e:set>
    ///     </e:in>
    ///   </e:and></e:expr>
    /// </e:exists>
  }

  // If-rule. Function postcondition.
  /// <e:exists>
  ///   <e:vars><e:var n="nh"/></e:vars>
  ///   <e:expr><e:and>
  ///     <e:pred name="BT"><code>o</code>, <e:st n="S"/>, <e:var n="nh"/></e:pred>
  ///     <e:in>
  ///       <e:var n="nh"/>
  ///       <e:set>
  ///         <e:var n="h"/>,
  ///         <e:plus><e:var n="h"/><e:const n="1"/></e:plus>
  ///       </e:set>
  ///     </e:in>
  ///   </e:and></e:expr>
  /// </e:exists>

  return root;
}