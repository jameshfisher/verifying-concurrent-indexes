module rb.blacken;

import rb.node;

Node* blacken(Node* root) {
  // Function precondition.
  /// <e:or>
  ///   <e:fst><e:pred name="RBT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred></e:fst>
  ///   <e:snd><e:pred name="RVT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred></e:snd>
  /// </e:or>

  if (root == null) {
    /// <e:pred name="ET"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>

    // Weakening
    /// <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>

    // Member of set containing itself
    /// <e:and>
    ///   <e:fst><e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred></e:fst>
    ///   <e:snd><e:in>
    ///     <e:fst><e:var n="h"/></e:fst>
    ///     <e:snd><e:set>
    ///       <e:var n="h"/>,
    ///       <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus>
    ///     </e:set></e:snd>
    ///   </e:in></e:snd>
    /// </e:and>

    // <m:existsIntro/> on <e:var n="h"/> as <e:var n="nh"/>.
    /// <e:exists>
    ///   <e:fst><e:var n="nh"/></e:fst>
    ///   <e:snd><e:and>
    ///     <e:fst><e:pred name="BT"><code>o</code>, <e:st n="S"/>, <e:var n="nh"/></e:pred></e:fst>
    ///     <e:snd><e:in>
    ///       <e:fst><e:var n="nh"/></e:fst>
    ///       <e:snd><e:set>
    ///         <e:var n="h"/>,
    ///         <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus>
    ///       </e:set></e:snd>
    ///     </e:in></e:snd>
    ///   </e:and></e:snd>
    /// </e:exists>
  }
  else {
    /// <e:or>
    ///   <e:fst><e:or>
    ///     <e:fst><e:pred name="RT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred></e:fst>
    ///     <e:snd><e:pred name="NBT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred></e:snd>
    ///   </e:or></e:fst>
    ///   <e:snd><e:pred name="RVT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred></e:snd>
    /// </e:or>

    if (root.black) {
      /// <e:pred name="NBT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>

      // Weakening
      /// <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>

      // Member of set containing itself
      /// <e:and>
      ///   <e:fst><e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred></e:fst>
      ///   <e:snd><e:in>
      ///     <e:fst><e:var n="h"/></e:fst>
      ///     <e:snd><e:set>
      ///       <e:var n="h"/>,
      ///       <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus>
      ///     </e:set></e:snd>
      ///   </e:in></e:snd>
      /// </e:and>

      // <m:existsIntro/> on <e:var n="h"/> as <e:var n="nh"/>.
      /// <e:exists>
      ///   <e:fst><e:var n="nh"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="BT"><code>o</code>, <e:st n="S"/>, <e:var n="nh"/></e:pred></e:fst>
      ///     <e:snd><e:in>
      ///       <e:fst><e:var n="nh"/></e:fst>
      ///       <e:snd><e:set>
      ///         <e:var n="h"/>,
      ///         <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus>
      ///       </e:set></e:snd>
      ///     </e:in></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists>
    }
    else {
      /// <e:or>
      ///   <e:fst><e:pred name="RT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred></e:fst>
      ///   <e:snd><e:pred name="RVT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred></e:snd>
      /// </e:or>

      root.black = true;
      /// <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus></e:pred>

      // Member of set containing itself
      /// <e:and>
      ///   <e:fst><e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred></e:fst>
      ///   <e:snd><e:in>
      ///     <e:fst><e:var n="h"/></e:fst>
      ///     <e:snd><e:set>
      ///       <e:var n="h"/>,
      ///       <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus>
      ///     </e:set></e:snd>
      ///   </e:in></e:snd>
      /// </e:and>

      // <m:existsIntro/> on <e:var n="h"/> as <e:var n="nh"/>.
      /// <e:exists>
      ///   <e:fst><e:var n="nh"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="BT"><code>o</code>, <e:st n="S"/>, <e:var n="nh"/></e:pred></e:fst>
      ///     <e:snd><e:in>
      ///       <e:fst><e:var n="nh"/></e:fst>
      ///       <e:snd><e:set>
      ///         <e:var n="h"/>,
      ///         <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus>
      ///       </e:set></e:snd>
      ///     </e:in></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists>
    }
    // If-rule.
    /// <e:exists>
    ///   <e:fst><e:var n="nh"/></e:fst>
    ///   <e:snd><e:and>
    ///     <e:fst><e:pred name="BT"><code>o</code>, <e:st n="S"/>, <e:var n="nh"/></e:pred></e:fst>
    ///     <e:snd><e:in>
    ///       <e:fst><e:var n="nh"/></e:fst>
    ///       <e:snd><e:set>
    ///         <e:var n="h"/>,
    ///         <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus>
    ///       </e:set></e:snd>
    ///     </e:in></e:snd>
    ///   </e:and></e:snd>
    /// </e:exists>
  }

  // If-rule. Function postcondition.
  /// <e:exists>
  ///   <e:fst><e:var n="nh"/></e:fst>
  ///   <e:snd><e:and>
  ///     <e:fst><e:pred name="BT"><code>o</code>, <e:st n="S"/>, <e:var n="nh"/></e:pred></e:fst>
  ///     <e:snd><e:in>
  ///       <e:fst><e:var n="nh"/></e:fst>
  ///       <e:snd><e:set>
  ///         <e:var n="h"/>,
  ///         <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus>
  ///       </e:set></e:snd>
  ///     </e:in></e:snd>
  ///   </e:and></e:snd>
  /// </e:exists>

  return root;
}