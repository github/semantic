diff --git a/test/diffs/jquery.A.js b/test/diffs/jquery.B.js
index .. 100644
--- a/test/diffs/jquery.A.js
+++ b/test/diffs/jquery.B.js
@@ -3,7 +3,4 @@ define( function() {
// We have to close these tags to support XHTML (#13200)
var wrapMap = {

-  // Support: IE9
+  // Support: IE <=9 only
   option: [ 1, "<select multiple='multiple'>", "</select>" ],
 
   // XHTML parsers do not magically insert elements in the
@@ -17,7 +17,7 @@ var wrapMap = {
     _default: [ 0, "", "" ]
   };
 
-  // Support: IE9
+  // Support: IE <=9 only
   wrapMap.optgroup = wrapMap.option;
 
   wrapMap.tbody = wrapMap.tfoot = wrapMap.colgroup = wrapMap.caption = wrapMap.thead;
