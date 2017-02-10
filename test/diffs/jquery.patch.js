diff --git a/test/diffs/jquery.A.js b/test/diffs/jquery.B.js
index 0000000000000000000000000000000000000000..0000000000000000000000000000000000000000 100644
--- a/test/diffs/jquery.A.js
+++ b/test/diffs/jquery.B.js
@@ -3,7 +3,7 @@
   // We have to close these tags to support XHTML (#13200)
   var wrapMap = {
 
-    // Support: IE9
+    // Support: IE <=9 only
     option: [ 1, "<select multiple='multiple'>", "</select>" ],
 
     // XHTML parsers do not magically insert elements in the
@@ -17,7 +17,7 @@
     _default: [ 0, "", "" ]
   };
 
-  // Support: IE9
+  // Support: IE <=9 only
   wrapMap.optgroup = wrapMap.option;
 
   wrapMap.tbody = wrapMap.tfoot = wrapMap.colgroup = wrapMap.caption = wrapMap.thead;
