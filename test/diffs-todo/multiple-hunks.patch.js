diff --git a/test/diffs/multiple-hunks.A.js b/test/diffs/multiple-hunks.B.js
index 0000000000000000000000000000000000000000..0000000000000000000000000000000000000000 100644
--- a/test/diffs/multiple-hunks.A.js
+++ b/test/diffs/multiple-hunks.B.js
@@ -1,5 +1,5 @@
 define( function() {
-  console.log("foo");
+  console.log("bar");
   // A comment
   // Another comment
   // Another comment
@@ -9,6 +9,6 @@
   // A comment
   // Another comment
   // Another comment
-  console.log("bar");
+  console.log("foo");
 });
