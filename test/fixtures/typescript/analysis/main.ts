// Use `tsc main.ts && node main.js` to test evaluation

import { baz as bar } from "./foo";
import { quz } from "./foo/b";

quz()
bar()
