import defaultMember from "foo";
import * as name from "aardvark";
import { member } from "ant";
import { member1 , member2 } from "antelope";
import { member1 , member2 as alias2 } from "ant-eater";
import defaultMember, { member1, member2 as alias2 } from "anaconda";
import defaultMember, * as name from "alligator";
import "arctic-tern";
import zip = require("../zip");

function someFunction(arg1, arg2): string { arg2; };

someFunction(arg1, "arg2");


defaultMember()
name.foo()
