<?php

namespace NS1\Sub1\Sub2
{
  function f() {
    return 1;
  }
}

namespace NS1
{
  function b() {
    return "hi";
  }
}
namespace NS1
{
  function c() {
    return "x";
  }
}

namespace Foo
{
  \NS1\Sub1\Sub2\f();
  \NS1\b();
  \NS1\c();
}
