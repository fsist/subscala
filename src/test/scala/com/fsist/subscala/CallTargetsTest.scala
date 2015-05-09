package com.fsist.subscala

import shapeless.test.illTyped

object CallTargetsTest {
  // This file doesn't contain any runtime tests; the tests are done at compile time.

  def testAllMethodsOf = {
    Restrict[String, Syntax.All, CallTargets.AllMethodsOf[String]] {
      "foo".contains("f")
      "bar".indexOf("b")
      ""
    }

    illTyped(
      """
        Restrict[String, Syntax.All, CallTargets.AllMethodsOf[String]] {
          "foo".foreach(println) // foreach is a not method of String
          ""
        }
      """, "Calling method foreach of IndexedSeqOptimized is disallowed")
  }
}
