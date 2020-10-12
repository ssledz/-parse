package io.github.ssledz.parse

import io.github.ssledz.parse.Parsers._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object ParsersSpecification extends Properties("Parsers") {

  property("char") = forAll { c: Char =>
    char(c).run(c.toString) == Success(c, 1)
  }

  property("string") = forAll { s: String =>
    string(s).run(s) == Success(s, s.length)
  }

  property("or") = forAll { (a: String, b: String) =>
    (a | b).run(a) == Success(a, a.length)
  }

  property("or2") = forAll { (a: String, b: String) =>
    !a.isEmpty ==> ((attempt(a) | b).run(b) == Success(b, b.length))
  }
}
