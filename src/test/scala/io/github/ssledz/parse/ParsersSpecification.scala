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

  property("succeed") = forAll { (a: String, b: Int) =>
    succeed(b).run(a) == Success(b, 0)
  }

  property("Left identity: return a >>= f ≡ f a") = forAll { (s: String, a: Int) =>
    def f(x: Int) = succeed(x + 1)

    succeed(a).flatMap(f).run(s) == f(a).run(s)
  }

  property("Right identity: m >>= return ≡ m") = forAll { s: String =>
    val m = string(s)
    m.flatMap(succeed).run(s) == m.run(s)
  }

  property("Associativity: (m >>= f) >>= g ≡ m >>= (\\x -> f x >>= g)") = forAll { (s: String, a: Int) =>
    def f(x: Int) = succeed(x + 1)

    def g(x: Int) = succeed(x * 3)

    val m = succeed(a)
    m.flatMap(f).flatMap(g).run(s) == m.flatMap(x => f(x).flatMap(g)).run(s)
  }

  property("product") = forAll { (a: String, b: String) =>
    product(a, b).run(a + b) == Success((a, b), a.length + b.length)
  }

  property("slice") = forAll { (a: String, b: String) =>
    slice(product(a, b)).run(a + b) == Success(a + b, a.length + b.length)
  }

  property("flatMap") = forAll { (a: String, b: String, c: String) =>
    (for {
      x <- string(a)
      y <- string(b)
      z <- string(c)
    } yield x + y + z).run(a + b + c) == Success(a + b + c, (a + b + c).length)
  }

  property("map2") = forAll { (a: Int, b: Int) =>
    val s = a.toString + b.toString
    a.toString.map2(b.toString())(_.toInt + _.toInt).run(s) == Success(a + b, s.length)
  }

  property("regex") = forAll { (a: Int, b: Int) =>
    val s = s"$a$b$a$b"
    val pattern = raw"($a$b)+".r
    pattern.run(s) == Success(s, s.length)
  }

  property("attempt") = forAll { s: String =>
    (attempt(fail("err")) | succeed(s)).run(s) == Success(s, 0)
  }

}
