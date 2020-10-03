package io.github.ssledz.parse

import scala.util.matching.Regex

object Parsers {

  type Parser[+A] = Location => Result[A]

  def run[A](p: Parser[A])(input: String): Result[A] = p(Location(input, 0))

  implicit def char(c: Char): Parser[Char] = map(string(c.toString))(_.charAt(0))

  def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(f andThen succeed)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = map(product(p, p2))(f.tupled)

  def many[A](p: Parser[A]): Parser[List[A]] = or(attempt(map2(p, many(p))(_ :: _)), succeed(List.empty))

  def timesN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) succeed(List.empty[A]) else map2(p, timesN(n - 1, p))(_ :: _)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)

  implicit def string(s: String): Parser[String] =
    scope(s"Expected: '$s'") { loc =>
      if (loc.in.startsWith(s)) Success(s, s.length) else Failure.Empty
    }

  implicit def regex(r: Regex): Parser[String] = scope(s"Expected: '$r'") { loc =>
    r.findPrefixOf(loc.in) match {
      case Some(value) => Success(value, value.length)
      case None => Failure.Empty
    }
  }


  def scope[A](msg: String)(p: Parser[A]): Parser[A] = loc => p(loc).mapError(_.push(loc, msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] = loc => p(loc).mapError(_.label(msg))

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = loc =>
    p1(loc) match {
      case Failure(_, false) => p2(loc)
      case other => other
    }

  def many1[A](p: Parser[A]): Parser[List[A]] =
    for {
      a <- p
      as <- many[A](p)
    } yield a :: as

  def slice[A](p: Parser[A]): Parser[String] = loc =>
    p(loc) match {
      case Success(_, charsConsumed) => Success(loc.in.take(charsConsumed), charsConsumed)
      case err@Failure(_, _) => err
    }

  def attempt[A](p: Parser[A]): Parser[A] = loc =>
    p(loc) match {
      case Failure(e, true) => Failure(e, false)
      case other => other
    }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = loc =>
    p(loc) match {
      case Success(a, charsConsumed) =>
        f(a)(loc.advanceBy(charsConsumed))
          .addCommit(charsConsumed != 0)
          .advanceSuccess(charsConsumed)
      case err@Failure(_, _) => err
    }

  implicit def ops[A](p: Parser[A]): ParserOps[A] = new ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = new ParserOps(f(a))

  implicit def asCharParser[A](a: A)(implicit f: A => Parser[Char]): ParserOps[Char] = new ParserOps(f(a))

  class ParserOps[A](val p: Parser[A]) extends AnyVal {

    def flatMap[B](f: A => Parser[B]): Parser[B] = Parsers.flatMap(p)(f)

    def map[B](f: A => B): Parser[B] = Parsers.map(p)(f)

    def **[B](p2: Parser[B]): Parser[(A, B)] = Parsers.product(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = Parsers.or(p, p2)

    def |[B >: A](p2: => Parser[B]): Parser[B] = Parsers.or(p, p2)

    def times(n: Int): Parser[List[A]] = Parsers.timesN(n, p)

    def many: Parser[List[A]] = Parsers.many(p)

    def slice: Parser[String] = Parsers.slice(p)

    def attempt: Parser[A] = Parsers.attempt(p)

    def many1: Parser[List[A]] = Parsers.many1(p)

    def run(input: String): Result[A] = Parsers.run(p)(input)

  }

  case class ParseError(stack: List[(Location, String)]) {
    def push(loc: Location, msg: String): ParseError = copy(stack = (loc, msg) :: stack)

    def latest: Option[(Location, String)] = stack.lastOption

    def latestLoc: Option[Location] = latest map (_._1)

    def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)
  }

  case class Location(private val input: String, offset: Int) {
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def in: String = input.substring(offset)

    def advanceBy(n: Int): Location = copy(offset = offset + n)

    def toError(msg: String): ParseError = ParseError(List(this -> msg))
  }

  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, isCommitted) => Failure(f(e), isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, m + n)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError, isCommitted: Boolean = true) extends Result[Nothing]

  object Failure {
    val Empty: Failure = Failure(ParseError(List.empty))
  }

}