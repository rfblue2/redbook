package com.redbook

import com.redbook.Chapter9.MyParsers
import com.redbook.Chapter9.Parsers.ParsersImpl

import scala.util.matching.Regex

object Chapter9 {

  case class ParseError(stack: List[(Location, String)]) {
    def push(loc: Location, msg: String): ParseError =
      copy(stack = (loc, msg) :: stack)
    def label[A](s: String): ParseError =
      ParseError(latestLoc.map((_, s)).toList)
    def latestLoc: Option[Location] = latest.map(_._1)
    def latest: Option[(Location, String)] = stack.lastOption

    // Ex16
    override def toString: String =
      stack
        .groupBy { case (loc, _) => loc }
        .map { case (loc, msgs) =>
          s"For ${loc.input} at ${loc.offset}: ${msgs.map(_._2).mkString(", ")}"
        }
        .mkString("\n")
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1        => offset + 1
      case lineStart => offset - lineStart
    }

    def toError(msg: String): ParseError = ParseError(List((this, msg)))
    def advanceBy(n: Int): Location = copy(offset = offset + n)
  }

  trait Parsers[Parser[+_]] { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def label[A](msg: String)(p: Parser[A]): Parser[A]
    def scope[A](msg: String)(p: Parser[A]): Parser[A]
    def attempt[A](p: Parser[A]): Parser[A]

    def errorLocation(e: ParseError): Location = e.stack.head._1

    def errorMessage(e: ParseError): String = e.stack.head._2

    implicit def string(s: String): Parser[String]
    implicit def regex(r: Regex): Parser[String]

    implicit def asStringParser[A](a: A)(implicit
        f: A => Parser[String]
    ): ParserOps[String] =
      ParserOps(f(a))

    implicit def asParser[A](a: Parser[A]): ParserOps[A] = ParserOps(a)

    implicit def char(c: Char): Parser[Char] =
      string(c.toString).map(_.charAt(0))

    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    // Ex8
    def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
      flatMap(a)(f andThen succeed)

    // Ex7
    def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
      for (a <- p; b <- p2) yield (a, b)

    def map2WithFlatMap[A, B, C](p: Parser[A], p2: => Parser[B])(
        f: (A, B) => C
    ): Parser[C] =
      for (a <- p; b <- p2) yield f(a, b)

    def slice[A](p: Parser[A]): Parser[String]

    def succeed[A](a: A): Parser[A]
//      string("").map(_ => a) // this causes circular impl between map && succeed

    def fail[A]: Parser[A]

    // Ex1
    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(
        f: (A, B) => C
    ): Parser[C] =
      (p ** p2) map f.tupled

    def many1[A](p: Parser[A]): Parser[List[A]] =
      map2(p, p.many)(_ :: _)

    // Ex3
    def many[A](p: Parser[A]): Parser[List[A]] =
      map2(p, p.many)(_ :: _) or succeed(List())

    // Ex4
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n <= 0) succeed(List())
      else map2(p, listOfN(n - 1, p))(_ :: _)

    // Ex5
    def nonStrict[A](p: => Parser[A]): Parser[A] = p
    // add entry to parser ops, then:
    // usage: map2(p, p.many.nonStrict)(_ :: _)

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def many: Parser[List[A]] = self.many(p)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
      def slice: Parser[String] = self.slice(p)
      def product[B >: A](p2: => Parser[B]): Parser[(A, B)] =
        self.product(p, p2)
      def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    }

    // .slice returns string which is constant time to eval .size
    val numA: Parser[Int] = char('a').many.slice.map(_.size)

    // Ex6
    val contextSensitiveCount: Parser[String] =
      "[0-9]".r
        .map(_.toInt)
        .flatMap(listOfN(_, 'a'))
        .map(_.mkString)

    object Laws {
      import Chapter8._
      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
        Gen.forAll(in)(s => run(p1)(s) == run(p2)(s))

      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
        equal(p, p.map(a => a))(in)

      def charLaw(in: Gen[Char]): Prop =
        Gen.forAll(in)(c => run(c)(c.toString) == Right(c))

      def stringLaw(in: Gen[String]): Prop =
        Gen.forAll(in)(s => run(s)(s) == Right(s))

      def succeedLaw[A](p: Parser[A])(in: Gen[String]): Prop =
        Gen.forAll(in)(s => run(succeed(p))(s) == Right(p))

      // Ex2
      def productLaw[A, B](p1: Parser[A], p2: Parser[B])(
          in: Gen[String]
      ): Prop =
        equal(product(p1, p2), product(p2, p1))(in)

      def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
        SGen.forAll(inputs ** Gen.string) { case (input, msg) =>
          run(label(msg)(p))(input) match {
            case Left(e) => errorMessage(e) == msg
            case _       => true
          }
        }

      def attemptLaw[A](p: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
        equal(attempt(p flatMap (_ => fail)) or p2, p2)(in)
    }
  }

  // Ex9
  trait JSON
  object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON

    def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
      import P._
      val jpWhitespace = regex("[ \\t\\n]*".r)
      val jpNumber: Parser[JSON] =
        regex("[0-9]+".r).map(n => JNumber(n.toDouble))
      val validString = regex("\"[a-zA-Z0-9]*\"".r)
      val jpString: Parser[JString] =
        validString.map(s => JString(s.substring(1, s.length - 1)))
      val jpBool: Parser[JBool] =
        ("true" | "false").map(b => JBool(b.toBoolean))
      val jpNull: Parser[JSON] = string("null").map(_ => JNull)

      def jpValue: Parser[JSON] =
        attempt(jpBool) | attempt(jpNull) | attempt(jpNumber) | attempt(
          jpString
        ) | attempt(jpArray) | jpObject

      def withWhitespace(p: Parser[String]): Parser[String] =
        for {
          _ <- jpWhitespace
          s <- p
          _ <- jpWhitespace
        } yield s

      def separatedList[A](
          sep: Parser[String],
          open: Parser[String],
          close: Parser[String],
          entry: => Parser[A]
      ): Parser[List[A]] = {
        val separated: Parser[A] =
          for (_ <- sep; e <- entry) yield e

        for {
          _ <- open
          es <- attempt(map2(entry, separated.many)(_ :: _)) | succeed(List())
          _ <- close
        } yield es
      }

      def jpArray: Parser[JArray] = {
        separatedList(
          sep = withWhitespace(","),
          open = string("["),
          close = withWhitespace("]"),
          jpValue
        ).map(js => JArray(js.toIndexedSeq))
      }

      def jpObject: Parser[JSON] = {
        separatedList(
          sep = withWhitespace(","),
          open = string("{"),
          close = withWhitespace("}"),
          for {
            k <- validString
            _ <- withWhitespace(":")
            v <- jpValue
          } yield (k, v)
        ).map(js => JObject(js.toMap))
      }

      jpValue
    }
  }

  // Ex12
  case class MyParser[+A](
      name: String,
      matches: (String, String) => Either[ParseError, (String, A)],
      attempt: Boolean = false,
      labels: List[String] = List()
  )
  object MyParsers extends Parsers[MyParser] {
    override def run[A](p: MyParser[A])(
        input: String
    ): Either[ParseError, A] =
      p.matches(input, "").map(_._2)

    override def label[A](msg: String)(p: MyParser[A]): MyParser[A] =
      p.copy(labels = List(msg))

    override def scope[A](msg: String)(p: MyParser[A]): MyParser[A] =
      p.copy(labels = msg +: p.labels)

    override def attempt[A](p: MyParser[A]): MyParser[A] =
      p.copy(attempt = true)

    override implicit def string(s: String): MyParser[String] =
      MyParser(
        "s",
        { (in, prevMatched) =>
          Either.cond(
            in.startsWith(s),
            (s, s),
            ParseError(
              List(
                (
                  Location(in, prevMatched.length),
                  s"Expected to start with ${s}"
                )
              )
            )
          )
        }
      )

    override implicit def regex(r: Regex): MyParser[String] =
      MyParser(
        "regex",
        { (in, prevMatched) =>
          r.findFirstIn(in)
            .map(s => Right((s, s)))
            .getOrElse(
              Left(
                ParseError(
                  List(
                    (
                      Location(in, prevMatched.length),
                      s"Expected to match regex ${r}"
                    )
                  )
                )
              )
            )
        }
      )

    override def or[A](p1: MyParser[A], p2: => MyParser[A]): MyParser[A] =
      MyParser(
        s"or(${p1.name}, ${p2.name})",
        { (in, prevMatched) =>
          val res = p1.matches(in, prevMatched)
          if (
            res.isLeft && (res.left.exists(
              _.stack.head._1.offset == prevMatched.length // if no chars were consumed do not commit 1st branch
            ) || p1.attempt)
          ) p2.matches(in, prevMatched)
          else res
        }
      )

    override def flatMap[A, B](p: MyParser[A])(
        f: A => MyParser[B]
    ): MyParser[B] =
      MyParser(
        s"flatmap(${p.name})",
        { (in, prevMatched) =>
          p.matches(in, prevMatched)
            .map { case (matched, a) =>
              val p2 = f(a)
              p2.copy(
                labels = p.labels ++ p2.labels,
                attempt = p.attempt || p2.attempt
              ).matches(
                in.stripPrefix(matched),
                matched
              ).map { case (ms, a) => (matched ++ ms, a) }
            }
            .flatten
        }
      )

    override def slice[A](p: MyParser[A]): MyParser[String] =
      MyParser(
        s"slice(${p.name})",
        { (in, prevMatched) =>
          p.matches(in, prevMatched).map { case (matched, _) =>
            (matched, matched)
          }
        }
      )

    override def succeed[A](a: A): MyParser[A] =
      MyParser("succeed", (_, _) => Right(("", a)))

    override def fail[A]: MyParser[A] =
      MyParser(
        "fail",
        (in, prevMatched) =>
          Left((ParseError(List((Location(in, prevMatched.length), "fail")))))
      )
  }

  object Parsers {
    type Parser[+A] = Location => Result[A]

    sealed trait Result[+A] {
      def mapError(f: ParseError => ParseError): Result[A] = this match {
        case Failure(e, c) => Failure(f(e), c)
        case _             => this
      }

      def uncommit: Result[A] = this match {
        case Failure(e, true) => Failure(e, false)
        case _                => this
      }

      def addCommit(isCommitted: Boolean): Result[A] = this match {
        case Failure(e, c) => Failure(e, c || isCommitted)
        case _             => this
      }

      def advanceSuccess(n: Int): Result[A] = this match {
        case Success(a, m) => Success(a, n + m)
        case _             => this
      }

      def toEither: Either[ParseError, A] = this match {
        case Success(get, _) => Right(get)
        case Failure(get, _) => Left(get)
      }
    }
    case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
    case class Failure(get: ParseError, isCommitted: Boolean)
        extends Result[Nothing]

    object ParsersImpl extends Parsers[Parser] {
      // Ex15
      override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
        p(Location(input)).toEither

      override def label[A](msg: String)(p: Parser[A]): Parser[A] =
        s => p(s).mapError(_.label(msg))

      override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
        s => p(s).mapError(_.push(s, msg))

      override def attempt[A](p: Parser[A]): Parser[A] =
        s => p(s).uncommit

      // Ex13
//      override implicit def string(s: String): Parser[String] = {
//        (loc: Location) =>
//          if (loc.input.startsWith(s)) Success(s, s.length)
//          else Failure(loc.toError("Expected: " + s))
//      }
      // Ex14
      override implicit def string(s: String): Parser[String] =
        scope(s"Failed to match: $s") { (loc: Location) =>
          if (s.length > 0) {
            loc.input.zipWithIndex
              .find { case (c, i) => c != s.charAt(i) }
              .map { case (c, i) =>
                Failure(
                  loc.toError(
                    s"Expected ${s.charAt(i)}, found $c at position $i"
                  ),
                  i > 0 // commit if consumed at least one char
                )
              }
              .getOrElse(Success(s, s.length))
          } else Success(s, s.length)
        }

      // Ex13
      override implicit def regex(r: Regex): Parser[String] = {
        (loc: Location) =>
          r.findFirstIn(loc.input)
            .map { m => Success(m, m.length) }
            .getOrElse(
              Failure(loc.toError("Expected: " + r), false)
            ) // never commit
      }

      override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
        s =>
          p1(s) match {
            case Failure(e, false) => p2(s)
            case r                 => r
          }

      override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
        s =>
          p(s) match {
            case Success(a, n) =>
              f(a)(s.advanceBy(n))
                .addCommit(n != 0)
                .advanceSuccess(n)
            case e @ Failure(_, _) => e
          }

      // Ex13
      override def slice[A](p: Parser[A]): Parser[String] =
        (loc: Location) =>
          p(loc) match {
            case Failure(get, isCommitted) => Failure(get, isCommitted)
            case Success(get, charsConsumed) =>
              Success(loc.input.substring(0, charsConsumed), charsConsumed)
          }

      // Ex13
      override def succeed[A](a: A): Parser[A] = (loc: Location) =>
        Success(a, 0)

      // Ex15
      override def fail[A]: Parser[A] = s => Failure(s.toError("fail"), false)
    }
  }

  def main(args: Array[String]): Unit = {
//    import MyParsers._ // Testing Ex12 Impl
//    assert(run(char('c'))('c'.toString) == Right('c'))
//    assert(run("s")("s") == Right("s"))
//    assert(run("abra" | "cadabra")("cadabra") == Right("cadabra"))
//    assert(
//      run(listOfN(3, "ab" | "cad"))("ababcad") == Right(
//        List("ab", "ab", "cad")
//      )
//    )
//    assert(run(numA)("aaa") == Right(3))
//    assert(run(numA)("b") == Right(0))

    /////////////// DEBUGGING
//    val jpWhitespace = regex("[ \\t\\n]*".r)
//    def withWhitespace(p: MyParser[String]): MyParser[String] =
//      for {
//        _ <- jpWhitespace
//        s <- p
//        _ <- jpWhitespace
//      } yield s
//
//    val validString = regex("\"[a-zA-Z0-9]*\"".r)
//
//    val separated = for (_ <- string(","); e <- regex("[a-z]+".r)) yield e
//    def v: MyParser[String] = validString | array
//    def array = for {
//      _ <- string("[")
//      _ <- or(map2(v, separated.many)(_ :: _), succeed(List()))
//      _ <- withWhitespace("]")
//    } yield "arrr"
//    println(run(array)("[]"))
    ///////////////

    import JSON._
//    assert(run(jsonParser(MyParsers))("true") == Right(JBool(true)))
//    assert(run(jsonParser(MyParsers))("123") == Right(JNumber(123)))
//    assert(run(jsonParser(MyParsers))("\"abc\"") == Right(JString("abc")))

    ////// BROKEN
//    println(run(for {
//      _ <- string("[")
//      _ <- string("a") | succeed()
//      _ <- string("]")
//    } yield "")("[]"))
//    println(run(jsonParser(MyParsers))("[\"a\"]"))
//    assert(
//      run(jsonParser(MyParsers))("[\"a\", 1, null]") == Right(
//        JArray(IndexedSeq(JString("a"), JNumber(1), JNull))
//      )
//    )
//    assert(
//      run(jsonParser(MyParsers))("{\"a\": 1}") == Right(
//        JObject(Map("a" -> JNumber(1)))
//      )
//    )

    // Testing Ex15
    import ParsersImpl._
    assert(run(jsonParser(ParsersImpl))("true") == Right(JBool(true)))
    assert(run(jsonParser(ParsersImpl))("123") == Right(JNumber(123)))
    assert(run(jsonParser(ParsersImpl))("\"abc\"") == Right(JString("abc")))

    /// BORKEN
//    assert(
//      run(jsonParser(ParsersImpl))("[\"a\", 1, null]") == Right(
//        JArray(IndexedSeq(JString("a"), JNumber(1), JNull))
//      )
//    )
//    assert(
//      run(jsonParser(ParsersImpl))("{\"a\": 1}") == Right(
//        JObject(Map("a" -> JNumber(1)))
//      )
//    )
  }
}
