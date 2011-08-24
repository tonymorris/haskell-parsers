package MyParser

import Character._

case class Parser[A](parse: List[Char] => Option[(List[Char], A)]) {
  def |||(p2: => Parser[A]): Parser[A] = Parser(s => this parse s match {
    case v@Some(_) => v
    case None => p2 parse s
  })

  def >>>[B](q: => Parser[B]): Parser[B] = Parser.bindParser[A, B](this, _ => q)
}

object Parser {
  def value[A](a: A): Parser[A] = Parser(s => Some(s, a))

  def failed[A]: Parser[A] = Parser(s => None)

  def character: Parser[Char] = Parser[Char] {
    case Nil => None
    case c::r => Some(r, c)
  }

  def mapParser[A, B](p: Parser[A], f: A => B): Parser[B] = Parser(s => p parse s match {
    case Some((r, c)) => Some(r, f(c))
    case None => None
  })

  def bindParser[A, B](p: Parser[A], f: A => Parser[B]): Parser[B] = Parser(s => p parse s match {
    case Some((r, c)) => f(c) parse r
    case None => None
  })

  def sequenceParser[A](ps: List[Parser[A]]): Parser[List[A]] = ps match {
    case Nil => value(Nil)
    case h::t => bindParser(h, (a: A) => mapParser(sequenceParser(t), (as: List[A]) => a :: as))
  }

  def thisMany[A](n: Int, p: Parser[A]): Parser[List[A]] = sequenceParser(List.make(n, p))

  def list[A](k: Parser[A]): Parser[List[A]] = many1(k) ||| value(Nil)

  def many1[A](k: Parser[A]): Parser[List[A]] = bindParser(k, (kx: A) => mapParser(list(k), (kkx: List[A]) => kx :: kkx))

  def satisfy(p: Char => Boolean): Parser[Char] = bindParser(character, (c: Char) => if(p(c)) value(c) else failed)

  def is(c: Char): Parser[Char] = satisfy(_ == c)

  val digit: Parser[Char] = satisfy(isDigit(_))

  val natural: Parser[Int] = mapParser(list(digit), (_: List[Char]).mkString.toInt)

  val space: Parser[Char] = satisfy(isWhitespace(_))

  val spaces: Parser[List[Char]] = many1(space)

  val lower: Parser[Char] = satisfy(isLowerCase(_))

  val upper: Parser[Char] = satisfy(isUpperCase(_))

  val alpha: Parser[Char] = satisfy(isLetter(_))

  val alphaNum: Parser[Char] = satisfy(isLetterOrDigit(_))
}

case class Person(age: Int, firstName: String, surname: String, gender: Char, phone: String)

object Person {
  import Parser._

  val ageParser: Parser[Int] = natural

  val firstNameParser: Parser[String] = bindParser(upper, (c: Char) => mapParser(list(lower), (cs: List[Char]) => (c :: cs).mkString))

  val surnameParser: Parser[String] = bindParser(upper, (c: Char) => bindParser(thisMany(5, lower), (cs: List[Char]) => mapParser(list(lower), (t: List[Char]) => (c :: cs ::: t).mkString)))

  val genderParser: Parser[Char] = is('m') ||| is('f')

  val phoneBodyParser: Parser[String] = mapParser(list(digit ||| is('.') ||| is('-')), (_: List[Char]).mkString)

  val phoneParser: Parser[String] = bindParser(digit, (d: Char) => bindParser(phoneBodyParser, (z: String) => mapParser(is('#'), (_: Char) => (d :: z.toList).mkString)))

  val personParser1: Parser[Person] = bindParser(ageParser, (age: Int) =>
                                      spaces >>>
                                      bindParser(firstNameParser, (firstName: String) =>
                                      spaces >>>
                                      bindParser(surnameParser, (surname: String) =>
                                      spaces >>>
                                      bindParser(genderParser, (gender: Char) =>
                                      spaces >>>
                                      bindParser(phoneParser, (phone: String) =>
                                      value(Person(age, firstName, surname, gender, phone)))))))

  implicit def PersonMonad[A](p: Parser[A]) = new {
    def map[B](f: A => B) = mapParser(p, f)
    def flatMap[B](f: A => Parser[B]) = bindParser(p, f)
  }

  val personParser2: Parser[Person] = for(age <- ageParser;
                                          _ <- spaces;
                                          firstName <- firstNameParser;
                                          _ <- spaces;
                                          surname <- surnameParser;
                                          _ <- spaces;
                                          gender <- genderParser;
                                          _ <- spaces;
                                          phone <- phoneParser)
                                      yield Person(age, firstName, surname, gender, phone)
}
