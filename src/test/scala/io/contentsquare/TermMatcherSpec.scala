package io.contentsquare

import scala.util.Random

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Test.Parameters
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.Checkers.check

class TermMatcherSpec extends FlatSpec with Matchers {

  val words = List(
    "Pandora",
    "Pinterest",
    "Paypal",
    "Pg&e",
    "Project free tv Priceline",
    "Press democrat",
    "Progressive",
    "Project runway",
    "Proactive",
    "Programming",
    "Progeria",
    "Progesterone",
    "Progenex",
    "Procurable",
    "Processor",
    "Proud",
    "Print",
    "Prank",
    "Bowl",
    "Owl",
    "River",
    "Phone",
    "Kayak",
    "Stamps",
    "Reprobe"
  )

  val shufflingWords: Gen[List[String]] = {
    val r = new Random()
    Gen.delay(Gen.const(r.shuffle(words)))
  }

  "TermMatcher" should "return empty list when term is empty" in {
    val matcher = TermMatcher(List("ABC"))

    val tested = matcher("")
    tested shouldBe Seq.empty[String]
  }

  "TermMatcher" should "return terms starting with input" in {
    val matcher = TermMatcher(List("ABC", "ABD", "other"))

    val tested = matcher("AB")
    tested shouldBe Seq("ABC", "ABD")
  }

  "TermMatcher" should "return terms in sorted order" in {
    val matcher = TermMatcher(List("ABD", "ABC"))

    val tested = matcher("AB")
    tested shouldBe Seq("ABC", "ABD")
  }

  "TermMatcher" should "be case insensitive" in {
    val matcher = TermMatcher(List("ABD", "abc"))

    val tested = matcher("ab")
    tested shouldBe Seq("abc", "ABD")
  }

  "TermMatcher" should "return maximum 4 results" in {
    val matcher = TermMatcher(List("abc", "abd", "abe", "abf", "abg"))

    val tested = matcher("ab")
    tested shouldBe Seq("abc", "abd", "abe", "abf")
  }

  "TermMatcher" should "be associatively merged" in {
    val shufflingTupleWords: Gen[(List[String], List[String])] = for {
      list1 <- shufflingWords
      list2 <- shufflingWords
    } yield (list1, list2)


    check(forAll(shufflingTupleWords) { terms: (List[String], List[String]) =>
      TermMatcher(terms._1) == TermMatcher(terms._2)
    })
  }

  "TermMatcher" should "work as 'startWith' String's method" in {
    val matcher = TermMatcher(words)

    val inputGen: Gen[String] = {
      for {
        w <- Gen.pick(1, words).map(_.head)
        i <- Gen.chooseNum(1, w.length)
      } yield w.take(i)
    }

    check(forAll(inputGen) { input: String =>
      matcher(input) == words.filter(_.startsWith(input)).take(4).sorted
    }, Parameters.default.withMinSuccessfulTests(5000))
  }
}
