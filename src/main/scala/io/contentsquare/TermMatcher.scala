package io.contentsquare

/*
  This is a Tree structure implementation of a String Matcher.

  There's three main elements :
    - RootTermMatcher is the top element of the structure. It have a reference to all others elements.
    - BranchNode have a reference to a char and to one or many sub elements.
    - LeafNode just have a reference to a char.

  To find terms matching with the input string, we just have to walk through all the matching character
  and return the corresponding terms.

  Example :

  We can create a TermMatcher with a list of terms : ("Hello", "Hi")

                            RootTermMatcher
                                  |
                            BranchNode("H")
                            /           \
                           /             \
                     BranchNode("e")   LeafNode("i")
                          |
                     BranchNode("l")
                          |
                     BranchNode("l")
                          |
                      LeafNode("o")


  This structure is expensive to build but fast to find matching terms.
 */
trait TermMatcher {
  def apply(term: String, numberOfWord: Int): Seq[String]

  final def apply(term: String): Seq[String] = apply(term, TermMatcher.MaximumResult)
}

private[this] sealed trait CharNode extends TermMatcher {
  def currentChar: Char
}

private[this] case class LeafNode(value: Char) extends CharNode {
  override def currentChar: Char = value

  override def apply(term: String, numberOfWord: Int): Seq[String] = {
    if (term.isEmpty && numberOfWord > 0) List(s"$value")
    else Nil
  }
}

private[this] case class BranchNode(current: LeafNode, next: Map[Char, CharNode], isWord: Boolean) extends CharNode {

  override def currentChar: Char = current.currentChar

  private def getWords(numberOfWord: Int): Seq[String] = {
    val firstChars = next.keys.toSeq.sorted.take(numberOfWord)
    val (list, _) = firstChars.foldLeft[(Seq[String], Int)]((Nil, numberOfWord)) { case ((words, nw), c) =>
      val r = next(c).apply("", nw)
      (words ++ r, nw - r.length)
    }
    list
  }

  private def subApply(head: Char, tail: String, numberOfWord: Int): Seq[String] = {
    def op(c: Char) = {
      next.get(c)
        .map(_.apply(tail, numberOfWord))
        .getOrElse(Nil)
    }

    val lowerValue = op(head.toLower)
    val upperValue = op(head.toUpper)
    (lowerValue ++ upperValue).sortBy(_.toLowerCase).take(numberOfWord)
  }

  def apply(term: String, numberOfWord: Int): Seq[String] = {

    if (numberOfWord > 0) {
      val nw = if (isWord) numberOfWord - 1 else numberOfWord

      val words = {
        if (term.nonEmpty) {
          subApply(term.head, term.tail, nw)
        } else {
          getWords(nw)
        }
      }

      if (isWord) s"${current.value}" +: words.map(w => current.value + w)
      else words.map(w => current.value + w)
    } else {
      Nil
    }
  }
}

private[this] case class RootTermMatcher(next: Map[Char, CharNode]) extends TermMatcher {

  private def subApply(head: Char, tail: String, numberOfWord: Int): Seq[String] = {
    def op(c: Char) = {
      next.get(c)
        .map(_.apply(tail, numberOfWord))
        .getOrElse(Nil)
    }
    val lowerValue = op(head.toLower)
    val upperValue = op(head.toUpper)
    (lowerValue ++ upperValue).sortBy(_.toLowerCase).take(numberOfWord)
  }

  def apply(term: String, numberOfWord: Int): Seq[String] = {
    if (term.nonEmpty) {
      subApply(term.head, term.tail, numberOfWord)
    } else {
      Nil
    }
  }
}

object TermMatcher {

  val MaximumResult = 4

  def apply(listOfTerm: List[String]): TermMatcher = {

    def buildTermNode(term: String): Option[CharNode] = {
      if (term.nonEmpty) {
        val lastChar = term.last

        val tree = term.init.reverseIterator.foldLeft[CharNode](LeafNode(lastChar)) { case (node, c) =>
          BranchNode(LeafNode(c), Map(node.currentChar -> node), isWord = false)
        }

        Some(tree)
      } else {
        None
      }
    }

    def merge(list: List[CharNode]): Map[Char, CharNode] = {

      def listToMap(nodeList: List[CharNode]): Map[Char, CharNode] = {
        nodeList.map(n => n.currentChar -> n).toMap
      }

      def mergeNode(nodeA: CharNode, nodeB: CharNode): Option[CharNode] = (nodeA, nodeB) match {
        case (lna@LeafNode(a), LeafNode(b)) =>
          if (a == b) Some(lna)
          else None

        case (LeafNode(a), bnb@BranchNode(LeafNode(b), _, _)) =>
          if (a == b) Some(bnb.copy(isWord = true))
          else None

        case (bna@BranchNode(LeafNode(a), _, _), LeafNode(b)) =>
          if (a == b) Some(bna.copy(isWord = true))
          else None

        case (bna@BranchNode(LeafNode(a), _, _), bnb@BranchNode(LeafNode(b), _, _)) =>
          if (a == b) {
            val nodeMap = listToMap(mergeAll(bna.next.values.toList ++ bnb.next.values.toList))
            Some(BranchNode(LeafNode(a), nodeMap, bna.isWord || bnb.isWord))
          } else {
            None
          }
      }

      def mergeAll(list: List[CharNode]): List[CharNode] = {
        list match {
          case Nil => Nil
          case current :: xs =>
            val (modifiedNode, modifiedList) = xs.foldLeft((current, xs)) { case ((c, l), n) =>
              mergeNode(c, n).map(newNode => (newNode, l.filter(_ != n))).getOrElse((c, l))
            }

            modifiedNode :: mergeAll(modifiedList)
        }
      }

      listToMap(mergeAll(list))
    }

    val terms = merge(listOfTerm.flatMap(buildTermNode))
    RootTermMatcher(terms)
  }
}
