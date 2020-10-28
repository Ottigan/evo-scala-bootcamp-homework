package homework.cats

import cats.implicits._

object Contravariant extends App {

  /**
    *  The `Contravariant` type class is for functors that define a `contramap` function with the following type:
    *  def contramap[A, B](fa: F[A])(f: B => A): F[B]
    *  It looks like regular (also called `Covariant`) Functor’s map, but with the f transformation reversed.
    *  Generally speaking, if you have some context F[A] for type A, and you can get an A value out of a B value —
    *  Contravariant allows you to get the F[B] context for B.
    *  Examples of Contravariant instances are Show and scala.math.Ordering (along with cats.kernel.Order).
    *
    *  Let's explore it further in the scope of scala.math.Ordering, while attempting to sort some Cards
    */

  val testCombination = Combination.create(List(
    Card(Rank.create("6"), Suit.create("c")),
    Card(Rank.create("4"), Suit.create("c")),
    Card(Rank.create("5"), Suit.create("c")),
    Card(Rank.create("3"), Suit.create("c")),
    Card(Rank.create("7"), Suit.create("c"))
  ))

  println(testCombination)
  // Some(Combination(List(Card(Six,Club), Card(Four,Club), Card(Five,Club), Card(Three,Club), Card(Seven,Club))))

  /**
    * There’s also a method, called `by`, that creates new Orderings out of existing ones:
    * def by[T, S](f: T => S)(implicit ord: Ordering[S]): Ordering[T]
    * In fact, it is just `contramap`, defined in a slightly different way! We supply T => S to receive F[S] => F[T] back.
    * So let’s use it to our advantage and get Ordering[Card] for free:
    */

  implicit val cardOrdering: Ordering[Card] = Ordering.by(_.rank.rank)

  val sortedTestCombination: Option[Combination] = for {
    x           <- testCombination
    sortedCombo <- Combination.create(x.combination.sorted)
  } yield sortedCombo

  println(sortedTestCombination)
  // Some(Combination(List(Card(Three,Club), Card(Four,Club), Card(Five,Club), Card(Six,Club), Card(Seven,Club))))

  // As a result we have a neatly ordered Straight Flush
}
