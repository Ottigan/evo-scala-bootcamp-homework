package homework.sandbox

object Sandbox extends App {

  //Let's call this thing a type-class!
  trait Show[T] {
    def apply(value: T): String
  }

  /*
    This is equivalent to def show[T](value: T)(implicit show: Show[T]): String = ...
    with the difference that the implicit argument is not named and can be obtained only using 'implicitly'.

    : Show syntax is called "a context bound"
   */
  def show[T: Show](value: T): String =
    implicitly[Show[T]].apply(value)

  object syntax {
    //our old friend implicit conversion but now with an implicit value requirement
    implicit class ShowOps[T: Show](inner: T) {
      def show: String = Sandbox.show(inner)
    }
  }

  object instances {
    /*
      Type-classes provide a way to create generic logic which can be extended to work on any type.

      Here we extend all the possible logic working on Show, to work on some standard library types.
     */

    //for String's
    implicit val stringShow: Show[String] = (value: String) => value
    //for Int's
    implicit val intShow: Show[Int] = (value: Int) => value.toString
    //even for any Seq[T] where T itself has a Show instance
    implicit def seqShow[T: Show]: Show[Seq[T]] =
      (value: Seq[T]) => value.map(show(_)).mkString("(", ", ", ")")
  }

  object Workspace {
    import instances._
    import syntax._

    /*
      And here we extend all the possible logic working on Show, to work on our custom types!
     */
    case class MyLuckyNumber(value: Int)
    object MyLuckyNumber {
      implicit val myLuckyNumberShow: Show[MyLuckyNumber] =
        (luckyNumber: MyLuckyNumber) => s"lucky ${luckyNumber.value}"
    }

    def showEverything(): Unit = {
      println(42.show)
      println("hello!".show)
      println(Seq("I", "am", "a", "ghost").show)
      println(Seq(1, 2, 3, 4, 5).show)
      println(Seq(MyLuckyNumber(13), MyLuckyNumber(99)).show)
    }

  }
  Workspace.showEverything()

}
