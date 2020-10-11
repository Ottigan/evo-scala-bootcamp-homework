package homework.sandbox

object Sandbox extends App {

  abstract class Animal {
    def name: String
  }
  case class Cat(name: String) extends Animal
  case class Dog(name: String) extends Animal

  def printAnimalNames(animals: List[Animal]): Unit =
    animals.foreach {
      animal => println(animal.name)
    }

  val cats: List[Cat] = List(Cat("Whiskers"), Cat("Tom"))
  val dogs: List[Dog] = List(Dog("Fido"), Dog("Rex"))

  // prints: Whiskers, Tom
  printAnimalNames(cats)

  // prints: Fido, Rex
  printAnimalNames(dogs)

  abstract class Printer[-A] {
    def print(value: A): Unit
  }

  class AnimalPrinter extends Printer[Animal] {
    def print(animal: Animal): Unit =
      println("The animal's name is: " + animal.name)
  }

  class CatPrinter extends Printer[Cat] {
    def print(cat: Cat): Unit =
      println("The cat's name is: " + cat.name)
  }

  def printMyCat(printer: Printer[Cat], cat: Cat): Unit =
    printer.print(cat)

  val catPrinter: Printer[Cat] = new CatPrinter
  val animalPrinter: Printer[Animal] = new AnimalPrinter

  printMyCat(catPrinter, Cat("Boots"))
  printMyCat(animalPrinter, Cat("Boots"))

  class Container[A](value: A) {
    private var _value: A = value
    def getValue: A = _value
    def setValue(value: A): Unit = {
      _value = value
    }
  }

  val catContainer: Container[Cat] = new Container(Cat("Felix"))
  //val animalContainer: Container[Animal] = catContainer
  //animalContainer.setValue(Dog("Spot"))
  //val cat: Cat = catContainer.getValue // Oops, we'd end up with a Dog assigned to a Cat

}
