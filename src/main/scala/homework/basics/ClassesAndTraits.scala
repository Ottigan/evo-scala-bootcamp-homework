package homework.basics

object ClassesAndTraits {

  sealed trait Shape extends Located with Bounded with Movable

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable {
    def move(dx: Double, dy: Double): Shape
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def move(dx: Double, dy: Double): Circle = Circle(x + dx, y + dy, radius)
  }

  final case class Rectangle(centerX: Double, centerY: Double, width: Double, height: Double) extends Shape {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - width / 2
    override def maxX: Double = x + width / 2
    override def minY: Double = y - height / 2
    override def maxY: Double = y + height / 2
    override def move(dx: Double, dy: Double): Rectangle = Rectangle(x + dx, y + dy, width, height)

  }

  final case class Triangle(points: Set[Point]) extends Shape {
    override def x: Double = points.map(_.x).sum / 3
    override def y: Double = points.map(_.y).sum / 3
    override def minX: Double = points.map(_.x).min
    override def maxX: Double = points.map(_.x).max
    override def minY: Double = points.map(_.y).min
    override def maxY: Double = points.map(_.y).max
    override def move(dx: Double, dy: Double): Triangle = Triangle(points.map(_.move(dx, dy)))
  }

  final case class Square(centerX: Double, centerY: Double, side: Double) extends Shape {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - side / 2
    override def maxX: Double = x + side / 2
    override def minY: Double = y - side / 2
    override def maxY: Double = y + side / 2
    override def move(dx: Double, dy: Double): Square = Square(x + dx, y + dy, side)
  }

}
