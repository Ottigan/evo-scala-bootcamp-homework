package homework.basics

object ClassesAndTraits extends App {

  sealed trait Shape2D extends Located2D with Bounded2D with Movable2D

  sealed trait Shape3D extends Located3D with Bounded3D with Movable3D

  sealed trait Located2D {
    def x: Double
    def y: Double
  }

  sealed trait Located3D {
    def x: Double
    def y: Double
    def z: Double
  }

  sealed trait Bounded2D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Bounded3D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable2D {
    def move(dx: Double, dy: Double): Shape2D
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  object Origin2D extends Located2D {
    override def x: Double = 0
    override def y: Double = 0
  }

  object Origin3D extends Located3D {
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
  }

  final case class Point2D(x: Double, y: Double) extends Shape2D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def move(dx: Double, dy: Double): Point2D = Point2D(x + dx, y + dy)
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)
  }

  final case class Circle2D(x: Double, y: Double, radius: Double) extends Shape2D {
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def move(dx: Double, dy: Double): Circle2D = Circle2D(x + dx, y + dy, radius)
  }

  final case class Sphere(x: Double, y: Double, z: Double, radius: Double) extends Shape3D {
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - radius
    override def maxZ: Double = z + radius
    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(x + dx, y + dy, z + dz, radius)
  }

  final case class Rectangle2D(x: Double, y: Double, width: Double, height: Double) extends Shape2D {
    override def minX: Double = x - width / 2
    override def maxX: Double = x + width / 2
    override def minY: Double = y - height / 2
    override def maxY: Double = y + height / 2
    override def move(dx: Double, dy: Double): Rectangle2D = Rectangle2D(x + dx, y + dy, width, height)

  }

  final case class Square2D(x: Double, y: Double, side: Double) extends Shape2D {
    override def minX: Double = x - side / 2
    override def maxX: Double = x + side / 2
    override def minY: Double = y - side / 2
    override def maxY: Double = y + side / 2
    override def move(dx: Double, dy: Double): Square2D = Square2D(x + dx, y + dy, side)
  }

  final case class Triangle2D(vertex1: Point2D, vertex2: Point2D, vertex3: Point2D) extends Shape2D {
    val vertices: List[Point2D] = vertex1 :: vertex2 :: vertex3 :: Nil
    override def x: Double = vertices.map(_.x).sum / 3
    override def y: Double = vertices.map(_.y).sum / 3
    override def minX: Double = vertices.map(_.x).min
    override def maxX: Double = vertices.map(_.x).max
    override def minY: Double = vertices.map(_.y).min
    override def maxY: Double = vertices.map(_.y).max
    override def move(dx: Double, dy: Double): Triangle2D = Triangle2D(vertex1.move(dx, dy), vertex2.move(dx, dy), vertex3.move(dx, dy))
  }

  def minimumBoundingRectangle(objects: Set[Bounded2D]): Rectangle2D = {
    implicit val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering

    def minX: Double = objects.map(_.minX).min
    def maxX: Double = objects.map(_.maxX).max
    def minY: Double = objects.map(_.minY).min
    def maxY: Double = objects.map(_.maxY).max
    val width: Double = maxX - minX
    val height: Double = maxY - minY
    val x: Double = maxX - width / 2
    val y: Double = maxY - height / 2

    Rectangle2D(x, y, width, height)
  }

  // Pattern matching and exhaustiveness checking
  def describe(x: Shape2D): String = x match {
    case Point2D(x, y) => s"Point(x = $x, y = $y)"
    case Circle2D(x, y, radius) => s"Circle(centerX = $x, centerY = $y, radius = $radius)"
    case Rectangle2D(x, y, width, height) => s"Rectangle(centerX = $x, centerY = $y, width = $width, height = $height)"
    case Square2D(x, y, side) => s"Square(centerX = $x, centerY = $y, sides = $side)"
    case Triangle2D(v1, v2, v3) => s"Triangle(vertex#1 = $v1, vertex#2 = $v2, vertex#3 = $v3)"
  }
}
