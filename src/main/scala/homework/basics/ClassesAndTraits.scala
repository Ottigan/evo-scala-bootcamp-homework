package homework.basics

object ClassesAndTraits extends App {

  sealed trait Shape2D extends Located2D with Bounded2D with Movable2D {
    def area: Double
  }

  sealed trait Shape3D extends Located3D with Bounded3D with Movable3D {
    def surfaceArea: Double
    def volume: Double
  }

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
    override def area: Double = 0
    override def move(dx: Double, dy: Double): Point2D = Point2D(x + dx, y + dy)
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
    override def surfaceArea: Double = 0
    override def volume: Double = 0
    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)
  }

  final case class Circle(x: Double, y: Double, radius: Double) extends Shape2D {
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def area: Double = (Math.PI * Math.pow(radius, 2)).round
    override def move(dx: Double, dy: Double): Circle = Circle(x + dx, y + dy, radius)
  }

  final case class Sphere(x: Double, y: Double, z: Double, radius: Double) extends Shape3D {
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - radius
    override def maxZ: Double = z + radius
    override def surfaceArea: Double = 4.0 * Math.PI * Math.pow(radius, 2).round
    override def volume: Double = (4.0 / 3.0) * (Math.PI * Math.pow(radius, 3)).round
    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(x + dx, y + dy, z + dz, radius)
  }

  final case class Rectangle(x: Double, y: Double, length: Double, height: Double) extends Shape2D {
    override def minX: Double = x - length / 2
    override def maxX: Double = x + length / 2
    override def minY: Double = y - height / 2
    override def maxY: Double = y + height / 2
    override def area: Double = length * height
    override def move(dx: Double, dy: Double): Rectangle = Rectangle(x + dx, y + dy, length, height)
  }

  final case class Cuboid(x: Double, y: Double, z: Double, length: Double, height: Double, width: Double) extends Shape3D {
    override def minX: Double = x - length / 2
    override def maxX: Double = x + length / 2
    override def minY: Double = y - height / 2
    override def maxY: Double = y + height / 2
    override def minZ: Double = z - width / 2
    override def maxZ: Double = z + width / 2
    override def surfaceArea: Double = 2.0 * (length * height + length * width + height * width)
    override def volume: Double = length * height * width
    override def move(dx: Double, dy: Double, dz: Double): Cuboid = Cuboid(x + dx, y + dy, z + dz, length, height, width)
  }

  final case class Square(x: Double, y: Double, side: Double) extends Shape2D {
    override def minX: Double = x - side / 2
    override def maxX: Double = x + side / 2
    override def minY: Double = y - side / 2
    override def maxY: Double = y + side / 2
    override def area: Double = Math.pow(side, 2)
    override def move(dx: Double, dy: Double): Square = Square(x + dx, y + dy, side)
  }

  final case class Cube(x: Double, y: Double, z: Double, edge: Double) extends Shape3D {
    override def minX: Double = x - edge / 2
    override def maxX: Double = x + edge / 2
    override def minY: Double = y - edge / 2
    override def maxY: Double = y + edge / 2
    override def minZ: Double = z - edge / 2
    override def maxZ: Double = z + edge / 2
    override def surfaceArea: Double = 6.0 * Math.pow(edge, 2)
    override def volume: Double = Math.pow(edge, 3)
    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(x + dx, y + dy, z + dz, edge)
  }

  final case class Triangle(v1: Point2D, v2: Point2D, v3: Point2D) extends Shape2D {
    implicit val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering

    val vertices: List[Point2D] = v1 :: v2 :: v3 :: Nil
    override def x: Double = vertices.map(_.x).sum / 3
    override def y: Double = vertices.map(_.y).sum / 3
    override def minX: Double = vertices.map(_.x).min
    override def maxX: Double = vertices.map(_.x).max
    override def minY: Double = vertices.map(_.y).min
    override def maxY: Double = vertices.map(_.y).max
    override def area: Double = 0.5 * (v1.x * (v2.y - v3.y) + v2.x * (v3.y - v1.y) + v3.x * (v1.y - v2.y))
    override def move(dx: Double, dy: Double): Triangle = Triangle(v1.move(dx, dy), v2.move(dx, dy), v3.move(dx, dy))
  }

  final case class Pyramid(v1: Point3D, v2: Point3D, v3: Point3D, v4: Point3D) extends Shape3D {
    implicit val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering

    val vertices: List[Point3D] = v1 :: v2 :: v3 :: Nil
    override def x: Double = vertices.map(_.x).sum / 3
    override def y: Double = vertices.map(_.y).sum / 3
    override def z: Double = vertices.map(_.z).sum / 3
    override def minX: Double = vertices.map(_.x).min
    override def maxX: Double = vertices.map(_.x).max
    override def minY: Double = vertices.map(_.y).min
    override def maxY: Double = vertices.map(_.y).max
    override def minZ: Double = vertices.map(_.z).min
    override def maxZ: Double = vertices.map(_.z).max
    override def surfaceArea: Double = ???
    override def volume: Double = ???
    override def move(dx: Double, dy: Double, dz: Double): Pyramid = ???
  }

  def minimumBoundingRectangle(objects: Set[Bounded2D]): Rectangle = {
    implicit val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering

    def minX: Double = objects.map(_.minX).min
    def maxX: Double = objects.map(_.maxX).max
    def minY: Double = objects.map(_.minY).min
    def maxY: Double = objects.map(_.maxY).max
    val length: Double = maxX - minX
    val height: Double = maxY - minY
    val x: Double = maxX - length / 2
    val y: Double = maxY - height / 2

    Rectangle(x, y, length, height)
  }

  def describe(x: Shape2D): String = x match {
    case Point2D(x, y) => s"Point(x = $x, y = $y)"
    case Circle(x, y, radius) => s"Circle(centerX = $x, centerY = $y, radius = $radius)"
    case Rectangle(x, y, length, height) => s"Rectangle(centerX = $x, centerY = $y, length = $length, height = $height)"
    case Square(x, y, side) => s"Square(centerX = $x, centerY = $y, sides = $side)"
    case Triangle(v1, v2, v3) => s"Triangle(vertex#1 = $v1, vertex#2 = $v2, vertex#3 = $v3)"
  }
}
