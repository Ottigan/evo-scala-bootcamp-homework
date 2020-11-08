package homework.effects

import homework.effects.EffectsHomework1.IO
import org.scalatest.funsuite.AnyFunSuite

class EffectsHomework1Spec extends AnyFunSuite {

  test("...") {
    assert(IO(println("foo")).unsafeRunSync() == println("foo"))
  }

}
