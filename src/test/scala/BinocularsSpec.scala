class BinocularsSpec extends munit.FunSuite {
  def seq() = (0 until 360).map(_ => Seq.empty[String]).toList

  test("center 0, angle 30, filter shouldn't include two elements in the same position") {
    val horizon = seq().updated(0, Seq("b"))

    assert(Binoculars.filterByCenterAngle(horizon, 0, 30) == List(List("b")))
  }

  test("center 20, angle 20, filter shouldn't include two elements in the same position") {
    val horizon = seq().updated(20, Seq("b"))

    assert(Binoculars.filterByCenterAngle(horizon, 20, 20) == List(List("b")))
  }
}
