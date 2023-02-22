class BinocularsSpec extends munit.FunSuite {
  def seq() = (0 until 360).map(_ => Seq.empty[String]).toList

  test("filter shouldn't include two elements in the same position") {
    val horizon = seq().updated(0, Seq("b"))

    assert(Binoculars.filterByCenterAngle(horizon, 0, 30) == List(List("b")))
  }
}
