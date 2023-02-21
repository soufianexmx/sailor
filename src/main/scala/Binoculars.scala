/** Binoculars are our way of looking at things on the horizon. Horizon is defined as a sequence of sequences of light
  * marks. The horizon contains 360 (as in 360 degrees) elements, each containing more or no light marks. The center of
  * the binoculars is the direction of where the binoculars are pointing, e.g. center = 45, means you're looking
  * North-East The angle defines how much of the horizon is visible through the binoculars. The higher the zoom, the
  * lower the angle.
  */
object Binoculars {

  def filterByCenterAngle(
      lightMarksOnHorizon: Seq[Seq[String]],
      center: Int,
      angle: Int
  ): Seq[Seq[String]] = {
    val plus = center + (angle / 2)
    val minus = center - (angle / 2)

    val plusSeq =
      if (plus <= 360) lightMarksOnHorizon.slice(center, plus)
      else
        lightMarksOnHorizon.slice(0, plus - 360) ++
          lightMarksOnHorizon.slice(center, 360)

    val minusSeq =
      if (minus >= 0) lightMarksOnHorizon.slice(minus, center)
      else
        lightMarksOnHorizon.slice(360 + minus, 360) ++
          lightMarksOnHorizon.slice(0, center)

    (plusSeq ++ minusSeq).filter(_.nonEmpty)
  }

  /** Counts vessels on the horizon, given the direction they are pointed to
    * @param lightMarksOnHorizon
    *   the entire horizon 0 - 360
    * @param center
    *   \- degree number where the binoculars are pointing
    * @param angle
    *   \- the zoom level, how many degrees of the horizon are visible
    * @param directionFilter
    *   \- the filter function which defines whether we're searching for vessels heading left, right, or ...
    * @return
    *   the number of ships visible in the subset of the horizon narrowed by the angle, applying the direction filter
    */
  def countVessels(
      lightMarksOnHorizon: Seq[Seq[String]],
      center: Int,
      angle: Int,
      directionFilter: (Vessel) => Boolean
  ): Int =
    filterByCenterAngle(lightMarksOnHorizon, center, angle)
      .map(Vessel)
      .count(directionFilter)

  /** Finds the degree to which binoculars should be pointed, where most vessels can be seen.
    * @param lightMarksOnHorizon
    *   the entire horizon 0 - 360
    * @param angle
    *   \- the zoom level, how many degrees of the horizon are visible
    *
    * @return
    *   the center of the binoculars where they should be pointed to
    */
  def mostVessels(lightMarksOnHorizon: Seq[Seq[String]], angle: Int): Int =
    (0 to 360)
      .map(center => center -> countVessels(lightMarksOnHorizon, center, angle, _ => true))
      .toMap
      .maxBy(_._2)
      ._1
}
