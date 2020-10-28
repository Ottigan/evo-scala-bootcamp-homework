package homework.cats

final case class Combination(combination: List[Card])
object Combination {
  def create(combination: List[Card]): Option[Combination] =
    if (combination.toSet.size == 5) Some(Combination(combination))
    else None
}
