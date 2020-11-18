package homework.http

sealed abstract case class GameCache private (game: List[Game]) {
  def add(game: Game): Unit
  def get(id: String): Option[Game]
  def getAll: List[Game]
  def remove(game: Game): Unit
  def update(game: Game): Unit
}

object GameCache {
  def create(games: List[Game]): GameCache = new GameCache(games) {
    var cache: List[Game] = List.empty[Game]
    override def add(game: Game): Unit = cache = cache :+ game
    override def get(id: String): Option[Game] = cache.find(_.id == id)
    override def getAll: List[Game] = cache
    override def remove(game: Game): Unit = cache = cache.filterNot(_ == game)
    override def update(game: Game): Unit = {
      remove(game)
      add(game.copy(attempts = game.attempts - 1))
    }
  }
}
