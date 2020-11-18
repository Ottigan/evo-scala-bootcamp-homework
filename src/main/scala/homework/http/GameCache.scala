package homework.http

final case class GameCache(games: List[Game]) {
  def addGame(game: Game): GameCache = GameCache.create(games :+ game)
  def removeGame(game: Game): GameCache = GameCache.create(games.filterNot(_ == game))
  def updateGame(game: Game): GameCache = removeGame(game).addGame(game.copy(attempts = game.attempts - 1))
}

object GameCache {
  def create(games: List[Game]): GameCache = GameCache(games)
}
