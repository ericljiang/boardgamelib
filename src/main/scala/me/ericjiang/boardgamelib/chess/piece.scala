package me.ericjiang.boardgamelib.chess

sealed trait Piece {
  /**
   * The player that owns this piece.
   * @return player instance
   */
  def player: Player
  /**
   * Possible moves before accounting for universal checks. Universal checks that do not need to be applied include
   * moving into a piece owned by the same player, moving out of bounds, and moving into check.
   * @param position the current position for which to calculate possible moves
   * @param state the current game state
   * @return a set of possible moves
   */
  def moves(position: (Int, Int), state: ChessState): Set[MovePieceAction]
}

sealed trait SlidingDirection
case object Orthogonal extends SlidingDirection
case object Diagonal extends SlidingDirection

trait Sliding {
  this: Piece =>
  def slidingMoves(direction: SlidingDirection, position: (Int, Int), state: ChessState): Set[MovePieceAction] = {
    val (file, rank) = position
    val lines = direction match {
      case Orthogonal => Seq(
        for (newFile <- file + 1 to 8) yield (newFile, rank), // up
        for (newFile <- 1 until file reverse) yield (newFile, rank), // down
        for (newRank <- rank + 1 to 8) yield (file, newRank), // right
        for (newRank <- 1 until rank reverse) yield (file, newRank)) // left
      case Diagonal => Seq(
        (file + 1 to 8) zip (rank + 1 to 8), // up+right
        (file + 1 to 8) zip (0 until rank reverse), // up+left
        (1 until file reverse) zip (rank + 1 to 8), // down+right
        (1 until file reverse) zip (0 until rank reverse)) // down+left
    }
    lines.flatMap(_
      .foldLeft(Seq.empty[(Int, Int)]) { (valid, newPosition) =>
        valid.lastOption match {
          case None => valid :+ newPosition
          case Some(p) if !state.board.contains(p) => valid :+ newPosition
          case _ => valid
        }
      }
      .map { newPosition => MovePieceAction(this, position, newPosition) })
      .toSet
  }
}

case class Pawn(player: Player) extends Piece {
  override def moves(position: (Int, Int), state: ChessState): Set[MovePieceAction] = {
    val (file, rank) = position
    val nextRank = rank + (if (player == White) 1 else -1)
    val startingRank = if (player == White) 2 else 7
    val doubleAdvanceRank = if (player == White) 4 else 5
    val attackingMoves = Set(-1, 1)
      .map { file + _ }
      .filter { newFile => state.board.get((newFile, nextRank)).exists(_.player != player) }
      .map { newFile => MovePieceAction(this, (file, rank), (newFile, nextRank))}
    val forwardMove = if (!state.board.contains((file, nextRank))) {
      Some(MovePieceAction(this, (file, rank), (file, nextRank)))
    } else {
      None
    }
    val doubleAdvanceMove = if (rank == startingRank &&
      !state.board.contains((file, nextRank)) &&
      !state.board.contains((file, doubleAdvanceRank))) {
      Some(MovePieceAction(this, (file, rank), (file, doubleAdvanceRank)))
    } else {
      None
    }
    // TODO en passant
    // TODO promotion
    attackingMoves ++ forwardMove ++ doubleAdvanceMove
  }
}

case class King(player: Player) extends Piece {
  override def moves(position: (Int, Int), state: ChessState): Set[MovePieceAction] = {
    val (file, rank) = position
    (for {
      deltaFile <- -1 to 1
      deltaRank <- -1 to 1
      newPosition = (file + deltaFile, rank + deltaRank)
      if newPosition != (file, rank) && state.board.get(newPosition).forall(_.player != player)
    } yield MovePieceAction(this, (file, rank), newPosition)).toSet
  }
}

case class Queen(player: Player) extends Piece with Sliding {
  override def moves(position: (Int, Int), state: ChessState): Set[MovePieceAction] =
    slidingMoves(Diagonal, position, state) ++ slidingMoves(Orthogonal, position, state)
}

case class Bishop(player: Player) extends Piece with Sliding {
  override def moves(position: (Int, Int), state: ChessState): Set[MovePieceAction] =
    slidingMoves(Diagonal, position, state)
}

case class Knight(player: Player) extends Piece {
  override def moves(position: (Int, Int), state: ChessState): Set[MovePieceAction] = {
    val (file, rank) = position
    (for {
      (deltaFile, deltaRank) <- Seq((2, 1), (1, 2))
      fileFlip <- Seq(-1, 1)
      rankFlip <- Seq(-1, 1)
      newFile = file + deltaFile * fileFlip
      newRank = rank + deltaRank * rankFlip
    } yield MovePieceAction(this, (file, rank), (newFile, newRank))).toSet
  }
}

case class Rook(player: Player) extends Piece with Sliding {
  override def moves(position: (Int, Int), state: ChessState): Set[MovePieceAction] =
    slidingMoves(Orthogonal, position, state)
}
