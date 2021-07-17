package me.ericjiang.boardgamelib.chess

import me.ericjiang.boardgamelib.chess.ChessFiles._
import me.ericjiang.boardgamelib.{Action, Game, State}

import scala.collection.mutable
import scala.io.AnsiColor._

object ChessGame extends Game[ChessState] {
  override def initialState: ChessState = ChessState(
    activePlayer = White,
    board = Seq(White, Black).flatMap(generatePieces).toMap,
    winner = None)

  private def generatePieces(player: Player): IndexedSeq[((Int, Int), Piece)] = {
    val (backRank, pawnRank) = player match {
      case White => (1, 2)
      case Black => (8, 7)
    }
    (for (file <- a to h) yield (file, pawnRank) -> Pawn(player)) :+
      (a, backRank) -> Rook(player) :+
      (b, backRank) -> Knight(player) :+
      (c, backRank) -> Bishop(player) :+
      (d, backRank) -> Queen(player) :+
      (e, backRank) -> King(player) :+
      (f, backRank) -> Bishop(player) :+
      (g, backRank) -> Knight(player) :+
      (h, backRank) -> Rook(player)
  }
}

case class ChessState(
  activePlayer: Player,
  board: Map[(Int, Int), Piece],
  winner: Option[Player])
  extends State[ChessState] {
  override def availableActions: Seq[Action[ChessState]] = {
    if (!ChessState.moveCache.contains(this)) {
      ChessState.moveCache(this) = if (winner.isDefined) {
        Seq.empty
      } else {
        board
          .filter { case (_, piece) => piece.player == activePlayer }
          .flatMap { case (position, piece) => piece.moves(position, this) }
          // in bounds
          .filter { case MovePieceAction(_, _, (file, rank)) =>
            a <= file && file <= h &&
              1 <= rank && rank <= 8
          }
          // moves into empty space or opponent piece
          .filter { case MovePieceAction(_, _, destination) =>
            board.get(destination).forall(_.player != activePlayer)
          }
          // TODO does not result in check for moving player
          .toSeq
      }
    }
    ChessState.moveCache(this)
  }

  override def toString: String = {
    val renderedBoard = (1 to 8).reverse
      .map(rank => {
        (a to h)
//          .map(file => board.get((file, rank)))
          .map { file =>
            val char = board.get((file, rank)) match {
              case Some(piece) => piece match {
                case Pawn(player) => player match {
                  case White => "♟"
                  case Black => "♙"
                }
                case King(player) => player match {
                  case White => "♚"
                  case Black => "♔"
                }
                case Queen(player) => player match {
                  case White => "♛"
                  case Black => "♕"
                }
                case Bishop(player) => player match {
                  case White => "♝"
                  case Black => "♗"
                }
                case Knight(player) => player match {
                  case White => "♞"
                  case Black => "♘"
                }
                case Rook(player) => player match {
                  case White => "♜"
                  case Black => "♖"
                }
              }
              case None => " "
            }
            if ((file + rank) % 2 == 0) s" $char " else s"$BLACK_B $char $RESET"
          }
          .mkString
      })
      .mkString("\n")
    val player = activePlayer match {
      case White => s"$REVERSED$activePlayer$RESET"
      case Black => activePlayer.toString
    }
    s"$renderedBoard$RESET\n$player to move."
  }
}

object ChessState {
  private val moveCache: mutable.Map[ChessState, Seq[Action[ChessState]]] = mutable.Map.empty
}

sealed trait Player
case object White extends Player
case object Black extends Player

object ChessFiles {
  val a = 1
  val b = 2
  val c = 3
  val d = 4
  val e = 5
  val f = 6
  val g = 7
  val h = 8
}
