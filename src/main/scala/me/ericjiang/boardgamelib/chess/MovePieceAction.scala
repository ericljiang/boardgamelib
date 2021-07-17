package me.ericjiang.boardgamelib.chess

import me.ericjiang.boardgamelib.chess.ChessFiles._
import me.ericjiang.boardgamelib.{Action, ActionResult}

case class MovePieceAction(piece: Piece, origin: (Int, Int), destination: (Int, Int)) extends Action[ChessState] {
  override def execute(state: ChessState): ActionResult[ChessState] = {
    require(state.activePlayer == piece.player)
    require(state.board(origin) == piece)

    val (file, rank) = destination
    require(a <= file && file <= h)
    require(1 <= rank && rank <= 8)

    val capturedPiece = state.board.get(destination)
    if (capturedPiece.isDefined) require(capturedPiece.get.player != state.activePlayer)

    val winner = capturedPiece.flatMap {
      case King(_) => Some(state.activePlayer)
      case _ => None
    }

    val newState = ChessState(
      activePlayer = state.activePlayer match { case White => Black; case Black => White },
      board = state.board - origin + (destination -> piece),
      winner = winner)

    ActionResult(
      state = newState,
      events = Seq.empty,
      availableActions = newState.availableActions)
  }

  override def toString: String = {
    val pieceName = piece match {
      case Pawn(_) => "P"
      case King(_) => "K"
      case Queen(_) => "Q"
      case Bishop(_) => "B"
      case Knight(_) => "N"
      case Rook(_) => "R"
    }
    val (originFile, originRank) = origin
    val (destinationFile, destinationRank) = destination
    def numberToFile: Int => String = {
      case 1 => "a"
      case 2 => "b"
      case 3 => "c"
      case 4 => "d"
      case 5 => "e"
      case 6 => "f"
      case 7 => "g"
      case 8 => "h"
    }
    s"$pieceName${numberToFile(originFile)}$originRank${numberToFile(destinationFile)}$destinationRank"
  }
}

object MovePieceAction {
  def parse(notation: String, player: Player): MovePieceAction = {
    val pattern = "(?i)([PKQBNR])([a-h])([1-8])([a-h])([1-8])".r
    val pattern(piece, originFile, originRank, destinationFile, destinationRank) = notation
    MovePieceAction(
      piece = piece.toUpperCase match {
        case "P" => Pawn(player)
        case "K" => King(player)
        case "Q" => Queen(player)
        case "B" => Bishop(player)
        case "N" => Knight(player)
        case "R" => Rook(player)
      },
      origin = (originFile.toCharArray.head - 'a' + 1, originRank.toInt),
      destination = (destinationFile.toCharArray.head - 'a' + 1, destinationRank.toInt))
  }
}
