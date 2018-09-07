package lila.game

import chess._
import org.specs2.mutable._
import lila.db.ByteArray
import chess.variant.{ Capablanca, Standard, Variant }

class BinaryPieceTest extends Specification {

  val noop = "00000000"
  def write(all: PieceMap): List[String] =
    write(Standard, all)
  def write(variant: Variant, all: PieceMap): List[String] =
    (BinaryFormat.piece.write(variant)(all)).showBytes.split(',').toList
  def read(bytes: List[String]): PieceMap =
    read(Standard, bytes)
  def read(variant: Variant, bytes: List[String]): PieceMap =
    BinaryFormat.piece.read(ByteArray.parseBytes(bytes), variant)

  "binary pieces" should {
    import chess.StdBoard._
    "write" should {
      "empty board" in {
        write(Map.empty) must_== List.fill(32)(noop)
      }
      "A1 white king" in {
        write(Map(A1 -> White.king)) must_== {
          "00010000" :: List.fill(31)(noop)
        }
      }
      "A1 black knight" in {
        write(Map(A1 -> Black.knight)) must_== {
          "11000000" :: List.fill(31)(noop)
        }
      }
      "B1 black pawn" in {
        write(Map(B1 -> Black.pawn)) must_== {
          "00001110" :: List.fill(31)(noop)
        }
      }
      "A1 black knight, B1 white bishop" in {
        write(Map(A1 -> Black.knight, B1 -> White.bishop)) must_== {
          "11000101" :: List.fill(31)(noop)
        }
      }
      "A1 black knight, B1 white bishop, C1 white queen" in {
        write(Map(A1 -> Black.knight, B1 -> White.bishop, C1 -> White.queen)) must_== {
          "11000101" :: "00100000" :: List.fill(30)(noop)
        }
      }
      "H8 black knight" in {
        write(Map(H8 -> Black.knight)) must_== {
          List.fill(31)(noop) :+ "00001100"
        }
      }
      "G8 black knight, H8 white bishop" in {
        write(Map(G8 -> Black.knight, H8 -> White.bishop)) must_== {
          List.fill(31)(noop) :+ "11000101"
        }
      }
    }
    "read" should {
      "empty board" in {
        read(List.fill(32)(noop)) must_== Map.empty
        "A1 white king" in {
          read("00010000" :: List.fill(31)(noop)) must_== Map(A1 -> White.king)
        }
        "B1 black pawn" in {
          read("00001110" :: List.fill(31)(noop)) must_== Map(B1 -> Black.pawn)
        }
      }
    }
  }
  "Capablanca variant binary pieces" should {
    import chess.CapaBoard._
    "write" should {
      "empty board" in {
        write(Capablanca, Map.empty) must_== List.fill(80)(noop)
      }
      "A1 white king" in {
        write(Capablanca, Map(A1 -> White.king)) must_== {
          "00000001" :: List.fill(79)(noop)
        }
      }
      "A1 black knight" in {
        write(Capablanca, Map(A1 -> Black.knight)) must_== {
          "00010100" :: List.fill(79)(noop)
        }
      }
      "J8 black cancellor" in {
        write(Capablanca, Map(J8 -> Black.cancellor)) must_== {
          List.fill(79)(noop) :+ "00011001"
        }
      }
      "A1 white archbishop, B1 white bishop" in {
        write(Capablanca, Map(A1 -> White.archbishop, B1 -> White.bishop)) must_== {
          "00001000" :: "00000101" :: List.fill(78)(noop)
        }
      }
    }
    "read" should {
      "empty board" in {
        read(Capablanca, List.fill(80)(noop)) must_== Map.empty
        "A1 white archbishop" in {
          read(Capablanca, "00001000" :: List.fill(79)(noop)) must_== Map(A1 -> White.archbishop)
        }
        "A1 black cancellor" in {
          read(Capablanca, "00011001" :: List.fill(79)(noop)) must_== Map(A1 -> Black.cancellor)
        }
      }
    }
  }
}
