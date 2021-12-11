package exe.bbllw8.aoc.day4

import scala.io.Source

private case class Board(numbers: List[List[Int]]):
  private case class Item(val row: Int,
                          val column: Int,
                          val value: Int,
                          var marked: Boolean)

  private val data = numbers.zipWithIndex.flatMap { case (itemsOfTheRow, row) =>
    itemsOfTheRow.zipWithIndex.map((item, column) => Item(row, column, item, false))
  }
  private var hasWon: Boolean = false

  def onDrawNumber(drawn: Int): Option[Int] =
    data.find(item => !item.marked && item.value == drawn)
        .flatMap { selected =>
          selected.marked = true
          val numMarkedInRow = data.count(item => item.marked && item.row == selected.row)
          val numMarkedInColumn = data.count(item => item.marked && item.column == selected.column)
          if (hasWon) {
            None
          } else if (numMarkedInRow == 5 || numMarkedInColumn == 5) {
            hasWon = true
            Some(drawn * data.filter(item => !item.marked).map(item => item.value).sum)
          } else {
            None
          }
        }

private def parseInput(path: String): Tuple2[List[Int], List[Board]] =
  val lines = Source.fromFile(path).getLines().toList
  val numbers = lines(0).split(",").map(num => num.toInt).toList

  var boards = List[Board]()
  var i = 2
  while (i < lines.size) {
    val numbers = List(lines(i), lines(i + 1), lines(i + 2), lines(i + 3), lines(i + 4)).map {
      _.split(" ").filter(str => !str.isEmpty)
          .map(num => num.toInt)
          .toList
    }
    boards = boards ++ List(Board(numbers))
    i += 6
  }
  numbers -> boards

@main
def part1(args: String*): Unit =
  val (draws, boards) = parseInput(args(0))

  draws.zipWithIndex.foreach { (draw, drawNumber) =>
    val wins: List[Tuple2[Int, Int]] = boards
        .map(board => board.onDrawNumber(draw))
        .zipWithIndex
        .flatMap { (winOpt, i) =>
          winOpt match {
            case Some(score) => Some(score -> i)
            case None => None
          }
        }
    if (wins.size > 0) {
      val (score, i) = wins(0)
      println(s"[$drawNumber] -> $draw\tBoard ${i + 1} wins with score $score")
    } else {
      println(s"[$drawNumber] -> $draw\tNo wins")
    }
  }

