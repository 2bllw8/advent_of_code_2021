package exe.bbllw8.aoc.day5

import scala.io.Source

case class Point(val x: Int, val y: Int)

case class Line(val start: Point, val end: Point)

private def parseInput(path: String): List[Line] =
  Source.fromFile(path)
      .getLines()
      .map(line =>
        line.split(" -> ")
            .map(str => str.split(",")
                .map(num => num.toInt))
            .map(nums => Point(nums(0), nums(1))))
      .map(points => Line(points(0), points(1)))
      .toList


def drawMatrix(matrix: Array[Array[Int]],
               lines: List[Line]): Unit =
  lines.foreach { line =>
    var x = line.start.x
    var y = line.start.y

    val endX = line.end.x
    val endY = line.end.y

    while (x != endX || y != endY) {
      matrix(y)(x) += 1
      if (x != endX) {
        x += (if (x > endX) -1 else 1)
      }
      if (y != endY) {
        y += (if (y > endY) -1 else 1)
      }
    }
    matrix(y)(x) += 1
  }

@main
def part1(args: String*): Unit =
  val lines = parseInput(args(0))
  val maxX = lines.map(line => if (line.start.x > line.end.x) line.start.x else line.end.x).max
  val maxY = lines.map(line => if (line.start.y > line.end.y) line.start.y else line.end.y).max

  val matrix = Array.fill(maxY + 1) { Array.fill(maxX + 1) { 0 } }

  drawMatrix(matrix, lines)
  println(matrix.map(row => row.map(x => String.format("%1d", x))
      .mkString
      .replace("0", ".")).mkString("\n"))

  val c = matrix.flatten.count(x => x > 1)
  println(s"There are $c elements with value > 1")



