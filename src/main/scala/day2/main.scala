package exe.bbllw8.aoc.day2

import scala.io.Source

private sealed trait Movement:
  val quantity: Int
  val horizontal: Boolean

case class Forward(quantity: Int) extends Movement :
  override val horizontal = true

case class Down(quantity: Int) extends Movement :
  override val horizontal = false

case class Up(quantity: Int) extends Movement :
  override val horizontal = false

private def parseInput(path: String): List[Movement] =
  val forwardMatch = "forward (\\d+)".r
  val downMatch = "down (\\d+)".r
  val upMatch = "up (\\d+)".r
  Source.fromFile(path)
      .getLines()
      .map(x => x match {
        case forwardMatch(x) => Some(Forward(x.toInt))
        case downMatch(x) => Some(Down(x.toInt))
        case upMatch(x) => Some(Up(x.toInt))
        case _ => None
      })
      .collect { case Some(x) => x }
      .toList

private def computePositionWithoutAim(movements: List[Movement],
                                      index: Int = 0,
                                      horizontal: Int = 0,
                                      vertical: Int = 0): Tuple2[Int, Int] =
  if (movements.size <= index) {
    horizontal -> vertical
  } else {
    movements(index) match {
      case Forward(x) => computePositionWithoutAim(movements,
        index + 1,
        horizontal + x,
        vertical)
      case Down(x) =>
        computePositionWithoutAim(movements,
          index + 1,
          horizontal,
          vertical + x)
      case Up(x) => computePositionWithoutAim(movements,
        index + 1,
        horizontal,
        vertical - x)
    }
  }

private def computePositionWithAim(movements: List[Movement],
                                   index: Int = 0,
                                   horizontal: Int = 0,
                                   depth: Int = 0,
                                   aim: Int = 0): Tuple2[Int, Int] =
  if (movements.size <= index) {
    horizontal -> depth
  } else {
    movements(index) match {
      case Forward(x) => computePositionWithAim(movements,
        index + 1,
        horizontal + x,
        depth + aim * x,
        aim)
      case Down(x) =>
        computePositionWithAim(movements,
          index + 1,
          horizontal,
          depth,
          aim + x)
      case Up(x) => computePositionWithAim(movements,
        index + 1,
        horizontal,
        depth,
        aim - x)
    }
  }

@main
def part1(args: String*): Unit =
  val (x, y) = computePositionWithoutAim(parseInput(args(0)))
  println(s"Horizontal: ${x}\nVertical: ${y}\nMultiplied: ${x * y}")

@main
def part2(args: String*): Unit =
  val (x, y) = computePositionWithAim(parseInput(args(0)))
  println(s"Horizontal: ${x}\nVertical: ${y}\nMultiplied: ${x * y}")


