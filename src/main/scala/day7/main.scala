package exe.bbllw8.aoc.day7

import scala.io.Source

private def parseInput(path: String): List[Int] =
  Source.fromFile(path)
      .getLines()
      .flatMap(line => line.split(",").map(num => num.toInt))
      .toList

private def computeFuelConstant(list: List[Int],
                                target: Int,
                                i: Int = 0,
                                fuel: Int = 0): Int =
  if (i >= list.size) {
    fuel
  } else {
    computeFuelConstant(list, target, i + 1, fuel + Math.abs(list(i) - target))
  }

private def computeFuelIncreasing(list: List[Int],
                                  target: Int,
                                  i: Int = 0,
                                  fuel: Int = 0): Int =
  if (i >= list.size) {
    fuel
  } else {
    val n = Math.abs(list(i) - target)
    val cost = n * (n + 1) / 2
    computeFuelIncreasing(list, target, i + 1, fuel + cost)
  }

@main
def part1(args: String*): Unit =
  val positions = parseInput(args(0))
  val min = (0 to positions.max)
      .map(x => x -> computeFuelConstant(positions, x))
      .minBy(x => x._2)
  println(s"Minimum fuel strategy: align to ${min._1} (cost = ${min._2})")

@main
def part2(args: String*): Unit =
  val positions = parseInput(args(0))
  val min = (0 to positions.max)
      .map(x => x -> computeFuelIncreasing(positions, x))
      .minBy(x => x._2)
  println(s"Minimum fuel strategy: align to ${min._1} (cost = ${min._2})")
