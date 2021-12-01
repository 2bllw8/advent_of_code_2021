package exe.bbllw8.aoc.day1

import scala.io.Source

private def countIncreasing(list: List[Int],
                            index: Int,
                            window: Int,
                            count: Int = 0): Int =
  if (list.size <= index) {
    count
  } else {
    countIncreasing(list,
      index + 1,
      window,
      if (list.slice(index - window, index).iterator.sum > list.slice(index - window - 1, index - 1).iterator.sum) {
        count + 1
      } else {
        count
      }
    )
  }

private def parseInput(path: String): List[Int] =
  Source.fromFile(path)
      .getLines()
      .map(x => x.toInt)
      .toList

@main
def part1(args: String*): Unit =
  println(countIncreasing(parseInput(args(0)), 1, 1))

@main
def part2(args: String*): Unit =
  println(countIncreasing(parseInput(args(0)), 3, 3))
