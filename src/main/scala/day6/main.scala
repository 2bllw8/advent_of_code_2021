package exe.bbllw8.aoc.day6

import scala.io.Source

private def parseInput(path: String): List[Int] =
  Source.fromFile(path)
      .getLines()
      .flatMap(line => line.split(",").map(num => num.toInt))
      .toList


private def schoolListToMap(school: List[Int]): Map[Int, Long] =
  List(0, 1, 2, 3, 4, 5, 6, 7, 8)
      .map(key => key -> school.count(x => x == key).toLong)
      .toMap

private def evolve(school: Map[Int, Long],
                   target: Int,
                   day: Int = 0): Long =
  if (day == target) {
    println(school)
    school.values.sum
  } else {
    var newSchool = Map[Int, Long]()
    var toReintroduce = school.get(0).getOrElse(0L)
    school.foreachEntry { case (k, v) =>
      newSchool = k match {
        case 0 => newSchool + (8 -> toReintroduce)
        case 7 => newSchool + (6 -> (v + toReintroduce))
        case n => newSchool + (n - 1 -> v)
      }
    }

    evolve(newSchool, target, day + 1)
  }

@main
def part1(args: String*): Unit =
  val a = evolve(schoolListToMap(parseInput(args(0))), 80)
  println(s"There are $a lanternfish after 80 days")
  val b = evolve(schoolListToMap(parseInput(args(0))), 256)
  println(s"There are $b lanternfish after 256 days")
