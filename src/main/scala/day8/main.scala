package exe.bbllw8.aoc.day8

import scala.io.Source

private object DISPLAY:
  val segments = Map(
    0 -> "abcefg",
    1 -> "fg",
    2 -> "acdeg",
    3 -> "acdfg",
    4 -> "bcdf",
    5 -> "abdfg",
    6 -> "abdefg",
    7 -> "acf",
    8 -> "abcdefg",
    9 -> "abcdfg",
  )

private case class Input(val patterns: Array[Array[Char]],
                         val numberDigits: Array[Array[Char]]):
  override def toString: String = s"patterns = [${patterns.mkString(", ")}], out = [${numberDigits.mkString(", ")}]"

private def parseInput(path: String): List[Input] =
  Source.fromFile(path)
      .getLines()
      .map(_.split(" \\| "))
      .map(x => Input(x(0).split(" ").map(_.toCharArray),
        x(1).split(" ").map(_.toCharArray)))
      .toList

private def countEasyDigits(input: Input,
                            digits: List[Int] = List(1, 4, 7, 8)): Int =
  digits.map { digit =>
    val numSegments = DISPLAY.segments.get(digit)
        .map(_.size)
        .getOrElse(0)
    input.numberDigits.count(_.size == numSegments)
  }.sum

private def getMapping(input: Input): Map[Set[Char], Int] =
  val one = input.patterns
      .filter(_.length == 2)
      .toList(0)
      .toSet
  val four = input.patterns
      .filter(_.length == 4)
      .toList(0)
      .toSet
  val seven = input.patterns
      .filter(_.length == 3)
      .toList(0)
      .toSet
  val eight = input.patterns
      .filter(_.length == 7)
      .toList(0)
      .toSet
  val six = input.patterns
      .filter(_.length == 6)
      .filter(chars => one.diff(chars.toSet).size == 1)
      .toList(0)
      .toSet
  val zero = input.patterns
      .filter(_.length == 6)
      .filter(chars => four.diff(chars.toSet).size == 1 && chars.toSet != six)
      .toList(0)
      .toSet
  val nine = input.patterns
      .filter(_.length == 6)
      .filter(chars => chars.toSet != zero && chars.toSet != six)
      .toList(0)
      .toSet
  val five = input.patterns
      .filter(_.length == 5)
      .filter(_.forall(c => six.contains(c)))
      .toList(0)
      .toSet
  val three = input.patterns
      .filter(_.length == 5)
      .filter(chars => chars.forall(c => nine.contains(c)) && chars.toSet != five)
      .toList(0)
      .toSet
  val two = input.patterns
      .filter(_.length == 5)
      .filter(chars => chars.toSet != five && chars.toSet != three)
      .toList(0)
      .toSet

  Map(
    zero -> 0,
    one -> 1,
    two -> 2,
    three -> 3,
    four -> 4,
    five -> 5,
    six -> 6,
    seven -> 7,
    eight -> 8,
    nine -> 9,
  )

@main
def part1(args: String*): Unit =
  val countEasy = parseInput(args(0))
      .map(x => countEasyDigits(x))
      .sum
  println(s"There are $countEasy instances of [1, 4, 7, or 8]")

@main
def part2(args: String*): Unit =
  val sum = parseInput(args(0))
      .map(x => x -> getMapping(x))
      .map {
        case (input, mapping) => {
          input.numberDigits.map { digit =>
            mapping.get(digit.toSet).getOrElse(0)
          }.mkString.toInt
        }
      }
      .sum

  println(sum)