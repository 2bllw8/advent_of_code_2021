package exe.bbllw8.aoc.day3

import scala.io.Source

private def parseInput(path: String): Array[Array[Byte]] =
  Source.fromFile(path)
      .getLines()
      .map(x => x.split("")
          .map(x => x.toByte)
          .toArray)
      .toArray

private def countBits(diagnostic: Array[Array[Byte]],
                      column: Int,
                      row: Int = 0,
                      numZeroes: Int = 0,
                      numOnes: Int = 0): Option[Tuple2[Int, Int]] =
  if (diagnostic.size <= row) {
    if (numZeroes == 0 && numOnes == 0) {
      None
    } else {
      Some(numZeroes -> numOnes)
    }
  } else if (diagnostic(row).size > column) {
    diagnostic(row)(column) match {
      case 0 => countBits(diagnostic,
        column,
        row + 1,
        numZeroes + 1,
        numOnes)
      case 1 => countBits(diagnostic,
        column,
        row + 1,
        numZeroes,
        numOnes + 1)
      case _ => ???
    }
  } else {
    None
  }

private def getGammaRate(diagnostic: Array[Array[Byte]],
                         column: Int = 0,
                         gammaRate: Int = 0): Int =
  countBits(diagnostic, column) match {
    case Some(numZeroes, numOnes) =>
      getGammaRate(diagnostic,
        column + 1,
        (gammaRate << 1) | (if (numZeroes > numOnes) 0 else 1))
    case None =>
      gammaRate
  }

private def getEpsilonRate(diagnostic: Array[Array[Byte]],
                           column: Int = 0,
                           epsilonRate: Int = 0): Int =
  countBits(diagnostic, column) match {
    case Some(numZeroes, numOnes) =>
      getEpsilonRate(diagnostic,
        column + 1,
        (epsilonRate << 1) | (if (numZeroes > numOnes) 1 else 0))
    case None =>
      epsilonRate
  }

private def getOxygenGeneratorRating(diagnostic: Array[Array[Byte]],
                                     i: Int = 0): Int =
  if (diagnostic.size == 1) {
    diagnostic(0).foldLeft(0)((acc: Int, x: Byte) => (acc << 1) | x)
  } else {
    countBits(diagnostic, i) match {
      case Some(numZeroes, numOnes) => {
        val target = if (numZeroes > numOnes) 0 else 1
        getOxygenGeneratorRating(diagnostic.filter(row => row(i) == target).toArray,
          i + 1)
      }
      case None => ???
    }
  }

private def getCo2ScrubberRating(diagnostic: Array[Array[Byte]],
                                 i: Int = 0): Int =
  if (diagnostic.size == 1) {
    diagnostic(0).foldLeft(0)((acc: Int, x: Byte) => (acc << 1) | x)
  } else {
    countBits(diagnostic, i) match {
      case Some(numZeroes, numOnes) => {
        val target = if (numZeroes > numOnes) 1 else 0
        getCo2ScrubberRating(diagnostic.filter(row => row(i) == target).toArray,
          i + 1)
      }
      case None => ???
    }
  }


@main
def part1(args: String*): Unit =
  val diagnostic = parseInput(args(0))

  val gammaRate = getGammaRate(diagnostic)
  val epsilonRate = getEpsilonRate(diagnostic)
  println(s"Gamma: ${gammaRate}\nEpsilon: ${epsilonRate}\nPower: ${gammaRate * epsilonRate}")

  val o2GeneratorRating = getOxygenGeneratorRating(diagnostic)
  val co2ScrubberRating = getCo2ScrubberRating(diagnostic)
  println(s"O2 generator: ${o2GeneratorRating}\nCO2 scrubber: ${co2ScrubberRating}\n" +
      s"Life support rating: ${o2GeneratorRating * co2ScrubberRating}")

