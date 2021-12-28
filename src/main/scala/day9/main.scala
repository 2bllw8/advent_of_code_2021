package exe.bbllw8.aoc.day9

import scala.io.Source

private def parseArg(path: String): List[List[Int]] =
  Source.fromFile(path)
      .getLines()
      .map(_.split("").map(_.toInt).toList)
      .toList

private def getMatrixItem[T](matrix: List[List[T]], x: Int, y: Int): Option[T] =
  if (y >= 0 && x >= 0 && matrix.size > y && matrix(y).size > x)
    Some(matrix(y)(x))
  else
    None

private def getRiskSum(matrix: List[List[Int]],
                       x: Int = 0,
                       y: Int = 0,
                       lowPoints: List[Int] = List()): Int =
  if (matrix.size > y && matrix(y).size > x) {
    val neighbours = List(
      getMatrixItem(matrix, x - 1, y),
      getMatrixItem(matrix, x + 1, y),
      getMatrixItem(matrix, x, y - 1),
      getMatrixItem(matrix, x, y + 1),
    ).flatten
    val (nextX, nextY) = if (matrix(y).size - 1 == x) (0, y + 1) else (x + 1, y)
    val item = matrix(y)(x)
    if (neighbours.forall(n => item < n)) {
      println(s"Low point at [$x, $y] = ${matrix(y)(x)} < [${neighbours.mkString(", ")}]")
      getRiskSum(matrix, nextX, nextY, lowPoints :+ item)
    } else {
      getRiskSum(matrix, nextX, nextY, lowPoints)
    }
  } else {
    lowPoints.map(_ + 1).sum
  }


private def getBasin(matrix: List[List[Int]],
                     x: Int,
                     y: Int,
                     lowPoint: Option[Int] = None,
                     points: List[Int] = List()): List[Int] =
  getMatrixItem(matrix, x, y).map { item =>
    val low = lowPoint.getOrElse(item)
    val neighbours = List(
      (x - 1, y),
      (x + 1, y),
      (x, y - 1),
      (x, y + 1),
    ).filter((pX, pY) => getMatrixItem(matrix, pX, pY).map(_ < low).getOrElse(false))
        .toList
    if (neighbours.size == 0)
      points :+ low
    else
      neighbours.map((pX, pY) => getBasin(matrix, pX, pY, Some(low), points :+ item))
          .flatten
          .toSeq
  }.getOrElse(List())

private def getBasinSizes(matrix: List[List[Int]],
                      x: Int = 0,
                      y: Int = 0,
                      basinSizes: List[Int] = List()): List[Int] =
  if (matrix.size > y && matrix(y).size > x) {
    val (nextX, nextY) = if (matrix(y).size - 1 == x) (0, y + 1) else (x + 1, y)
    val basin = getBasin(matrix, x, y)
    if (basin.size > 0) {
      println(s"Basin [$x, $y]: [${basin.mkString(", ")}]")
    }
    getBasinSizes(matrix, nextX, nextY, basinSizes :+ basin.size)
  } else {
    basinSizes
  }


@main
def part1(args: String*): Unit =
  val matrix = parseArg(args(0))
  val risk = getRiskSum(matrix)
  println(s"Risk level sums is $risk")
  val basinSizes = getBasinSizes(matrix)
      .sorted
      .slice(0, 3)
      .foldLeft(1)((acc, x) => x * acc)
  println(s"Top 3 Basin sizes product: $basinSizes")