package org.justinhj

import scala.io.Source
import scala.util.Try

object AOCUtil {
  def inputToStrings(name: String): List[String] = {
    Source.fromResource(name).getLines().toList
  }
}

object Adventofcode2021day5 extends App {

  import AOCUtil.inputToStrings

  case class Vec2(x: Int, y: Int)
  case class Line(p1: Vec2, p2: Vec2)

  def parseInput(input: List[String]): List[Line] = {
    input.map {
      line =>
        val pattern = """(\d+),(\d+) -> (\d+).(\d+)""".r

        val parsed = Try {
          val pattern(x1,y1,x2,y2) = line
          Line(Vec2(x1.toInt,y1.toInt),Vec2(x2.toInt,y2.toInt))
        }
        parsed.get
    }
  }

  def createLine(line: Line, allowDiagonal: Boolean): List[Vec2] = {
    // vertical
    val points = List.newBuilder[Vec2]
    if(line.p1.x == line.p2.x) {
      val range = if(line.p1.y > line.p2.y) {
        line.p2.y to line.p1.y
      } else {
        line.p1.y to line.p2.y
      }
      range.foreach {
        y =>
          points += Vec2(line.p1.x, y)
      }
    } // horizontal
    else if(line.p1.y == line.p2.y) {
      val range = if(line.p1.x > line.p2.x) {
        line.p2.x to line.p1.x
      } else {
        line.p1.x to line.p2.x
      }
      range.foreach {
        x =>
          points += Vec2(x, line.p1.y)
      }
    } else if(allowDiagonal) {
      // must be a diagonal
      var yInc = 0
      var y = 0
      val range = if(line.p1.x > line.p2.x) {
        yInc = if(line.p2.y > line.p1.y) -1 else 1
        y = line.p2.y
        line.p2.x to line.p1.x
      } else {
        yInc = if(line.p2.y < line.p1.y) -1 else 1
        y = line.p1.y
        line.p1.x to line.p2.x
      }

      range.foreach {
        x =>
          points += Vec2(x, y)
          y += yInc
      }
    }
    points.result()
  }

  def solve(lines: List[Line], allowDiagonal: Boolean): Int = {
    val allPoints = lines
                      .flatMap(p => createLine(p,allowDiagonal))
                      .groupMapReduce(identity)(_ => 1)(_ + _)

    allPoints.values.count(_ > 1)
  }

  val exampleInput = inputToStrings("example.txt")
  val exampleLines = parseInput(exampleInput)
  val exampleSolution = solve(exampleLines,allowDiagonal = false)
  println(exampleSolution)
  val exampleSolutionPart2 = solve(exampleLines,allowDiagonal = true)
  println(exampleSolutionPart2)

  val part1Input = inputToStrings("day5.txt")
  val part1Lines = parseInput(part1Input)
  val part1Solution = solve(part1Lines, allowDiagonal = false)
  println(part1Solution)

  val part2Solution = solve(part1Lines, allowDiagonal = true)
  println(part2Solution)
}