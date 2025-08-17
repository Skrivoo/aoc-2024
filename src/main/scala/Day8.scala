import scala.annotation.tailrec
import scala.math._

object Day8 {

  private val exampleInput =
    """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............""".stripMargin
  private val myInput = 
    """...............................6.B..........P.....
n..............M..................................
....n.....sM7.............................6.....p.
......................Mr......................P...
.......n.......................................p.E
.......................6...................p......
r..............................C........B....P....
.....................d........6......B4....P......
.........................7....................4...
.n......................R..............4..........
.....N........S.................K.C..........4....
...........N..r.....................B....K........
..................................................
......N.......x.............7.......K.....2....E..
...................r..H........R..................
.....................s....p........C...........2..
....3.......................M.....................
........k....................H....5...............
.....x....N................d.5..y................J
m.....................d7...................J......
.......exk........................................
.......x.............5.......R....................
..........eY......................................
...S.3..............................O.E...J.......
.......8...H....k...............J.................
......S.e.........C.H.....................X.....y.
................j..........y.........2............
...........e.........k............................
......YS...3..............5..........K...XR.......
...m..............j.s..........c..................
.........................j........................
...............j..................................
.....m................................2...........
.........Y......................................b.
..................................................
.......................h...........b..............
............m......D..............d...............
........o......D..................................
...................................O..............
..................................................
......8...........................................
........D.Y..o...................1................
.....................................b..9.........
........................h..0......................
.....o......................h..0........b1........
.........8.............X..........................
..........o..........c..........1...........O.....
....8....................y0...c...................
..............D.......c..................9..0.....
............................1..........O..9.......""".stripMargin

  private sealed trait Block
  private case class EmptyBlock(x: Int, y: Int) extends Block
  private case class NonEmptyBlock(frequency: Option[Char], antinode: Boolean, x: Int, y: Int) extends Block

  private object Block {
    def apply(c: Char, x: Int, y: Int): Block = c match {
      case '.' => EmptyBlock(x, y)
      case _   => NonEmptyBlock(frequency = Some(c), antinode = false, x, y)
    }
  }

  private val input: List[List[Block]] =
  myInput
    .split("\n")
    .zipWithIndex
    .map { case (line, y) =>
      line.zipWithIndex.map { case (c, x) => Block(c, x, y) }.toList
    }
    .toList

  private def start(input: List[List[Block]], metFrequencies: List[Char]): List[List[Block]] = {
    def createAntinodesInMap(input: List[List[Block]], currentFrequencyToProcess: List[NonEmptyBlock]): List[List[Block]] = {
      def createAntinodePair(one: NonEmptyBlock, another: NonEmptyBlock): List[NonEmptyBlock] = {
        val xDifference = abs(one.x - another.x)
        val yDifference = abs(one.y - another.y)
      
        val first = if (one.x < another.x && one.y < another.y) {
          List(NonEmptyBlock(getFrequencyFromCoordante(one.x - xDifference, one.y - yDifference), true, one.x - xDifference, one.y - yDifference))
        } else if (one.x < another.x && one.y > another.y) {
          List(NonEmptyBlock(getFrequencyFromCoordante(one.x - xDifference, one.y + yDifference), true, one.x - xDifference, one.y + yDifference))
        } else if (one.x > another.x && one.y < another.y) {
          List(NonEmptyBlock(getFrequencyFromCoordante(one.x + xDifference, one.y - yDifference), true, one.x + xDifference, one.y - yDifference))
        } else {
          List(NonEmptyBlock(getFrequencyFromCoordante(one.x + xDifference, one.y + yDifference), true, one.x + xDifference, one.y + yDifference))
        }

        val second = if (another.x < one.x && another.y < one.y) {
          List(NonEmptyBlock(getFrequencyFromCoordante(another.x - xDifference, another.y - yDifference), true, another.x - xDifference, another.y - yDifference))
        } else if (another.x < one.x && another.y > one.y) {
          List(NonEmptyBlock(getFrequencyFromCoordante(another.x - xDifference, another.y + yDifference), true, another.x - xDifference, another.y + yDifference))
        } else if (another.x > one.x && another.y < one.y) {
          List(NonEmptyBlock(getFrequencyFromCoordante(another.x + xDifference, another.y - yDifference), true, another.x + xDifference, another.y - yDifference))
        } else {
          List(NonEmptyBlock(getFrequencyFromCoordante(another.x + xDifference, another.y + yDifference), true, another.x + xDifference, another.y + yDifference))
        }
        first ++ second
      }
      val newAntinodes: List[NonEmptyBlock] = {
        (for {
          one <- currentFrequencyToProcess
          another <- currentFrequencyToProcess
          if one != another
        } yield (
          createAntinodePair(one, another)
        )).flatten
      }
      input.zipWithIndex.map { case (row, y) =>
        row.zipWithIndex.map { case (block, x) =>
          newAntinodes.find(nb => nb.x == x && nb.y == y) match {
            case Some(nb) => nb
            case None => block
          }
        }.toList
      }.toList
    }
    def getNextFrequency(input: List[List[Block]]): Option[Char] = {
      input.flatten.collectFirst {
        case NonEmptyBlock(Some(frequency), _, _, _) if !metFrequencies.contains(frequency) => frequency
      }
    }
    def collectAllOfAFrequency(input: List[List[Block]], frequency: Char): List[NonEmptyBlock] = {
      input.flatten.collect {
        case NonEmptyBlock(Some(f), antinode, x, y) if f == frequency => NonEmptyBlock(Some(f), antinode, x, y)
      }
    }
    def getFrequencyFromCoordante(x: Int, y: Int): Option[Char] = {
      input.lift(y).flatMap(_.lift(x)).collect {
        case NonEmptyBlock(Some(frequency), _, _, _) => frequency
      }
    }
    val allFrequencies: List[Char] = {
      input.flatten.collect {
        case NonEmptyBlock(Some(frequency), _, _, _) => frequency
      }.distinct
    }
    if (allFrequencies == metFrequencies) {
      input
    } else {
      getNextFrequency(input) match
        case Some(frequency) => {
          val currentFrequencyToProcess = collectAllOfAFrequency(input, frequency)
          start(createAntinodesInMap(input, currentFrequencyToProcess), metFrequencies :+ frequency)
        } 
        case None => input
    }
  }


  private def countAntinodes(input: List[List[Block]]): Int = {
    input.flatten.count {
      case NonEmptyBlock(_, antinode, _, _) => antinode
      case _ => false
    }
  }

  def day8task1: Int = countAntinodes(start(input, List.empty))
  
}
