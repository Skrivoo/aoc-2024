import scala.annotation.tailrec

object Day6 {
  private val myInput =
    ".....#..#................#...#.....#.......................................................#.............................##.......\n......................#..............................................................#..............#..........................#..\n.........#........................#.....................................................#..............#...........#........##....\n..........#......................#.....#...#............#..........................#.....#........................................\n#....................................................................................................................#............\n.#....#......................#.......................#...............................#...#.....#...................#........#.....\n..#..#.......................##........#...............................................................#........#.........#.......\n..............................................................#...#.........#..#..................................................\n.............####..................................#................#..#.....................................#.....#..............\n..........#................#................................................#........................#.....#......................\n..............................................#.........#.....................#..................#.......................#........\n.........................................##..#.................................#.....#............................................\n............#..#...#...............................#.#.....#...............#...............................#..............#.#.#..#\n.................#..........#..#....#.....................#......................................##............#..................\n.......#....#.....................#......##...................#..............#.................#........#..#......#...............\n...............................................#.............#....................................................................\n..............#..............................................##..................#..........................#............#........\n................#.......................#..#............................................................#..........#..#.........#.\n.#.......................###............#.........#.................................................#.............................\n........#......................................................................................#.....#.......#...#................\n#.#.........................#..#.............................................#....#........................#......................\n.......#.....#...............................#.....#......................#.................................#.....#...#...........\n................................................................................................#............#....................\n.......#......#...............#...#............................#................................#................................#\n.#.....................................................................................................................#......#...\n...........#..........................................#........#...................#..............................................\n....#......................................................................................#.....##.#...................#.........\n...#..........................................................................##.......................................#.....#....\n#........#................#...............#.........#..#.#..............##..............#........#............#....#.....#........\n......#......................................#...............#....#..#...........................#................................\n#..............................................................................#............#...#..#....................#......#..\n.......#.........................................#.........#.......#....................................#.........................\n......#......#.............................................#.....................#...........#.............#..#...................\n#.......................#........#............................................##..............................#...................\n..........................................................................#........................................#....#......#..\n..........#..#...............................................#............................#.#..............................#......\n....#...........#..#...................................#................................................................#.........\n..............#..........#...............#....#.....................................#.......................#.....................\n.............#........#....#.........#...............#.........#..................................................................\n.#......#...................#.......#...........................................................................#.............#...\n.............##...........#.........................................#......................#.......................#..............\n.....#................#.....#...................##.......#......#...........#.......#............................................#\n.#..#.............................................................#..............................................#................\n...#......#............................#..........................................................................................\n...................................................................................#..............................................\n#...................................#...................##..................#...............................................#.....\n................#.....#...........................#...............................................................................\n.................................#............#........................................................#.............#............\n..................#......#........................................................#..^.....#......................................\n..#.....#.................................................#........#.........................................................#..#.\n................#..............................#....................................................#.............................\n................#....#..........................................#............................#....................................\n...............#.......................................#.....................................#..............................#.....\n..................................................#............#..............#........##.#.....................................#.\n.#..................................#......#..#............#.................#.............#......#..#.......................#....\n..................#..............................#..........................#..........................##............#.......#....\n.........#..................................................................#...................#..........................#......\n................#....................................#............#............................................................#..\n..##..........#.#..#........................#....#.................................#.............................................#\n............................................#.........................................................#..#...................#...#\n.......................................#......................................................................#...................\n....#.......................#...#.........................................................................#................#......\n...........#...........#............#.....#...#...................#........................................#.................##...\n....#...........................................................................................#.................................\n..............................................................................#.#.............................................#...\n.........#..#..............................................#......................................................................\n#...................#........#............#.......#.....................#................#.#.#............#..................#....\n.......................#................#.....#...........#.....................#...........................#..........#.........#\n........................................................................#.#.....................#.................................\n...#.................................................#..........................#........#.......................#................\n............................#...........#.......#.................................#.##...........#............................#...\n...##.....................................#.........................#...........#...................................#.............\n#.#..#................................#.........#..#.#..........#......................#............#......................#......\n....#.............................................................#...............................................................\n...............#.#......#........................................................................#.........................#......\n................#.....#...#...............#.........................................#....#...........................#.......#....\n...............#.........................................................................#........................................\n..........................#..##...................................................................................................\n.##....................#..#.....................#..............................................#........................#.........\n............................................................................................#............#........................\n#...................#...#......................#...............#............#....................................#................\n...........................#........#......................................................................#......................\n................................#...#...#.#.......#.................#.......#................##...................................\n........................#.......................#.................................................................................\n#...#...............#.........................................................................#.......................#...........\n.........#...................#...............................................#...................##..............................#\n.....#............................................................................................................................\n.#.......#...............................................................#.......................#................#..........#....\n....................................#..........#......#...........................................................#.#.#..#...#....\n......................#..............................................##..................#........................................\n....#......#...................#................#..............................................................................#..\n...........#.................##.#...................................................#..#................#.........................\n......#.....#......#...#........#.....................................#....#...........#................#..........#..............\n#.......##.....#........#...........................................#...#..........#.............#........#.......#...............\n......................#......#...................#.........................#........................#..................#...#......\n.....#...........#...#.........................##.......#.....................................#...................................\n....................................................................#.............................................................\n........#...............#.........................................................................................................\n.......................#.........................#..........................................#.#....#..#....#......................\n........................##............................................................#.....#.........#....#......#..#............\n........................#........#..................#................#.#.##............#..........................................\n.......................................................#.........................#...............##...................##....#...#.\n........#...#.............#...........#........#......................#...#.........#..#..................#.#............#........\n#.#....................#..........................................#...................................................#........#..\n...............................................#............#.....#.#...........#........#.#....................................#.\n......#..........................................#............................................................#..........#...#...#\n....................#......................................#....#..............................#....................#.............\n......................#.#.....#......#....#..#....................................................................................\n.....#..........................................#.........#...................................#................#.......#..........\n........##....................................................................#...................................#.............#.\n#.........................................#..#....#................#.#.....#....................#............................#....\n#....#..#..........#..................#......................#....................................................................\n...................#..#....#...................#...........................#.#..#............#......#................#.......#.#..\n...............#......................................................#......#..#..............#.....................#............\n............#.....................#.........#....#...............#.......#...#...............#.......................##.#.........\n....#............#..................#....#.......................#..............................................#...#.............\n..........................................#....#..........#..#....................#......#......................#...#.............\n............#..........#...........#......#..#......................................................................#.............\n...#.................#.............................................................#..............................................\n.....#.#.........#....................................................#..................#...............#........................\n..#......................##.....#.........#..............#..#............#..#............................................#........\n...................#.....#.........#........#....................#..................................#................#............\n....#.............................#.............................#.......................#.......................#.....#...........\n...#......#....#...........#............................#.....#........#......#...................................................\n........#..........#............................#..#..#.......................................#....#.##...........................\n.......#......##........#.................#.............................................................#....#........#...........\n.....................#............................................#.#......#.....##....#........#.................................\n...........#.....................##.#....#..#.....................................................................................\n....................#......#................................#.....................................................#.......#..#....\n.....................................................#.........#.......................................#.....##..#................"

  private val exampleInput =
    "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."

  trait Position
  private trait Direction
  private case object Left extends Direction
  private case object Up extends Direction
  private case object Right extends Direction
  private case object Down extends Direction
  private case object Obstruction extends Position
  private case class Field(seen: Boolean, maybeGuard: Option[Guard])
      extends Position
  private case class Guard(direction: Direction)
  private object Guard {
    def apply(guardChar: Char): Guard = {
      if (guardChar == '<') {
        Guard(Left)
      } else if (guardChar == '^') {
        Guard(Up)
      } else if (guardChar == '>') {
        Guard(Right)
      } else {
        Guard(Down)
      }
    }
  }

  val input: Array[Array[Position]] = myInput
    .split("\n")
    .map(
      _.map(e =>
        if (e == '#') { Obstruction }
        else if (e == '.') { Field(false, None) }
        else { Field(true, Some(Guard(e))) }
      ).toArray
    )

  def day6task1: Int = {
    val solvedPuzzle: Array[Array[Position]] = solvePuzzle(input, Set.empty)
    getNumberOfSeenFields(solvedPuzzle)
  }
  
  def day6task2: Int = {
    val everyMapWithOneMoreObstruction: List[Array[Array[Position]]] =
      input.zipWithIndex.flatMap { case (row, i) =>
        row.zipWithIndex.collect {
          case (position, j) if position != Obstruction =>
            position match
              case Field(_, None) =>
                val updatedRow = row.updated(j, Obstruction)
                input.updated(i, updatedRow)
              case Field(_, Some(_)) => input
        }
      }.toList
    val solvedPuzzles = everyMapWithOneMoreObstruction.filter { puzzle =>
      try {
        solvePuzzle(puzzle, Set.empty)
        true
      } catch {
        case _: Exception => false
      }
    }
    everyMapWithOneMoreObstruction.length - solvedPuzzles.length
  }

  @tailrec
  private def solvePuzzle(
      puzzleInput: Array[Array[Position]],
      steps: Set[((Int, Int), Direction)]
  ): Array[Array[Position]] = {
    if (notPossibleToStepAnother(puzzleInput)) {
      puzzleInput
    } else {
      val (updatedPuzzle, updatedSteps) = stepOne(puzzleInput, steps)
      solvePuzzle(updatedPuzzle, updatedSteps)
    }
  }

  private def notPossibleToStepAnother(
      array: Array[Array[Position]]
  ): Boolean = {
    val maxSizeOfMap = array.length - 1
    val direction = getTheGuardsDirection(array)
    val (x, y) = getCoordinatesOfGuard(array)
    if (
      (x == 0 && direction == Up) ||
      (x == maxSizeOfMap && direction == Down) ||
      (y == 0 && direction == Left) ||
      (y == maxSizeOfMap && direction == Right)
    ) true
    else false
  }

  private def getTheGuardsDirection(
      array: Array[Array[Position]]
  ): Direction = {
    array.flatten
      .collectFirst { case Field(_, Some(Guard(direction))) =>
        direction
      }
      .getOrElse {
        throw new NoSuchElementException("No guard found in the array")
      }
  }

  private def getCoordinatesOfGuard(
      array: Array[Array[Position]]
  ): (Int, Int) = {
    array.zipWithIndex
      .flatMap { case (row, rowIndex) =>
        row.zipWithIndex.collect { case (Field(_, Some(_)), colIndex) =>
          (rowIndex, colIndex)
        }
      }
      .headOption
      .getOrElse(
        throw new NoSuchElementException("No guard found in the array")
      )
  }

  private def stepOne(
      array: Array[Array[Position]],
      steps: Set[((Int, Int), Direction)]
  ): (Array[Array[Position]], Set[((Int, Int), Direction)]) = {
    val (x, y) = getCoordinatesOfGuard(array)
    val direction = getTheGuardsDirection(array)
    val fieldInFrontOf = direction match {
      case Left  => (x, y - 1)
      case Up    => (x - 1, y)
      case Right => (x, y + 1)
      case Down  => (x + 1, y)
    }
    val fieldSeenInFrontOf = steps.contains(fieldInFrontOf, direction)
    if (fieldSeenInFrontOf) {
      throw Exception("We are in a loop, baby")
    }
    if (array(fieldInFrontOf._1)(fieldInFrontOf._2) == Obstruction) {
      (
        array.updated(
          x,
          array(x).updated(
            y,
            Field(true, Some(turnRight(Guard(direction))))
          )
        ),
        steps
      )
    } else {
      val updatedSteps = steps + ((fieldInFrontOf, direction))
      val updatedArrayWithGuardRemoved = array.updated(
        x,
        array(x).updated(
          y,
          Field(true, None)
        )
      )
      (
        updatedArrayWithGuardRemoved.updated(
          fieldInFrontOf._1,
          updatedArrayWithGuardRemoved(fieldInFrontOf._1).updated(
            fieldInFrontOf._2,
            Field(true, Some(Guard(direction)))
          )
        ),
        updatedSteps
      )
    }
  }

  private def getNumberOfSeenFields(
      everyPosition: Array[Array[Position]]
  ): Int = {
    everyPosition.flatten.collect {
      case field: Field if field.seen => field
    }.length
  }

  private def turnRight(guard: Guard): Guard = {
    guard.direction match
      case Down  => Guard(Left)
      case Left  => Guard(Up)
      case Up    => Guard(Right)
      case Right => Guard(Down)
  }

}
