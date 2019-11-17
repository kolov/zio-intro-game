package net.degoes.zio

import java.io.IOException
import java.nio.file.{Files, Paths}

import net.degoes.zio.PromptName.StdInputFailed
import org.w3c.dom.css.CSSUnknownRule
import zio.ZSchedule.Decision
import zio.{ZSchedule, _}
import zio.console.putStrLn
import zio.duration.Duration
import zio.duration.Duration.Finite

import scala.util.{Failure, Success, Try}

object HelloWorld extends App {
  import zio.console._

  /**
    * EXERCISE 1
    *
    * Implement a simple "Hello World" program using the effect returned by `putStrLn`.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("Hello").as(0)
}

object ErrorConversion extends App {
  val StdInputFailed = 1

  import zio.console._

  val failed =
    putStrLn("About to fail...") *>
      ZIO.fail("Uh oh!") *>
      putStrLn("This will NEVER be printed!")

  /**
    * EXERCISE 2
    *
    * Using `ZIO#orElse` or `ZIO#fold`, have the `run` function compose the
    * preceding `failed` effect into the effect that `run` returns.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    failed.fold(e => StdInputFailed, _ => 0)
}

object PromptName extends App {
  val StdInputFailed = 1

  import zio.console._

  /**
    * EXERCISE 3
    *
    * Implement a simple program that asks the user for their name (using
    * `getStrLn`), and then prints it out to the user (using `putStrLn`).
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _ <- putStrLn("What's your name")
      name <- getStrLn
      _ <- putStrLn(s"Hello, $name")
    } yield ()).fold(_ => StdInputFailed, _ => 0)
}

object ZIOTypes {
  type ??? = Nothing

  /**
    * EXERCISE 4
    *
    * Provide definitions for the ZIO type aliases below.
    */
  type Task[+A] = ZIO[Any, Throwable, A]
  type UIO[+A] = ZIO[Any, Nothing, A]
  type RIO[-R, +A] = ZIO[R, Throwable, A]
  type IO[+E, +A] = ZIO[Any, E, A]
  type URIO[-R, +A] = ZIO[R, Nothing, A]
}

object NumberGuesser extends App {
  import zio.console._
  import zio.random._

  def analyzeAnswer(random: Int, guess: String) =
    if (random.toString == guess.trim) putStrLn("You guessed correctly!")
    else putStrLn(s"You did not guess correctly. The answer was ${random}")

  /**
    * EXERCISE 5
    *
    * Choose a random number (using `nextInt`), and then ask the user to guess
    * the number, feeding their response to `analyzeAnswer`, above.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val game: ZIO[Console with Random, IOException, Unit] = for {
      num <- ZIO.accessM[Random](_.random.nextInt)
      _ <- putStrLn("Guess")
      guess <- ZIO.accessM[Console](_.console.getStrLn)
      _ <- analyzeAnswer(num, guess)
    } yield ()

    game.fold(_ => 1, _ => 0)
  }

}

object AlarmApp extends App {
  import zio.console._
  import zio.duration._
  import java.io.IOException

  /**
    * EXERCISE 6
    *
    * Create an effect that will get a `Duration` from the user, by prompting
    * the user to enter a decimal number of seconds.
    */
  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      Try(Duration.fromNanos((input.toDouble * 1000000000L).toLong)) match {
        case Success(duration)                 => ZIO.succeed(duration)
        case Failure(e: NumberFormatException) => ZIO.fail(e)
        case Failure(_)                        => ZIO.fail(new NumberFormatException())
      }

    def fallback(input: String): ZIO[Console, IOException, Duration] =
      putStrLn(s"Not correct number: $input") *> getAlarmDuration

    for {
      _ <- putStrLn("Please, type in a decimal number of seconds")
      input <- getStrLn
      duration <- parseDuration(input)
        .foldM(_ => fallback(input), a => ZIO.succeed(a))
    } yield duration

  }

  /**
    * EXERCISE 7
    *
    * Create a program that asks the user for a number of seconds to sleep,
    * sleeps the specified number of seconds, and then prints out a wakeup
    * alarm message.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      d <- getAlarmDuration
      _ <- putStrLn(s"Will sleep for $d")
      _ <- ZIO.unit.delay(d)
      _ <- putStrLn(s"TADA!")
    } yield ()).fold(_ => 1, _ => 0)

}

object Cat extends App {
  import zio.console._
  import zio.blocking._
  import java.io.IOException

  /**
    * EXERCISE 8
    *
    * Implement a function to read a file on the blocking thread pool, storing
    * the result into a string.
    */
  def readFile(file: String): ZIO[Blocking, IOException, String] = {
    val read = ZIO
      .effect(Files.readAllBytes(Paths.get(file)))
      .map(new String(_))
      .refineOrDie[IOException] { case e: IOException => e }
    blocking(read)
  }

  /**
    * EXERCISE 9
    *
    * Implement a version of the command-line utility "cat", which dumps the
    * contents of the specified file to standard output.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    (args match {
      case head :: Nil =>
        readFile(head)
          .flatMap(putStrLn)
          .flatMapError(e => putStrLn(s"Error: ${e}"))
      case _ => ZIO.effectTotal(println("Usage: cat [filename]"))
    }).fold(_ => 1, _ => 0)
  }
}

object CatIncremental extends App {
  import zio.console._
  import zio.blocking._
  import java.io._

  /**
    * EXERCISE 10
    *
    * Implement all missing methods of `FileHandle`. Be sure to do all work on
    * the blocking thread pool.
    */
  final case class FileHandle private (private val is: InputStream) {

    final def close: ZIO[Blocking, IOException, Unit] =
      blocking(ZIO.effect(is.close())).refineOrDie { case e: IOException => e }

    final def read: ZIO[Blocking, IOException, Option[Chunk[Byte]]] = {
      val buf = new Array[Byte](100)

      def readNextChunk(initial: Option[Chunk[Byte]]): Option[Chunk[Byte]] =
        is.read(buf) match {
          case -1 => initial
          case n =>
            val data = Chunk.fromArray(buf.dropRight(100 - n))
            val all = initial match {
              case None => Some(data)
              case some => some.map(_ ++ data)
            }
            readNextChunk(all)
        }
      blocking(ZIO.effect(readNextChunk(None)))
        .refineOrDie { case e: IOException => e }
    }
  }
  object FileHandle {
    final def open(file: String): ZIO[Blocking, IOException, FileHandle] =
      blocking(ZIO.effect(FileHandle(new FileInputStream(file)))).refineOrDie {
        case e: IOException => e
      }
  }

  /**
    * EXERCISE 11
    *
    * Implement an incremental version of the `cat` utility, using `ZIO#bracket`
    * or `ZManaged` to ensure the file is closed in the event of error or
    * interruption.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (args match {
      case head :: Nil =>
        FileHandle
          .open(head)
          .bracket(fh => fh.close.catchAll(_ => ZIO.unit)) {
            (handle: FileHandle) =>
              handle.read.flatMap {
                case None    => putStrLn("File empty")
                case Some(c) => putStrLn(new String(c.toArray[Byte]))
              }
          }
          .flatMapError(e => putStrLn(s"Error: ${e}"))
      case _ => ZIO.effectTotal(println("Usage: cat [filename]"))
    }).fold(_ => 1, _ => 0)
}

object ComputePi extends App {
  import zio.random._
  import zio.console._

  /**
    * Some state to keep track of all points inside a circle,
    * and total number of points.
    */
  final case class PiState(inside: Ref[Long], total: Ref[Long])

  /**
    * A function to estimate pi.
    */
  def estimatePi(inside: Long, total: Long): Double =
    (inside.toDouble / total.toDouble) * 4.0

  /**
    * A helper function that determines if a point lies in
    * a circle of 1 radius.
    */
  def insideCircle(x: Double, y: Double): Boolean =
    Math.sqrt(x * x + y * y) <= 1.0

  /**
    * An effect that computes a random (x, y) point.
    */
  val randomPoint: ZIO[Random, Nothing, (Double, Double)] =
    nextDouble zip nextDouble

  /**
    * EXERCISE 12
    *
    * Build a multi-fiber program that estimates the value of `pi`. Print out
    * ongoing estimates continuously until the estimation is complete.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {

    def untilPrecisionReached(state: PiState, precision: Double) = new Schedule[Unit, Unit] {
      override type State = PiState
      override val initial: ZIO[Any, Nothing, PiState] = ZIO.succeed(state)
      override val update: (
          Unit,
          State
      ) => ZIO[Any, Nothing, ZSchedule.Decision[State, Unit]] = (_, _) =>
        for {
          inside <- state.inside.get
          total <- state.total.get
          currentError = Math.abs(estimatePi(inside, total) - Math.PI)
        } yield Decision(
          currentError > precision,
          Duration.Zero,
          state,
          () => ()
        )
    }

    def addPoint(name: String, state: PiState): ZIO[ZEnv, Nothing, Unit] =
      for {
        hit <- randomPoint.map { case (x, y) => insideCircle(x, y) }
        total <- state.total.update(_ + 1)
        inside <- state.inside.update(n => if (hit) n + 1 else n)
        _ <- putStrLn(s"$name:$total: ${estimatePi(inside, total)}")
      } yield ()

    def runners(
        countRunners: Int
    ): ZIO[zio.ZEnv, Nothing, Unit] =
      for {
        insideRef <- Ref.make(0L)
        totalRef <- Ref.make(0L)
        state = PiState(insideRef, totalRef)
        runners <- (0 until countRunners)
          .map(
            i => addPoint(s"R$i", state).repeat(untilPrecisionReached(state, 0.0001)).as(())
          )
          .reduce { (a, b) =>
            a.zipPar(b).as(())
          }
        insideFinal <- insideRef.get
        totalFinal <- totalRef.get

        _ <- putStrLn(
          s"Result: ${estimatePi(insideFinal, totalFinal)} in $totalFinal iteratiions"
        )
      } yield runners

    runners(10).fold(_ => 1, _ => 0)
  }

}

object Hangman extends App {
  import zio.console._
  import zio.random._
  import java.io.IOException

  /**
    * EXERCISE 13
    *
    * Implement an effect that gets a single, lower-case character from
    * the user.
    */
  lazy val getChoice: ZIO[Console, IOException, Char] = ???

  /**
    * EXERCISE 14
    *
    * Implement an effect that prompts the user for their name, and
    * returns it.
    */
  lazy val getName: ZIO[Console, IOException, String] = ???

  /**
    * EXERCISE 15
    *
    * Implement an effect that chooses a random word from the dictionary.
    */
  lazy val chooseWord: ZIO[Random, Nothing, String] = ???

  /**
    * EXERCISE 17
    *
    * Implement the main game loop, which gets choices from the user until
    * the game is won or lost.
    */
  def gameLoop(ref: Ref[State]): ZIO[Console, IOException, Unit] = ???

  def renderState(state: State): ZIO[Console, Nothing, Unit] = {

    /**
      *
      *  f     n  c  t  o
      *  -  -  -  -  -  -  -
      *
      *  Guesses: a, z, y, x
      *
      */
    val word =
      state.word.toList
        .map(c => if (state.guesses.contains(c)) s" $c " else "   ")
        .mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    putStrLn(text)
  }

  final case class State(name: String, guesses: Set[Char], word: String) {
    final def failures: Int = (guesses -- word.toSet).size

    final def playerLost: Boolean = failures > 10

    final def playerWon: Boolean = (word.toSet -- guesses).isEmpty

    final def addChar(char: Char): State = copy(guesses = guesses + char)
  }

  sealed trait GuessResult
  object GuessResult {
    case object Won extends GuessResult
    case object Lost extends GuessResult
    case object Correct extends GuessResult
    case object Incorrect extends GuessResult
    case object Unchanged extends GuessResult
  }

  def guessResult(oldState: State, newState: State, char: Char): GuessResult =
    if (oldState.guesses.contains(char)) GuessResult.Unchanged
    else if (newState.playerWon) GuessResult.Won
    else if (newState.playerLost) GuessResult.Lost
    else if (oldState.word.contains(char)) GuessResult.Correct
    else GuessResult.Incorrect

  /**
    * EXERCISE 18
    *
    * Implement hangman using `Dictionary.Dictionary` for the words,
    * and the above helper functions.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    ???
}

/**
  * GRADUATION PROJECT
  *
  * Implement a game of tic tac toe using ZIO, then develop unit tests to
  * demonstrate its correctness and testability.
  */
object TicTacToe extends App {
  import zio.console._

  sealed trait Mark {
    final def renderChar: Char = this match {
      case Mark.X => 'X'
      case Mark.O => 'O'
    }
    final def render: String = renderChar.toString
  }
  object Mark {
    case object X extends Mark
    case object O extends Mark
  }

  final case class Board private (value: Vector[Vector[Option[Mark]]]) {

    /**
      * Retrieves the mark at the specified row/col.
      */
    final def get(row: Int, col: Int): Option[Mark] =
      value.lift(row).flatMap(_.lift(col)).flatten

    /**
      * Places a mark on the board at the specified row/col.
      */
    final def place(row: Int, col: Int, mark: Mark): Option[Board] =
      if (row >= 0 && col >= 0 && row < 3 && col < 3)
        Some(
          copy(
            value = value.updated(row, value(row).updated(col, Some(mark)))
          )
        )
      else None

    /**
      * Renders the board to a string.
      */
    def render: String =
      value
        .map(_.map(_.fold(" ")(_.render)).mkString(" ", " | ", " "))
        .mkString("\n---|---|---\n")

    /**
      * Returns which mark won the game, if any.
      */
    final def won: Option[Mark] =
      if (wonBy(Mark.X)) Some(Mark.X)
      else if (wonBy(Mark.O)) Some(Mark.O)
      else None

    private final def wonBy(mark: Mark): Boolean =
      wonBy(0, 0, 1, 1, mark) ||
        wonBy(0, 2, 1, -1, mark) ||
        wonBy(0, 0, 0, 1, mark) ||
        wonBy(1, 0, 0, 1, mark) ||
        wonBy(2, 0, 0, 1, mark) ||
        wonBy(0, 0, 1, 0, mark) ||
        wonBy(0, 1, 1, 0, mark) ||
        wonBy(0, 2, 1, 0, mark)

    private final def wonBy(
        row0: Int,
        col0: Int,
        rowInc: Int,
        colInc: Int,
        mark: Mark
    ): Boolean =
      extractLine(row0, col0, rowInc, colInc).collect { case Some(v) => v }.toList == List
        .fill(3)(mark)

    private final def extractLine(
        row0: Int,
        col0: Int,
        rowInc: Int,
        colInc: Int
    ): Iterable[Option[Mark]] =
      for {
        row <- (row0 to (row0 + rowInc * 2))
        col <- (col0 to (col0 + colInc * 2))
      } yield value(row)(col)
  }
  object Board {
    final val empty = new Board(Vector.fill(3)(Vector.fill(3)(None)))

    def fromChars(
        first: Iterable[Char],
        second: Iterable[Char],
        third: Iterable[Char]
    ): Option[Board] =
      if (first.size != 3 || second.size != 3 || third.size != 3) None
      else {
        def toMark(char: Char): Option[Mark] =
          if (char.toLower == 'x') Some(Mark.X)
          else if (char.toLower == 'o') Some(Mark.O)
          else None

        Some(
          new Board(
            Vector(
              first.map(toMark).toVector,
              second.map(toMark).toVector,
              third.map(toMark).toVector
            )
          )
        )
      }
  }

  val TestBoard = Board
    .fromChars(List(' ', 'O', 'X'), List('O', 'X', 'O'), List('X', ' ', ' '))
    .get
    .render

  /**
    * The entry point to the game will be here.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn(TestBoard) as 0
}
