package day2

import scala.io.Source
import java.io.File
import scala.util.Try
import scala.util.chaining._
object part1:
    enum Move:
        case Forward(value:Int)
        case Down(value:Int)
        case Up(value:Int)
        case Invalid

    case class Position(horizontal:Int = 0, depth:Int = 0):
        def result = horizontal * depth

    def calculateNext:Position => Move => Position =
        pos => 
            case Move.Forward(value) => pos.copy(horizontal = pos.horizontal + value)
            case Move.Down(value) => pos.copy(depth = pos.depth + value)
            case Move.Up(value) => pos.copy(depth = pos.depth - value)
            case Move.Invalid => pos // ignoring error

    def convert:Iterator[String] => Iterator[Move] =
        itr => 
            def toMove:String => Move = _.split(" ").toList match
                case "forward"::value::_ => Move.Forward(value.toInt)
                case "down"::value::_ => Move.Down(value.toInt)
                case "up"::value::_ => Move.Up(value.toInt)
                case _ => Move.Invalid            

            Try{itr.map(toMove)}.getOrElse(Iterator.empty[Move])

    def finalPosition(moves:Iterator[Move]):Int =
        moves.foldLeft(Position()){(p,m) => calculateNext(p)(m)}.result

object part2:
    enum Move:
        case Forward(value:Int)
        case Down(value:Int)
        case Up(value:Int)
        case Invalid

    case class Position(horizontal:Int = 0, depth:Int = 0, aim:Int = 0):
        def result = horizontal * depth
        def down(value:Int):Position = this.copy(aim = aim + value)
        def up(value:Int):Position = this.copy(aim = aim - value)
        def forward(value:Int):Position = this.copy(horizontal = horizontal + value, depth = if aim == 0 then depth else depth + aim * value)

    def calculateNext:Position => Move => Position =
        pos => 
            case Move.Forward(value) => pos.forward(value)
            case Move.Down(value) => pos.down(value)
            case Move.Up(value) => pos.up(value)
            case Move.Invalid => pos // ignoring error

    def convert:Iterator[String] => Iterator[Move] =
        itr => 
            def toMove:String => Move = _.split(" ").toList match
                case "forward"::value::_ => Move.Forward(value.toInt)
                case "down"::value::_ => Move.Down(value.toInt)
                case "up"::value::_ => Move.Up(value.toInt)
                case _ => Move.Invalid            

            Try{itr.map(toMove)}.getOrElse(Iterator.empty[Move])

    def finalPosition(moves:Iterator[Move]):Int =
        moves.foldLeft(Position()){(p,m) => calculateNext(p)(m)}.result
@main def runPosition =
    import part2._

    Source.fromFile(new File("./data/day2"))
        .getLines
        .pipe(convert)
        .pipe(finalPosition)
        .pipe(println)
        