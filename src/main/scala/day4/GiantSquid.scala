package day4

import scala.io.Source
import scala.util.chaining._

case class Point(x:Int, y:Int, value:Int, isMarked:Boolean = false):
    def checkAndmark(v:Int):Point = if v == value then this.copy(value = v, isMarked = true) else this
    
object Board:
    opaque type Board = Vector[Point]

    extension (board:Board)
        def markIt(num:Int):Board = board.map(_.checkAndmark(num))
        def markCount:Int = board.filter(_.isMarked).length
        def checkWinning:Boolean =
            if board.markCount > 5 then board.checkRows || board.checkColumns else false
        def checkRows:Boolean = 
            (0 to 4).map(row => board.filter(_.x == row).forall(_.isMarked)).contains(true)
        def checkColumns:Boolean = 
            (0 to 4).map(row => board.filter(_.y == row).forall(_.isMarked)).contains(true)
        def calculateScore(lastDraw:Int):Int = board.filter(p => !p.isMarked).map(_.value).sum * lastDraw

    val height, width = 5 //fixed

    def apply(boardLines:Vector[String]):Board = 
        val rawBoard = boardLines.map(_.split(" ").toVector.filter(_.trim.nonEmpty))
        for
            x <- (0 to 4).toVector
            y <- (0 to 4).toVector
        yield Point(x, y, rawBoard(x)(y).toInt)

    //EmptyBoard    
    def apply():Board = Vector.empty[Point]

end Board

import Board._

def readBoard:(List[Int], Vector[Board]) =
    val (luckyDraws+:boards) = Source.fromFile("./data/day4").getLines.toVector

    def loop(input:Vector[String], boardLines: Vector[String], accBoards:Vector[Board]):Vector[Board] = input match
        case emptyLine+:l1+:l2+:l3+:l4+:l5+:remaining if emptyLine.trim.isEmpty => 
            loop(remaining, Vector() ,accBoards.appended(Board(Vector(l1,l2,l3,l4,l5))))
        case _ => accBoards

    (luckyDraws.split(",").map(Integer.parseInt(_)).toList, loop(boards,Vector(), Vector()))

def winningBoard(input:(List[Int], Vector[Board]), lastDraw:Option[Int]):(Board,Int) = input match 
    case (_, boards) if boards.exists(_.checkWinning) => 
        (boards.find(_.checkWinning).get, lastDraw.get)
    case (draw::xs, boards) => 
        winningBoard((xs, boards.map(_.markIt(draw))), Some(draw))
    case (Nil, boards) => (Board(), -1) //Error condition, no winner

def lastWinningBoard(input:(List[Int], Vector[Board]), stackOfWinBoard:List[(Board,Int)]):(Board,Int) = input match 
    case (Nil, boards) => stackOfWinBoard.head
    case (draw::xs, boards) => 
        val updatedBoards = boards.map(_.markIt(draw))
        if updatedBoards.exists(_.checkWinning)
            then lastWinningBoard((xs, updatedBoards.filterNot(_.checkWinning)),(updatedBoards.find(_.checkWinning).get,draw) :: stackOfWinBoard)
            else lastWinningBoard((xs, updatedBoards),stackOfWinBoard)

@main def bingoPart1 = 
    readBoard
        .pipe(winningBoard(_, None))
        .pipe((b,d) => b.calculateScore(d))
        .tap(println)

@main def bingoPart2 = 
    readBoard
        .pipe(lastWinningBoard(_, List()))
        .pipe((b,d) => b.calculateScore(d))
        .tap(println)