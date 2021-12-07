package day5

import scala.util.chaining._
import scala.io.Source

case class Point(y:Int, x:Int)

case class Line(start:Point, end:Point):
    def horizontalAndVerticalSegment:Vector[Point] = (start, end) match
        case (s, e)  if s.x < e.x && s.y == e.y => Vector.unfold(s.x)(x => if x > e.x then None else Some((Point(s.y, x), x + 1)))
        case (s, e)  if s.x > e.x && s.y == e.y => Vector.unfold(s.x)(x => if x < e.x then None else Some((Point(s.y, x), x - 1)))
        case (s, e)  if s.y < e.y && s.x == e.x => Vector.unfold(s.y)(y => if y > e.y then None else Some((Point(y, s.x), y + 1)))
        case (s, e)  if s.y > e.y && s.x == e.x => Vector.unfold(s.y)(y => if y < e.y then None else Some((Point(y, s.x), y - 1)))
        case _ => Vector()

    def allSegment:Vector[Point] = (start, end) match
        case (s, e)  if s.x < e.x && s.y == e.y => Vector.unfold(s.x)(x => if x > e.x then None else Some((Point(s.y, x), x + 1)))
        case (s, e)  if s.x > e.x && s.y == e.y => Vector.unfold(s.x)(x => if x < e.x then None else Some((Point(s.y, x), x - 1)))
        case (s, e)  if s.y < e.y && s.x == e.x => Vector.unfold(s.y)(y => if y > e.y then None else Some((Point(y, s.x), y + 1)))
        case (s, e)  if s.y > e.y && s.x == e.x => Vector.unfold(s.y)(y => if y < e.y then None else Some((Point(y, s.x), y - 1)))
        case _ => unfoldDiagonal(start.x, start.y, Vector())

    def unfoldDiagonal(x:Int, y:Int, acc:Vector[Point]):Vector[Point] = 
        if x == end.x && y == end.y then acc.appended(Point(y,x)) 
        else if x < end.x && y < end.y then unfoldDiagonal(x + 1, y + 1, acc.appended(Point(y,x))) 
        else if x > end.x && y > end.y then unfoldDiagonal(x - 1, y - 1, acc.appended(Point(y,x))) 
        else if x > end.x && y < end.y then unfoldDiagonal(x - 1, y + 1, acc.appended(Point(y,x))) 
        else if x < end.x && y > end.y then unfoldDiagonal(x + 1, y - 1, acc.appended(Point(y,x))) 
        else if x < end.x && y == end.y then unfoldDiagonal(x + 1, y , acc.appended(Point(y,x))) 
        else if x > end.x && y == end.y then unfoldDiagonal(x - 1, y, acc.appended(Point(y,x))) 
        else if x == end.x && y < end.y then unfoldDiagonal(x, y + 1, acc.appended(Point(y,x))) 
        else unfoldDiagonal(x, y - 1, acc.appended(Point(y,x))) //if x == end.x && y > end.y then 
    
object Line:
    val line = """(\d+),(\d+)\s->\s(\d+),(\d+)""".r 
    
    def parse:String => Line = 
        case line(y1,x1,y2,x2) => Line(Point(y1.toInt,x1.toInt), Point(y2.toInt,x2.toInt))

end Line
object Surface:
    opaque type Surface = Vector[Line]

    extension (surface:Surface)
        def repeatedCoordinatesCount:Int = 
            surface.flatMap(_.horizontalAndVerticalSegment).groupBy(identity).collect{case (p, ps) if ps.length > 1 => p}.size
        
        def repeatedCoordinatesCount2:Int = 
            surface.flatMap(_.allSegment).groupBy(identity).collect{case (p, ps) if ps.length > 1 => p}.size

        def map = surface.map

    def apply(itr:Iterator[String]):Surface = itr.map(Line.parse).toVector
end Surface

@main def ventsPart1 =
    Source.fromFile("./data/day5")
        .getLines
        .pipe(Surface(_))
        .repeatedCoordinatesCount
        .tap(println)

@main def ventsPart2 =
    Source.fromFile("./data/day5")
        .getLines
        .pipe(Surface(_))
        .repeatedCoordinatesCount2
        .tap(println)
