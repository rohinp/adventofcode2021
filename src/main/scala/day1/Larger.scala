package day1

import scala.io.Source
import scala.util.chaining._
import scala.util.Try
import java.io.File

def convert:Iterator[String] => Iterator[Int] = 
    itr => Try(itr.map(_.toInt)).getOrElse(Iterator.empty[Int])

def countPart1:Iterator[Int] => Int = 
    _.foldLeft[(Option[Int],Int)]((None,0)){
        case ((None, counter), next) => (Some(next), counter)
        case ((Some(previous), counter), next) if previous < next => (Some(next), counter + 1)
        case ((Some(previous), counter), next) => (Some(next), counter)
    }._2


def countPart2:Iterator[Int] => Int = 
    input => 
        def loop(in:Vector[Int], counter:Int, previous:Option[Int]):Int = (in, previous) match
            case (a+:b+:c+:remaining, None) => loop(b+:c+:remaining, counter, Some(a + b + c))
            case (a+:b+:c+:remaining, Some(previous)) if previous < (a + b + c) => loop(b+:c+:remaining, counter + 1, Some(a + b + c))
            case (a+:b+:c+:remaining, Some(previous))  => loop(b+:c+:remaining, counter, Some(a + b + c))
            case _  => counter
            
        loop(input.toVector, 0, None)

@main def testLargeCounter = 
    Source.fromFile(new File("./data/day1")).getLines
        .pipe(convert)
        .pipe(countPart2)
        .tap(println)
        
        