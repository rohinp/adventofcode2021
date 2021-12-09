package day6

import scala.util.chaining._
import scala.io.Source

def simmulation(fishies: List[Long], numOfDays: Int): Long = 
    def loop(genaration:Map[Long, Long], currentDay:Int):Long = 
        if currentDay > numOfDays then genaration.values.sum
        else 
            val next = genaration.foldLeft(Map.empty[Long, Long]) {
                case (accNextGen, (currentAge, occurances)) => {
                    if currentAge == 0 then
                        accNextGen ++ Map(
                            6L -> (occurances + accNextGen.getOrElse(6, 0L)),
                            8L -> (occurances + accNextGen.getOrElse(8, 0L))
                        )
                    else 
                        accNextGen + (currentAge - 1 -> (occurances + accNextGen.getOrElse(currentAge - 1, 0L)))    
                }
            }
            loop(next, currentDay + 1)
    loop(fishies.groupMapReduce(identity)(_ => 1L)(_ + _), 1)

@main def lanternFishPart = 
    Source.fromFile("./data/day6")
        .getLines.toList.head
        .split(",")
        .map(_.toLong)
        .toList
        .pipe(simmulation(_,256))
        .tap(println)