package day7

import scala.util.chaining._
import scala.io.Source

//part one not so smart solution
def crabStepCalculator(itr:List[Int]) =
    val memo: Map[Int, Int] = itr.groupMapReduce(identity)(_ => 1)(_ + _)
    val startStep = memo.keysIterator.min
    val endStep = memo.keysIterator.max
    def loop(steps:List[Int],results:List[Int]):List[Int] = 
        steps match 
            case Nil => results
            case x::xs =>
                val t = memo.foldLeft(0){(acc, elem) =>
                    val (k ,v) = elem
                    acc + (Math.abs(k - x) * v)
                } 
                loop(xs, t :: results)

    loop((startStep to endStep).toList, List.empty[Int]).min
     
//better solution, simple math
def part1(ls:Vector[Int]):Int =
    ls.sorted
        .pipe(sl => sl(sl.size / 2))
        .pipe(median => ls.map(x => Math.abs(x - median)).sum)

def part2(itr:List[Int]) =
    val memo: Map[Int, Int] = itr.groupMapReduce(identity)(_ => 1)(_ + _)
    val startStep = memo.keysIterator.min
    val endStep = memo.keysIterator.max
    def loop(steps:List[Int],results:List[Int]):List[Int] = 
        steps match 
            case Nil => results
            case x::xs =>
                val t = memo.foldLeft(0){(acc, elem) =>
                    val (k ,v) = elem
                    acc + ((1 to Math.abs(k - x)).sum * v)
                }
                loop(xs, t :: results)

    loop((startStep to endStep).toList, List.empty[Int]).min

@main def sos =
    Source.fromFile("./data/day7")
        .getLines.toList.head
        .split(",")
        .map(_.toInt)
        .toList
        .pipe(part2)
        .tap(println)
