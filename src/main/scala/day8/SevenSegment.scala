package day8

import scala.io.Source
import scala.util.chaining._


object Digit:
    opaque type Digit = String
    val uniqueNumberMap = Map(1 -> 2, 4 -> 4, 7 -> 3, 8 -> 7)

    given Ordering[Digit] = Ordering[String]

    extension (digit:Digit)
        def isUnique:Boolean = 
            uniqueNumberMap.values.exists(_ == digit.length)
        def itr:String = digit
        def length = digit.length

    def apply(in:String):Digit = in
end Digit

import Digit._

case class Input(signal:List[Digit], digits:List[Digit]):
    def uniqueDigitCount:Int = digits.count(_.isUnique)

object Input:
    def apply(i:String):Input = 
        i.split('|').toList
            .pipe{
                case ss::ds::_ => 
                    Input(ss.trim.split(" ").toList.map(Digit(_)), ds.trim.split(" ").toList.map(Digit(_)))
                case _ => 
                    Input(List(), List())
            }
end Input
        
def part1(itr:Iterator[Input]) = itr.map(_.uniqueDigitCount).sum

type PatternTree = Vector[Map[Char,Char]]

def createPatternTree(signals:List[Digit]):PatternTree = 
    val correctSignals = Map(2 -> "cf",4 -> "bcdf", 3 -> "acf", 7 -> "abcdefg")
    def loop(ss:List[String], acc:PatternTree):PatternTree = 
        ss match 
            case s::xs => 
                val keys:Map[Char,Char] => List[Char] =
                    m => s.filterNot(k => m.keys.toList.contains(k)).toList
                val values:Map[Char,Char] => List[Char] =
                    m => correctSignals(s.length).filterNot(v => m.values.toList.contains(v)).toList
                acc match
                    case IndexedSeq() if s.length == 1 => loop(xs,Vector(keys(Map()).zip(values(Map())).toMap))
                    case IndexedSeq() => loop(xs,Vector(keys(Map()).zip(values(Map())).toMap)) ++ loop(xs,Vector(keys(Map()).zip(values(Map()).reverse).toMap))
                    case ps if s.length == 1 => loop(xs,ps.map(m => m ++ keys(m).zip(values(m))))
                    case ps => loop(xs,acc.map(m => m ++ keys(m).zip(values(m)).toMap)) ++ loop(xs,acc.map(m => m ++ keys(m).zip(values(m).reverse).toMap))
            case _ => acc
    loop(signals.collect{case x if x.isUnique => x.itr}.sortBy(s => (s.length,s)), Vector())


def decodedDigits(patternTree:PatternTree):List[String] => Int =
    digits =>
        val original = Map("abcefg" -> 0,"cf"     -> 1,"acdeg"  -> 2,"acdfg"  -> 3,"bcdf"   -> 4, "abdfg"  -> 5, "abdefg" -> 6, "acf"    -> 7, "abcdefg"-> 8,"abcdfg" -> 9)
        digits.map(digit => 
                patternTree.map(m => digit.map(m).sorted)
                .map(k => original.get(k))
                .collect{case Some(v) => v}.head)
                .pipe(_.mkString.toInt)

def part2(itr:List[String]) =
    itr.map(Input(_))
        .map(i => (decodedDigits compose createPatternTree)(i.signal)(i.digits.map(_.itr)))
        .sum
    
@main def ssd =
    Source.fromFile("./data/day8")
        .getLines
        .toList
        .pipe(part2)
        .tap(println)
    