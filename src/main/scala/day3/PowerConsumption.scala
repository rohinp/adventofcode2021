package day3

import scala.util.chaining._
import scala.io.Source

case class ColumnCounter(zeros:Int = 0, ones:Int = 0):
    def zero:ColumnCounter = this.copy(zeros = zeros + 1)
    def one:ColumnCounter = this.copy(ones = ones + 1)
    def mostCommon:Char = if zeros < ones then '1' else '0' 
    def leastCommon:Char = if zeros < ones then '0' else '1'

type BinaryRateCounter = List[ColumnCounter]
type BinaryString = String

def toColumnCounter(binary:Char):ColumnCounter => ColumnCounter = if binary == '0' then _.zero else _.one    

def updateRateCounter(binary:BinaryString):BinaryRateCounter => BinaryRateCounter = 
    brc => (if brc.isEmpty then List.fill(binary.length)(ColumnCounter()) else brc).zip(binary).map((cc, c) => toColumnCounter(c)(cc))

def gammaRate:BinaryRateCounter =>  Int = br => Integer.parseInt(br.map(_.mostCommon).mkString, 2)
def epsilonRate:BinaryRateCounter =>  Int = br => Integer.parseInt(br.map(_.leastCommon).mkString, 2)

//part 1:
def calculatePowerConsumption(itr:Iterator[BinaryString]):Int =
    itr.foldLeft(List.empty[ColumnCounter]){(acc, bs) => updateRateCounter(bs)(acc)}.pipe(brc => gammaRate(brc) * epsilonRate(brc))


//Part 2:
def lifeSupportRating:List[BinaryString] => Int =
    input => 
        def calculateRate:List[BinaryString] => BinaryRateCounter =
            _.foldLeft(List.empty[ColumnCounter]){(acc, bs) => updateRateCounter(bs)(acc)}
        
        def binaryRateCounterMost:BinaryRateCounter => BinaryString = _.map(bc => if bc.ones < bc.zeros then '0' else '1').mkString
        def binaryRateCounterLeast:BinaryRateCounter => BinaryString = _.map(bc => if bc.ones < bc.zeros then '1' else '0').mkString

        def recursiveFilter(pred:BinaryRateCounter => BinaryString)(input:List[BinaryString], index:Int):List[BinaryString] = 
            val binaryRates = (pred compose calculateRate)(input)
            input match
                case Nil => Nil
                case single@List(one) => single
                case xs if index < binaryRates.length => 
                    recursiveFilter(pred)(xs.filter(_(index) == binaryRates(index)), index + 1)
                case _ => input
            

        def loop(oxygen:List[BinaryString], carbon: List[BinaryString]):Int = (oxygen, carbon) match
            case (Nil, Nil) => 0
            case (List(o), List(c)) => Integer.parseInt(o, 2) * Integer.parseInt(c, 2)
            case (ox@List(o), cb) => loop(ox, recursiveFilter(binaryRateCounterLeast)(cb, 0))
            case (ox, cb@List(c)) => loop(recursiveFilter(binaryRateCounterMost)(ox, 0), cb)
            case (ox, cb) => loop(recursiveFilter(binaryRateCounterMost)(ox, 0), recursiveFilter(binaryRateCounterLeast)(cb, 0))

        loop(input, input)


@main def powerConsumption = 
    Source.fromFile("./data/day3")
        .getLines
        //.pipe(calculatePowerConsumption)
        .pipe(ls => lifeSupportRating(ls.toList))
        .tap(println)