import scala.io.Source

case class Equation(result: BigInt, terms: List[BigInt])

@main def main = {
  val equations = Source.fromFile("input").getLines.map(line => {
    val Array(resultStr, termsStr) = line.split(": ")
    val terms = termsStr.split(' ').map(BigInt(_)).toList

    Equation(BigInt(resultStr), terms)
  }).toList

  val possiblyTrueEquations = equations.filter(checkEquation)

  println(possiblyTrueEquations.map(_.result).sum)
}

def checkEquation(equation: Equation): Boolean = {
  checkTerms(equation.result, equation.terms.head, equation.terms.tail, 1)
}

def checkTerms(expectedResult: BigInt, accumulator: BigInt, restOfTerms: List[BigInt], depth: Int): Boolean = {
  if (restOfTerms.isEmpty) {
    expectedResult == accumulator
  } else {
    val checkAdd = checkTerms(expectedResult, accumulator + restOfTerms.head, restOfTerms.tail, depth + 1)
    val checkMultiply = checkTerms(expectedResult, accumulator * restOfTerms.head, restOfTerms.tail, depth + 1)
    val checkConcat = checkTerms(expectedResult, BigInt(s"${accumulator}${restOfTerms.head}"), restOfTerms.tail, depth + 1)

    checkAdd || checkMultiply || checkConcat
  }
}
