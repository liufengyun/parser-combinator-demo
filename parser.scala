import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import java.io.StringReader

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

/** Expr ::= 'true'
    | 'false'
    | 'if' Expr 'then' Expr 'else' Expr
    | '0'
    | 'succ' Expr
    | 'pred' Expr
    | 'iszero' Expr
  */


//   ... To complete ...
case object True extends Term
case object False extends Term
case object Zero extends Term
case class IsZero(t: Term) extends Term
case class Pred(t: Term) extends Term
case class Succ(t: Term) extends Term
case class If(pred: Term, opt: Term, other: Term) extends Term

object Parser extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")

  import lexical.NumericLit

  def numericLitSugar(n: Int): Term = if (n == 0) Zero else Succ(numericLitSugar(n - 1))

    /** Expr ::= 'true'
      | 'false'
      | 'if' Expr 'then' Expr 'else' Expr
      | '0'
      | 'succ' Expr
      | 'pred' Expr
      | 'iszero' Expr
      */
  def Expr: Parser[Term] = (
    //   ... To complete ...
    "true" ^^^ True
      | "false" ^^^ False
      | "zero" ^^^ Zero
      | numericLit ^^ (n => numericLitSugar(n.toInt))
      | "if" ~ Expr ~ "then" ~ Expr ~ "else" ~ Expr ^^ { case "if" ~ pred ~ "then" ~ opt ~ "else" ~ other => If(pred, opt, other) }
      | "iszero" ~> Expr ^^ (IsZero(_))
      | "succ" ~> Expr ^^ (Succ(_))
      | "pred" ~> Expr ^^ (Pred(_))
      | failure("illegal start of expression"))

  def parse(input: Reader[Char]) = {
    val tokens = new lexical.Scanner(input)
    phrase(Expr)(tokens)

  }

  def parseString(string: String) = {
    parse(StreamReader(new StringReader(string)))
  }

  def main(args: Array[String]): Unit = {
    // val input = StreamReader(new java.io.InputStreamReader(System.in))
    val input = java.lang.System.console().readLine()

    parseString(input) match {
      case Success(tree, _) =>
        println(tree)
      case failure @ NoSuccess(_, _) =>
        println(failure)
    }
  }
}
