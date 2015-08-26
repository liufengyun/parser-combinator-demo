import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import java.io.StringReader
import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case object True extends Term
case object False extends Term
case class Var(id: String) extends Term
case class Or(t1: Term, t2: Term) extends Term
case class And(t1: Term, t2: Term) extends Term
case class Not(t: Term) extends Term

object Parser extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "and", "or")
  lexical.delimiters ++= List("!", "(", ")")

  /** precedence: not > and > or
    *
    * associativity:
    *      not : right
    *      and : left
    *      or  : left
    *
    * Expr ::=
    *   | 'true'
    *   | 'false'
    *   | ident
    *   | Expr 'and' Expr
    *   | Expr 'or' Expr
    *   | '!'Expr
    *   | '(' Expr ')'
    */
  def Expr: Parser[Term] = (
        OrExpr
      | AndExpr
      | "(" ~> Expr <~ ")"
      | "true" ^^^ True
      | "false" ^^^ False
      | ident ^^ Var
      | NotExpr
      | failure("illegal start of expression"))

  def OrExpr: Parser[Term] = {
    val operand = (
          AndExpr
        | "(" ~> Expr <~ ")"
        | "true" ^^^ True
        | "false" ^^^ False
        | ident ^^ Var
        | NotExpr
    )

    chainl1( operand, "or" ^^^ Or)
  }

  def AndExpr: Parser[Term] = {
    val operand = (
          "("~> Expr <~ ")"
        | "true" ^^^ True
        | "false" ^^^ False
        | ident ^^ Var
        | NotExpr
    )

    chainl1( operand, "and" ^^^ And)
  }

  def NotExpr: Parser[Term] = "!"~> (
      "("~> Expr <~ ")"
    | "true" ^^^ True
    | "false" ^^^ False
    | ident ^^ Var
    | NotExpr
  )^^Not

  def parse(input: Reader[Char]) = {
    val tokens = new lexical.Scanner(input)
    phrase(Expr)(tokens)
  }

  def parseString(string: String) = {
    parse(StreamReader(new StringReader(string)))
  }

  def demo = {
    def test( expr: String) = {
      println("\ninput: " + expr)
      parseString(expr) match {
        case Success(tree, _) =>
          println("output: " + tree)
        case failure @ NoSuccess(_, _) =>
          println(failure)
      }
    }

    test("true")
    test("false")
    test("a")
    test("a or b")
    test("a and b")
    test("!a")
    test("a or b or c")
    test("a or b and c")
    test("a and b or a and c")
    test("!a or !b")
    test("a or (b or c)")
    test("a and b and c")
    test("a and b or c")
    test("a and (b and c)")
    test("!a and !b")
  }

  def main(args: Array[String]): Unit = {
    demo

    println("\ninput a test expression:")
    val input = java.lang.System.console().readLine()

    parseString(input) match {
      case Success(tree, _) =>
        println(tree)
      case failure @ NoSuccess(_, _) =>
        println(failure)
    }
  }
}
