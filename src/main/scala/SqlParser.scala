package zio.sql

import com.github.ghik.silencer.silent
import org.spartanz.parserz._

object SqlParser extends Sql with App {

  import Column._
  
  val columnsString = "id int,name varchar,legendary boolean"
  val tableString   = s"create table pokemons($columnsString)"

  val columns = int("id") :*: string("name") :*: boolean("legendary") :*: ColumnSet.Empty
  val table = columns.table("pokemons")

  object Parser extends ParsersModule {
    override type Input = String
  }

  import Parser._
  import Parser.Expr._
  import Parser.Grammar._

  type S    = Unit
  type E    = String
  type G[A] = Grammar[S, S, E, A]

  //TODO
  //1. handle empty column set
  //2. handle whitespaces: ' ', '\n', etc.
  //3. handle uppercase/lowercase
  //4. handle digits in column types: int2, int4, int8, etc.
  //5. pretty print the resulting string

  private val `(` = '('
  private val `)` = ')'
  private val `,` = ','
  private val ` ` = ' '
  private val createKw = "create".toList.asInstanceOf[::[Char]]
  private val tableKw  = "table".toList.asInstanceOf[::[Char]]

  val char: G[Char] = "char" @@ consumeOption("expected: char")(
    s => s.headOption.map(s.drop(1) -> _),
    { case (s, c) => Some(s + c.toString) }
  )

  val alpha: G[Char]    = char.filter("expected: alphabetical")(cond(_.isLetter)).tag("alpha")
  val comma: G[Char]    = char.filter("expected: comma")(===(`,`))
  val paren1: G[Char]   = char.filter("expected: open paren")(===(`(`))
  val paren2: G[Char]   = char.filter("expected: close paren")(===(`)`))
  val space: G[Char]    = char.filter("expected: space")(===(` `))
  val kwCreate: G[::[Char]] = alpha.rep1.filter("expected keyword: CREATE")(===(createKw))
  val kwTable: G[::[Char]] =  alpha.rep1.filter("expected keyword: TABLE")(===(tableKw))

  val name: G[String] = "name" @@ alpha.rep1.mapOption("name cannot be empty")(
    s => Some(s.mkString), 
    _.toList match {
      case c1 :: rest if c1.isLetter => Some(::(c1, rest))
      case _                         => None
    }
  )

  val typee: G[String] = "type" @@ alpha.rep1.mapOption("type cannot be empty")(
    s => Some(s.mkString),
    _.toList match {
      case c1 :: rest if c1.isLetter => Some(::(c1, rest))
      case _                         => None
    }
  )

  val column: G[Column[_]] = "column" @@ (name ~ space ~ typee).map(
    { case ((n, _), t) => t.toLowerCase match {
                            case "bigint" | "int8" => Column.long(n)
                            case "boolean"         => Column.boolean(n)
                            case "int" | "int4"    => Column.int(n)
                            case "varchar"         => Column.string(n)
                          }
    },
    { case c@Column(n) => ((n, ` `), c.typeTag match {
                            case TypeTag.TBoolean => "boolean"
                            case TypeTag.TInt     => "int"
                            case TypeTag.TLong    => "bigint"
                            case TypeTag.TString  => "varchar"
                          })
    }
  )

  lazy val columnSet: G[ColumnSet] = "columnSet" @@ (column ~ ((comma, `,`) ~> column).rep).map({
    case (e1, en) => e1 :*: en.foldLeft[ColumnSet](ColumnSet.Empty) { case (a, el) => el :*: a }
  }, {
    case e => (e.columnsUntyped.head, e.columnsUntyped.tail)
  })

  //change Table to Table.Source and get rid of the ColumnSet.Empty case
  //to consider: PostgreSQL allows creating tables with no columns for partitioning, 
  //  but this requires a change to Table model. Probably not something ZIO SQL needs to support.
  @silent
  val tab: G[Table] = "table" @@ (kwCreate ~ space ~ kwTable ~ space ~ name ~ ((paren1, `(`) ~> columnSet <~ (`)`, paren2))).map(
    { case ((_, name), exp) => exp match {
                                  case ColumnSet.Empty => throw new IllegalArgumentException("Column set can't be empty")
                                  case c: ColumnSet.Cons[_, _] => c.table(name)
                                } },
    { case t => t match {
                  case ts: Table.Source[_, _] => (((((createKw, ` `), tableKw), ` `), ts.name), ts.columnsUntyped.reverse.foldLeft[ColumnSet](ColumnSet.Empty) { case (a, c) => c :*: a })
                  case _ => throw new IllegalArgumentException("Column set can't be empty")
                } }
  )

  val parser: (Unit, Input) => (Unit, String \/ (Input, Table))  = Parser.parser(tab)
  val printer: (Unit, (Input, Table)) => (Unit, String \/ Input) = Parser.printer(tab)
  val description: List[String]                                  = Parser.bnf(tab)

  //example
  @silent
  val Right((_, r)) = parser((), tableString)._2

  @silent
  val (_, Right(p)) = printer((), ("", table))

  println(r == table) //false, why?
  println(p == tableString)
  println(r)
  println(p)
}
