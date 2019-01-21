import scala.util.parsing.combinator.RegexParsers

// Parser combinators solution inspired by the examples
object Sgf extends RegexParsers {

  type Tree[A] = Node[A] // to separate the type from the constructor, cf. Haskell's Data.Tree
  type Forest[A] = List[Tree[A]]
  case class Node[A](rootLabel: A, subForest: Forest[A] = List())

  // A tree of nodes.
  type SgfTree = Tree[SgfNode]

  // A node is a property list, each key can only occur once.
  // Keys may have multiple values associated with them.
  type SgfNode = Map[String, List[String]]

  override val skipWhitespace = false

  def parseSgf(text: String): Option[SgfTree] = {
    // (;FF[4];AB[aa][ab][ba])
    // (;FF[4](;B[aa];W[ab])(;B[dd];W[ee]))
    // (;FF[4]C[root]SZ[19];B[aa];W[ab])

    parseAll(gameTree, text)
      .map(Some(_))
      .getOrElse(None)
  }

  private val gameTree: Parser[SgfTree] =
    ("(" ~ rep1(node) ~ rep(gameTree) ~ ")") ^^ {
      case _ ~ (rootNode::singleChildNodes) ~ multipleChildNodes ~ _ =>
        val subNodeForest: List[SgfTree] = singleChildNodes.map( Node(_))
        Node(rootNode, subNodeForest ++ multipleChildNodes)
    }

  // SgfNode is either an sgfProperty or emptyNode
  // emptyNode is Map()
  // sgfProperty is Map(key -> values) where key is propKey and values are propValues
  // propValues are a repeated non empty propValue
  //  propValue is a repeated valueParser inside of "[]"
  // valueParser defines how to extract a value
  private val node: Parser[SgfNode] =
    ";" ~ (sgfProperty | emptyNode) ^^ {
      case _ ~ node => node
    }

    private val emptyNode: Parser[SgfNode] =
     "" ^^ (_ => Map())

    private def sgfProperty: Parser[SgfNode] =
      propKey ~ propValues ^^ {
        case key ~ values => Map(key -> values)
      }

    private val propKey: Parser[String] = "[A-Z]".r
    private val propValues: Parser[List[String]] = rep1(propValue)
    private val propValue: Parser[String] =
    "[" ~ rep1(valueParser) ~ "]" ^^ {
      case _ ~ values ~ _ => values mkString
    }

  private val valueParser: Parser[String] = {
    implicit class AsStringParser(self: String) { def p: Parser[String] = self }

    val escapedNewline: Parser[String] = "\\\n".p ^^ (_ => "") // ignore newline
    val escapedChar: Parser[String] = """\\.""".r ^^ (_.takeRight(1)) // take the escaped char
    val whitespace: Parser[String] = """\s""".r ^^ (_ => " ") // don't ignore whitespace
    val ident: Parser[String] = "[^]]".r

    escapedNewline | escapedChar | whitespace | ident
  }
}
