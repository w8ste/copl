package extras.langserver.parser

import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.TillNextWhitespace

case class COPLError(position: (Int, Int), expected: List[String])

abstract class COPLErrorBuilder extends ErrorBuilder[COPLError] {

  /** @inheritdoc */
  type Item = String

  /** @inheritdoc */
  type ExpectedItems = List[String]

  /** @inheritdoc */
  type ExpectedLine = List[String]

  /** @inheritdoc */
  type UnexpectedLine = Option[String]

  /** @inheritdoc */
  type Message = String

  /** @inheritdoc */
  type Messages = List[String]

  /** @inheritdoc */
  type EndOfInput = String

  /** @inheritdoc */
  type Position = (Int, Int)

  /** @inheritdoc */
  type Source = Option[String]

  /** @inheritdoc */
  type ErrorInfoLines = List[String]

  /** @inheritdoc */
  type LineInfo = List[String]

  /** @inheritdoc */
  type Named = String

  /** @inheritdoc */
  type Raw = String

  final val Unknown = "unknown parse error"

  /** @inheritdoc */
  override def combineExpectedItems(alts: Set[COPLErrorBuilder.this.Item]): COPLErrorBuilder.this.ExpectedItems =
    alts.toList

  /** @inheritdoc */
  override def combineMessages(alts: Seq[COPLErrorBuilder.this.Message]): COPLErrorBuilder.this.Messages = alts.toList

  val endOfInput: COPLErrorBuilder.this.EndOfInput = "end of input"

  /** @inheritdoc */
  override def expected(alts: COPLErrorBuilder.this.ExpectedItems): COPLErrorBuilder.this.ExpectedLine = alts

  /** @inheritdoc */
  override def format(
      pos: COPLErrorBuilder.this.Position,
      source: COPLErrorBuilder.this.Source,
      lines: COPLErrorBuilder.this.ErrorInfoLines
  ): COPLError = COPLError(pos, lines)

  /** @inheritdoc */
  override def lineInfo(
      line: String,
      linesBefore: Seq[String],
      linesAfter: Seq[String],
      errorPointsAt: Int,
      errorWidth: Int
  ): COPLErrorBuilder.this.LineInfo = List.concat(linesBefore.toList, List(line, "^" * errorWidth), linesAfter.toList)

  /** @inheritdoc */
  override def message(msg: String): COPLErrorBuilder.this.Message = msg

  def named(item: String): COPLErrorBuilder.this.Named = item

  val numLinesAfter: Int  = 1
  val numLinesBefore: Int = 1

  def pos(line: Int, col: Int): COPLErrorBuilder.this.Position = (line, col)

  def raw(item: String): COPLErrorBuilder.this.Raw = item

  def reason(reason: String): COPLErrorBuilder.this.Message = reason

  def source(sourceName: Option[String]): COPLErrorBuilder.this.Source = sourceName

  def specialisedError(
      msgs: COPLErrorBuilder.this.Messages,
      line: COPLErrorBuilder.this.LineInfo
  ): COPLErrorBuilder.this.ErrorInfoLines =
    if msgs.isEmpty then List(Unknown) ++ line
    else msgs ++ line

  def unexpected(item: Option[COPLErrorBuilder.this.Item]): COPLErrorBuilder.this.UnexpectedLine = item

  def vanillaError(
      unexpected: COPLErrorBuilder.this.UnexpectedLine,
      expected: COPLErrorBuilder.this.ExpectedLine,
      reasons: COPLErrorBuilder.this.Messages,
      line: COPLErrorBuilder.this.LineInfo
  ): COPLErrorBuilder.this.ErrorInfoLines = expected

}

object COPLErrorBuilder extends COPLErrorBuilder with TillNextWhitespace {
  override val trimToParserDemand = true
}

implicit val customErrorBuilder: COPLErrorBuilder = COPLErrorBuilder
