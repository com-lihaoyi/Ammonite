package ammonite.repl.api

import ammonite.interp.api.APIHolder
import ammonite.repl.tools.Desugared
import ammonite.util.CodeColors

case class Location(fileName: String, lineNum: Int, fileContent: String)

trait SourceAPI {

  def browseSourceCommand(targetLine: Int) = Seq("less", "+" + targetLine,"-RMN")

  def failLoudly[T](res: Either[String, T]): T = res match{
    case Left(s) => throw new Exception(s)
    case Right(r) => r
  }

  def loadObjectMemberInfo(symbolOwnerCls: Class[_],
                           value: Option[Any],
                           memberName: String,
                           returnType: Class[_],
                           argTypes: Class[_]*): Either[String, Location]

  def loadObjectInfo(value: Any): Either[String, Location]

  def browseObjectMember(symbolOwnerCls: Class[_],
                         value: Option[Any],
                         memberName: String,
                         pprinter: pprint.PPrinter,
                         colors: CodeColors,
                         command: Int => Seq[String],
                         returnType: Class[_],
                         argTypes: Class[_]*): Any

  def browseObject(value: Any,
                   pprinter: pprint.PPrinter,
                   colors: CodeColors,
                   command: Int => Seq[String]): Any

  def desugarImpl(s: String)(implicit colors: ammonite.util.CodeColors): Desugared

}

object SourceBridge extends APIHolder[SourceAPI]

