case class LogConf(ident: Int, app: String)

object Logs:
  def logError(msg: String)(using conf: LogConf): Unit =
    val identStr = " " * conf.ident
    println(s"$identStr[ERROR] [App:${conf.app}] $msg")

  def logInfo(msg: String)(using conf: LogConf): Unit =
    val identStr = " " * conf.ident
    println(s"$identStr[INFO] [App:${conf.app}] $msg")

object GivenUsing extends App:
  import Logs.*

  given LogConf(4, "Demo")

  logInfo("Loading app")
  logInfo("Loading conf")
  logError("Ups...")