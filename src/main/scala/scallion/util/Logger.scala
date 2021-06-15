package scallion.util

import scala.collection.mutable.StringBuilder

object Logger {

  val Enabled = true

  private val logs = new StringBuilder()

  def msg(logger: => String, msg: => String): Logger.type = {
    if(Enabled){
      msg.split("\n").foreach(str =>
        logs
          .append(f"<$logger%6s>  ")
          .append(str)
          .append('\n')
      )
    }
    this
  }

  def nl: Logger.type = {
    if(Enabled){
      logs.append('\n')
    }
    this
  }

  override def toString(): String = logs.toString()

  def print: Unit = println(this)

  def clear: Unit = logs.clear()
}