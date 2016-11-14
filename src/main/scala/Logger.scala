package idk.yet


abstract class Log {

  override def toString = this match {
    case TimeLog(id, name, args, time) => id :: name :: time :: Nil mkString "\n"
    case ResultLog(id, name, args, result) => id :: name :: (args mkString "\n") :: result :: Nil mkString "\n"
  }

}

case class TimeLog(id: String, name: String, args: Seq[Any], time: Long) extends Log
case class ResultLog(id: String, name: String, args: Seq[Any], result: Any) extends Log


abstract class Logger {

  var logs = List.empty[Log]

  def log[T](name: String)
            (args: Any*)
            (block: => T)
            (implicit id: String): T

  def dump[T](name: String)
             (args: Any*)
             (block: => T)
             (implicit id: String): T = {
    val res = block
    logs = ResultLog(id, name, args, res) :: logs
    res
  }

  def time[T](name: String)
             (args: Any*)
             (block: => T)
             (implicit id: String): T = {
    val start = System.currentTimeMillis
    val res = block
    val totalTime = System.currentTimeMillis - start
    logs = TimeLog(id, name, args, totalTime) :: logs
    res
  }

}


trait Loggable {

  val logger: Logger

  implicit val id = ""

}


trait VerboseLoggable extends Loggable {

  val logger = new Logger {

    def log[T](name: String)
              (args: Any*)
              (block: => T)
              (implicit id: String): T =
      dump(name)(args)(block)(id)

  }

}


trait OptimizedLoggable extends Loggable {

  val logger = new Logger {

    def log[T](name: String)
              (args: Any*)
              (block: => T)
              (implicit id: String): T =
      time(name)(args)(block)(id)
  }

}
