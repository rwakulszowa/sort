package idk.yet

//TODO: use a macro

abstract class Logger {

  var logs = List.empty[String]

  def log[T](name: String)
            (args: Any*)
            (block: => T)
            (implicit id: String): T

  def dump[T](name: String)
             (args: Any*)
             (block: => T)
             (implicit id: String): T = {
    val res = block
    val arguments = args.toList
    logs = (s"$id - $name - $arguments - $res") :: logs
    res
  }

  def time[T](name: String)
             (args: Any*)
             (block: => T)
             (implicit id: String): T = {
    val start = System.currentTimeMillis
    val res = block
    val totalTime = System.currentTimeMillis - start

    logs = (s"$totalTime ms") :: logs
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
