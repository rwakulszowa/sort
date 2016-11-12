package idk.yet

//TODO: use a macro

class Logger {

  var logs = List.empty[String]

  def log[T](name: String)
            (block: => T)
            (implicit id: String): T = {
    val res = block
    logs = (s"$id - $name - $res") :: logs
    res
  }

  def time[T](block: => T): T = {
    val start = System.currentTimeMillis
    val res = block
    val totalTime = System.currentTimeMillis - start

    logs = (s"$totalTime ms") :: logs
    res
  }

}
