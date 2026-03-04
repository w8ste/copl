package extras.compilers.general

import scala.Console.{BLUE, RED, RESET, YELLOW}

object ConsoleLogger {
  def info(output: String): Unit = println(s"${BLUE}${}[INFO] $output ${RESET}")

  def warning(output: String): Unit = println(s"${YELLOW}${}[WARNING] $output ${RESET}")

  def error(output: String): Unit = println(s"${RED}${}[ERROR] $output ${RESET}")
}
