package extras.compilers.general

import modularized.environmental.FullInterpreter.*

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{FileAlreadyExistsException, Files, Path, Paths}

trait Emitter {
  def transpile(
      expr: Expr,
      locals: Map[String, Int],
      functions: Map[String, Fun],
      functionIndices: Map[String, Int]
  ): String
  def generateOutputfile(outputFilePath: Path, content: String): Boolean = {
    try {
      // If outputFilePath  is a normal file we check if we the filesystem contains the directory
      // to create the file in, if not we create it recursively
      if !Files.exists(outputFilePath) then {
        // create parent directory if necessary
        Files.createDirectories(outputFilePath.getParent())
        Files.createFile(outputFilePath)
        ConsoleLogger.info(s"Genrated file $outputFilePath")
      } else {
        ConsoleLogger.warning("Tried to generate file but it is already generated! No overwriting!")
      }
    } catch {
      case err: UnsupportedOperationException => ConsoleLogger.error("UnsupportedOperationException")
      case err: FileAlreadyExistsException    => ConsoleLogger.error("File already exists")
      case err: IOException                   => ConsoleLogger.error(s"IOExcption!${err}")
      case err: SecurityException             => ConsoleLogger.error("SecurityException!")
    }

    try
      Files.write(outputFilePath.toAbsolutePath(), content.getBytes(StandardCharsets.UTF_8))
    catch {
      case err: UnsupportedOperationException => ConsoleLogger.error("UnsupportedOperationException")
      case err: SecurityException             => ConsoleLogger.error("SecurityException!")
      case err: IOException                   => ConsoleLogger.error(s"IOExcption!${err}")
    }

    if content.isEmpty then {
      ConsoleLogger.warning("About to write Outputfile with empty content!")
    }

    return Files.exists(outputFilePath)
  }

  def cleanGeneratedOutputfile(fileToDelete: Path): Boolean = {
    if Files.exists(fileToDelete) then {
      ConsoleLogger.info("Deleting " + fileToDelete.toAbsolutePath())
      Files.delete(fileToDelete)
      return !Files.exists(fileToDelete)
    }
    ConsoleLogger.info("Emitter.cleanGeneratedOutputfile(): No file to delete")
    return false
  }

  def createPath(filePath: String): Path =
    return Paths.get(filePath).toAbsolutePath()

  // Returns Absolute path to a folder called generated in project hierarchy
  def genFolderAbsPath(): String =
    Paths.get("target/generated").toAbsolutePath().toString()
}
