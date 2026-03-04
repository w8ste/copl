package extras.compilers.general

import extras.compilers.general.Emitter
import modularized.environmental.FullInterpreter.*

import java.nio.file.Files
import scala.language.unsafeNulls

// To not use a real implementation we have a TestEmitter
class TestEmitter extends Emitter {
  override def transpile(
      expr: Expr,
      locals: Map[String, Int],
      functions: Map[String, Fun],
      functionIndices: Map[String, Int]
  ): String =
    // empty implementation
    return ""
}

class FileGenerationTest extends munit.FunSuite {
  test("Teste Generierung eines Files") {
    var testEmitter   = new TestEmitter()
    var content       = "Some content for file generation"
    var testFilePath  = testEmitter.createPath(testEmitter.genFolderAbsPath() + "/testFile.txt")
    val nonValidPaths =
      List(
        // These paths actually work on Linux ...
        // we would need to check those on windows and accordingly only execute the code on windows

        // Paths.get("generated/F?ileGenerationTest/testfile.txt").toAbsolutePath(),
        // Paths.get("generated/FileGenerationTest////testfile.txt").toAbsolutePath(),
        // Paths.get("generated/Fi|leGenerationTest/testfile.txt").toAbsolutePath()
      )

    // Test if file generatd
    assertEquals(testEmitter.generateOutputfile(testFilePath, content), true)
    testEmitter.cleanGeneratedOutputfile(testFilePath)

    for path <- nonValidPaths do {
      var ok = testEmitter.generateOutputfile(path, "content")
      assertEquals(ok, false)
      assertEquals(Files.exists(path), false)
    }
  }
}
