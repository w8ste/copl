package extras.compilers.wasm

import extras.compilers.general.ConsoleLogger
import extras.compilers.wasm.WASMEmitter
import modularized.environmental.FullInterpreter.*

import java.lang.System
import java.nio.file.{Files, Path, Paths}
import scala.Option
import scala.language.unsafeNulls
import scala.sys.process.*
import scala.util.Try

// Use this variable to keep content of generated/ folder
val keepGeneratedFolderContent: Boolean = false

class WASMEmitterTests extends munit.FunSuite {

  // Global variables used in multiple tests
  val wat2wasmInstalled: Boolean = isToolInstalled("wat2wasm")
  val wasmedgeInstalled: Boolean = isToolInstalled("wasmedge")
  var operatingSystem: String    = ""
  val wasmEmitter                = new WASMEmitter()
  val genFolder: String          = wasmEmitter.genFolderAbsPath()

  override def munitIgnore: Boolean = !(wat2wasmInstalled && wasmedgeInstalled)

  // Execution of code one time before all tests
  override def beforeAll(): Unit = {
    // Check OS
    ConsoleLogger.info("Identifying operating system ...")
    operatingSystem = System.getProperty("os.name")
    ConsoleLogger.info(s"Determined OS: $operatingSystem")

    if !(operatingSystem.startsWith("Mac") || operatingSystem.startsWith("Lin")) then {
      throw new Exception("Operating system is not UNIX. Windows is not implemented yet. Skipping WASMEmitterTests.")
    }

    Files.createDirectories(Paths.get(genFolder))

    // Create generated/ folder
    ConsoleLogger.info("Creating generated/ folder ...")
    ConsoleLogger.info(s"Created $genFolder")

  }

  // Execution of code one time after all tests
  override def afterAll(): Unit = {
    keepGeneratedFolderContent match
        case false =>
          // Clean generated/ folder
          Files.list(Paths.get(genFolder).toAbsolutePath()).forEach { path => Files.delete(path) }
        case true =>
          ConsoleLogger.info(s"Skipping deletion of $genFolder")
  }

  test("Test generating simple addition/ subtraction/ multiplication from AST") {

    val expressions: List[(Expr, String)] = List(
      (Add(1, 2), "3"),
      (Add(10, 95), "105"),
      (Sub(2, 4), "4294967294"),
      (Add(2, -4), "4294967294"),
      (Sub(2, 3), "4294967295"),
      (Sub(3, 3), "0"),
      (Add(1, Add(2, 9)), "12"),
      (Add(Sub(Num(17), Num(5)), Num(2)), "14"),
      (Add(Mult(Num(2), Num(3)), Sub(Num(10), Num(4))), "12")
      // more interesting test cases?
    )

    doTestCases(expressions, "add_sub")
  }

  test("Test bindings") {
    val expressions: List[(Expr, String)] = List(
      (Let("x", Add(5, 5), Let("y", Sub("x", 3), Add("y", "y"))), "14"),
      (Let("x", Add(5, 5), Add("x", "x")), "20"),
      (Let("x", Add(-5, -5), Add("x", "x")), "4294967276")
      // add some more complex test cases
    )

    doTestCases(expressions, "bindings")
  }

  test("Test booleans") {
    val expressions: List[(Expr, String)] = List(
      (And(Or(True, False), Not(Eq(Num(3), Num(4)))), "1") // 1 = true
      // add some more complex test cases
    )

    doTestCases(expressions, "bindings")
  }

  test("Test First Order Functions") {
    val expressions: List[(Expr, String)] = List(
      (
        Let(
          "g",
          Fun("n", Sub(Id("n"), Num(1))),
          Let("f", Fun("n", App(Id("g"), Add(Id("n"), Num(5)))), App(Id("f"), Num(5)))
        ),
        "9"
      )
    )

    doTestCases(expressions, "first_order_functions")
  }

  test("Test Recursive Factorial Function") {
    val expressions: List[(Expr, String)] = List(
      (
        Let(
          "factorial",
          Fun(
            "n",
            If0(
              Id("n"),
              Num(1),
              Mult(Id("n"), App(Id("factorial"), Sub(Id("n"), Num(1))))
            )
          ),
          App(Id("factorial"), Num(5))
        ),
        "120"
      )
    )

    doTestCases(expressions, "recursive_factorial")
  }

  test("Test Recursive Fibonacci Function") {
    val expressions: List[(Expr, String)] = List(
      (
        Let(
          "fibonacci",
          Fun(
            "n",
            If0(
              Id("n"),
              Num(0),
              If(
                Eq(Id("n"), Num(1)),
                Num(1),
                Add(
                  App(Id("fibonacci"), Sub(Id("n"), Num(1))),
                  App(Id("fibonacci"), Sub(Id("n"), Num(2)))
                )
              )
            )
          ),
          App(Id("fibonacci"), Num(10))
        ),
        "55"
      )
    )

    doTestCases(expressions, "recursive_fibonacci")
  }

  test("Test Higher Order Functions".ignore) { // Doesn't quite work yet
    val expressions: List[(Expr, String)] = List(
      (
        Let(
          "increment",
          Fun("n", Add(Id("n"), Num(1))),
          Let(
            "applyFunc",
            Fun("f", Fun("x", App(Id("f"), Id("x")))),
            App(App(Id("applyFunc"), Id("increment")), Num(5))
          )
        ),
        "6"
      )
    )

    doTestCases(expressions, "higher_order_functions")
  }

  def doTestCases(expressions: List[(Expr, String)], testName: String): Unit = {
    var fileCounter = 0
    for (expr, exprResult) <- expressions do
        val watCode      = wasmEmitter.generateWat(expr)
        val watFilePath  = wasmEmitter.createPath(genFolder + s"/${testName}_generation_$fileCounter.wat")
        val wasmFilePath = genFolder + s"/${testName}_generation_$fileCounter.wasm"
        assertEquals(wasmEmitter.generateOutputfile(watFilePath, watCode), true)

        if operatingSystem.startsWith("Mac") || operatingSystem.startsWith("Lin") then {
          // 1. generate wasm from wat (wat2wasm)
          var exitCode = runWat2Wasm(watFilePath, wasmFilePath)
          if exitCode != 0 then {
            ConsoleLogger.error(s"wat2wasm failed with exit code $exitCode")
            exitCode
          }

          val executionResult = runWasmEdge(watFilePath, wasmFilePath)
          if executionResult(0) != 0 then {
            ConsoleLogger.error(s"wasmEdge failed with exit code $exitCode")
          }

          // 2. check if generated .wasm file outputs expected result
          executionResult(1) match {
            case Some(output) =>
              assertEquals(output.trim, exprResult)
            case None =>
              ConsoleLogger.error("Failed to capture process output")
          }
        }
        fileCounter = fileCounter + 1
        // TODO: Add Windows

  }

  def isToolInstalled(tool: String): Boolean = {
    Try {
      val exitCode = Process(Seq(tool, "--version")).!
      exitCode == 0
    }.getOrElse(false)
  }

  def runWat2Wasm(watFilePath: Path, wasmFilePath: String): Integer = {
    val process  = Process(s"wat2wasm $watFilePath -o $wasmFilePath")
    var exitCode = process.!
    assertEquals(exitCode, 0)
    return exitCode
  }

  // returns (exitcode, Command Line Output)
  def runWasmEdge(watFilePath: Path, wasmFilePath: String): (Integer, Option[String]) = {
    val process  = Process(s"wasmedge $wasmFilePath main")
    var exitCode = process.!
    assertEquals(exitCode, 0)
    val cmdLineOutput = Try(process.!!).toOption
    return (exitCode, cmdLineOutput)
  }
}
