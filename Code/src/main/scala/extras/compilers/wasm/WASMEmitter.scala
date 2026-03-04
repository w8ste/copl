package extras.compilers.wasm

import extras.compilers.general.Emitter
import modularized.environmental.FullInterpreter.*

class WASMEmitter extends Emitter {
  private var localCounter    = 0
  private var functionCounter = 0

  private def freshLocal(): Int = {
    val index = localCounter
    localCounter += 1
    index
  }

  // Keygenerator for map
  private def freshFunction(): String = {
    val name = s"func_$functionCounter"
    functionCounter += 1
    name
  }

  private def collectFunctions(expr: Expr): Map[String, Fun] = expr match {
    case Let(boundId, Fun(param, body), boundBody) =>
      Map(boundId -> Fun(param, body)) ++ collectFunctions(boundBody)
    case Let(_, namedExpr, boundBody) =>
      collectFunctions(namedExpr) ++ collectFunctions(boundBody)
    case Fun(_, body)          => collectFunctions(body)
    case App(funExpr, argExpr) =>
      collectFunctions(funExpr) ++ collectFunctions(argExpr)
    case _ => Map.empty
  }

  override def transpile(
      expr: Expr,
      locals: Map[String, Int] = Map.empty,
      functions: Map[String, Fun] = Map.empty,
      functionIndices: Map[String, Int] = Map.empty
  ): String = expr match {
    case Num(n)         => s"(i32.const $n)"
    case Add(lhs, rhs)  => s"(i32.add ${transpile(lhs, locals, functions)} ${transpile(rhs, locals, functions)})"
    case Sub(lhs, rhs)  => s"(i32.sub ${transpile(lhs, locals, functions)} ${transpile(rhs, locals, functions)})"
    case Mult(lhs, rhs) => s"(i32.mul ${transpile(lhs, locals, functions)} ${transpile(rhs, locals, functions)})"
    case If0(test, thenBody, elseBody) =>
      s"""
      (if (result i32) (i32.eqz ${transpile(test, locals, functions)})
        (then ${transpile(thenBody, locals, functions)})
        (else ${transpile(elseBody, locals, functions)}))
      """
    case True          => "(i32.const 1)"
    case False         => "(i32.const 0)"
    case And(lhs, rhs) => s"(i32.and ${transpile(lhs, locals, functions)} ${transpile(rhs, locals, functions)})"
    case Or(lhs, rhs)  => s"(i32.or ${transpile(lhs, locals, functions)} ${transpile(rhs, locals, functions)})"
    case Not(e)        => s"(i32.eqz ${transpile(e, locals, functions)})"
    case Eq(lhs, rhs)  => s"(i32.eq ${transpile(lhs, locals, functions)} ${transpile(rhs, locals, functions)})"
    case If(test, thenBody, elseBody) =>
      s"""
      (if (result i32) (i32.ne (i32.const 0) ${transpile(test, locals, functions)})
        (then ${transpile(thenBody, locals, functions)})
        (else ${transpile(elseBody, locals, functions)}))
      """
    case Fun(param, body) =>
      val funcName = freshFunction()
      val bodyWat  = transpile(body, locals + (param -> 0), functions + (funcName -> Fun(param, body)))
      s"""
      (func $$${funcName} (param $$${param} i32) (result i32)
        $bodyWat
      )
      """
    case App(funExpr, argExpr) =>
      val argWat = transpile(argExpr, locals, functions, functionIndices)
      funExpr match {
        case Id(name) if functions.contains(name) =>
          s"(call $$${name} $argWat)"
        case Id(name) if locals.contains(name) =>
          s"""
          (call_indirect (type $$func_type)
            $argWat
            (local.get ${locals(name)}))
          """
        case _ =>
          val funWat = transpile(funExpr, locals, functions, functionIndices)
          s"""
          (call_indirect (type $$func_type)
            $argWat
            $funWat)
          """
      }
    case Id(name) =>
      locals.get(name) match {
        case Some(index) => s"(local.get $index)"
        case None        =>
          functionIndices.get(name) match {
            case Some(index) => s"(i32.const $index)"
            case None        => throw new RuntimeException(s"Unbound variable or function: $name")
          }
      }
    case Let(boundId, namedExpr, boundBody) =>
      namedExpr match {
        case Fun(_, _) =>
          val bodyWat = transpile(boundBody, locals, functions, functionIndices)
          bodyWat
        case _ =>
          val localIndex   = freshLocal()
          val namedExprWat = transpile(namedExpr, locals, functions, functionIndices)
          val newLocals    = locals + (boundId -> localIndex)
          val boundExprWat = transpile(boundBody, newLocals, functions, functionIndices)
          s"""
          (local.set $localIndex $namedExprWat)
          $boundExprWat
          """
      }
    case _ => throw new RuntimeException(s"Unsupported expression: $expr")
  }

  def generateWat(expr: Expr): String = {
    localCounter = 0
    functionCounter = 0

    val functions       = collectFunctions(expr)
    val functionIndices = functions.keys.zipWithIndex.toMap

    val funDefsWat = functions.map { case (funName, Fun(param, body)) =>
      val bodyWat = transpile(body, Map(param -> 0), functions, functionIndices)
      s"""
      (func $$${funName} (param $$${param} i32) (result i32)
        $bodyWat
      )
      """
    }.mkString("\n")

    val mainBody   = transpile(expr, Map.empty, functions, functionIndices)
    val localCount = localCounter

    s"""
    (module

      (type $$func_type (func (param i32) (result i32)))
      (table ${functions.size + 1} funcref)
      (elem (i32.const 0) ${functions.keys.map(name => s"$$${name}").mkString(" ")})

      $funDefsWat

      (func $$main (result i32)
        (local ${List.fill(localCount)("i32").mkString(" ")})
        $mainBody
      )
      (export "main" (func $$main))
    )
    """
  }
}
