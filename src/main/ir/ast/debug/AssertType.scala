package ir.ast.debug

import ir.Type

/**
  * assertType checks if the given type is the same as the type of its parameter.
  */
case class AssertType(expectedType: Type, name: String = "") extends
  TypeOperator(actualType => {
    try {
      assert(expectedType.equals(actualType))
    } catch {
      case e: java.lang.AssertionError =>
        System.err.println("AssertType for \"" + name + "\" failed.\nExpected type:\n" +
          expectedType.toString.replace("),", "),\n") +
          "\nActual type:\n" + actualType.toString.replace("),", "),\n"))
        throw e
    }
  })
