package org.mockito
package internal.handler

import java.lang.reflect.Method
import java.lang.reflect.Modifier.isAbstract
import java.util.concurrent.ConcurrentHashMap

import org.mockito.ReflectionUtils.readDeclaredField
import org.mockito.internal.handler.ScalaMockHandler._
import org.mockito.internal.invocation.mockref.MockReference
import org.mockito.internal.invocation.{ InterceptedInvocation, MockitoMethod, RealMethod }
import org.mockito.internal.progress.ThreadSafeMockingProgress.mockingProgress
import org.mockito.invocation.{ Invocation, MockHandler }
import org.mockito.mock.MockCreationSettings
import org.scalactic.TripleEquals._
import collection.JavaConverters._

class ScalaMockHandler[T](mockSettings: MockCreationSettings[T]) extends MockHandlerImpl[T](mockSettings) {

  override def handle(invocation: Invocation): AnyRef =
    if (invocation.getMethod.getName.contains("$default$") && !isAbstract(invocation.getMethod.getModifiers))
      invocation.callRealMethod()
    else {
      val scalaInvocation = invocation match {
        case i: InterceptedInvocation =>
          val scalaInvocation = for {
            mockitoMethod <- i.mockitoMethod
            mockRef       <- i.mockRef
            realMethod    <- i.realMethod
            rawArguments = i.getRawArguments
            arguments = if (rawArguments != null && rawArguments.nonEmpty && !isCallRealMethod)
              unwrapArgs(mockitoMethod.getJavaMethod, rawArguments.asInstanceOf[Array[Any]])
            else rawArguments
          } yield new ScalaInvocation(mockRef, mockitoMethod, arguments, rawArguments, realMethod, i.getLocation, i.getSequenceNumber)
          scalaInvocation.getOrElse(invocation)
        case other => other
      }
      super.handle(scalaInvocation)
    }
}

object ScalaMockHandler {
  def apply[T](mockSettings: MockCreationSettings[T]): MockHandler[T] =
    new InvocationNotifierHandler[T](new ScalaNullResultGuardian[T](new ScalaMockHandler(mockSettings)), mockSettings)

  implicit class InterceptedInvocationOps(i: InterceptedInvocation) {
    def mockitoMethod: Option[MockitoMethod]   = readDeclaredField(i, "mockitoMethod")
    def mockRef: Option[MockReference[Object]] = readDeclaredField(i, "mockRef")
    def realMethod: Option[RealMethod]         = readDeclaredField(i, "realMethod")
  }

  private def isCallRealMethod: Boolean =
    (new Exception).getStackTrace.toList.exists { t =>
      t.getClassName == "org.mockito.internal.handler.ScalaInvocation" &&
      t.getMethodName == "callRealMethod"
    }

  private def unwrapArgs(method: Method, args: Array[Any]): Array[Object] = {
    val transformed = Extractors.asScala.values.flatten
      .find {
        case (cls, mtd, _) => method.getDeclaringClass.isAssignableFrom(cls) && method === mtd
      }
      .map(_._3)
      .map { transformIndices =>
        val matchers = mockingProgress().getArgumentMatcherStorage.pullLocalizedMatchers().asScala.toIterator
        val a: Array[Any] = args.zipWithIndex.flatMap {
          case (arg: Function0[_], idx) if transformIndices.contains(idx) =>
            List(arg())
          case (arg: Iterable[_], idx) if transformIndices.contains(idx) =>
            arg.foreach(_ => if (matchers.nonEmpty) mockingProgress().getArgumentMatcherStorage.reportMatcher(matchers.next().getMatcher))
            arg
          case (arg, _) =>
            if (matchers.nonEmpty) mockingProgress().getArgumentMatcherStorage.reportMatcher(matchers.next().getMatcher)
            List(arg)
        }
        a
      }
      .getOrElse(args)

    unwrapVarargs(transformed).asInstanceOf[Array[Object]]
  }

  val Extractors = new ConcurrentHashMap[Class[_], Seq[(Class[_], Method, Set[Int])]]
}
