package org.mockito
package internal.handler

import java.lang.reflect.Method
import java.lang.reflect.Modifier.isAbstract
import java.util.concurrent.ConcurrentHashMap

import org.mockito.ReflectionUtils.readDeclaredField
import org.mockito.internal.handler.ScalaMockHandler._
import org.mockito.internal.invocation.mockref.MockReference
import org.mockito.internal.invocation.{ InterceptedInvocation, MockitoMethod, RealMethod }
import org.mockito.invocation.{ Invocation, MockHandler }
import org.mockito.mock.MockCreationSettings
import org.scalactic.TripleEquals._

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
              unwrapArgs(mockitoMethod, rawArguments.asInstanceOf[Array[Any]])
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

  private def unwrapArgs(method: MockitoMethod, args: Array[Any]): Array[Object] =
    Extractors
      .getOrDefault(method.getJavaMethod.getDeclaringClass, ArgumentExtractor.Empty)
      .transformArgs(method.getJavaMethod, args.asInstanceOf[Array[Any]])
      .asInstanceOf[Array[Object]]

  val Extractors = new ConcurrentHashMap[Class[_], ArgumentExtractor]

  case class ArgumentExtractor(toTransform: Seq[(Method, Set[Int])]) {
    def transformArgs(method: Method, args: Array[Any]): Array[Any] = {
      val transformed = toTransform
        .find(_._1 === method)
        .map(_._2)
        .map { transformIndices =>
          val a: Array[Any] = args.zipWithIndex.flatMap {
            case (arg: Function0[_], idx) if transformIndices.contains(idx) => List(arg())
            case (arg: Iterable[_], idx) if transformIndices.contains(idx)  => arg
            case (arg, _)                                                   => List(arg)
          }
          a
        }
        .getOrElse(args)

      //Due to some scala compiler bug, sometimes the vararg may not be found, so we try the hard way just in case
      unwrapVarargs(transformed)
    }
  }

  object ArgumentExtractor {
    val Empty = ArgumentExtractor(Seq.empty)
  }
}
