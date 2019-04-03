package org.mockito

import java.lang.reflect.Method
import java.util.function

import org.mockito.internal.handler.ScalaMockHandler.{ArgumentExtractor, Extractors}
import org.mockito.invocation.InvocationOnMock
import org.scalactic.TripleEquals._
import ru.vyarus.java.generics.resolver.GenericsResolver

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.internal.Symbols
import scala.reflect.runtime.{ universe => ru }
import ru._

private[mockito] object ReflectionUtils {

  implicit def symbolToMethodSymbol(sym: Symbol): Symbols#MethodSymbol = sym.asInstanceOf[Symbols#MethodSymbol]

  private val mirror = runtimeMirror(getClass.getClassLoader)
  private val customMirror = mirror.asInstanceOf[{
    def methodToJava(sym: Symbols#MethodSymbol): Method
  }]

  implicit class InvocationOnMockOps(invocation: InvocationOnMock) {
    def returnType: Class[_] = {
      val method         = invocation.getMethod
      val javaReturnType = method.getReturnType

      if (javaReturnType == classOf[Object])
        resolveWithScalaGenerics(method)
          .orElse(resolveWithJavaGenerics(method))
          .getOrElse(javaReturnType)
      else javaReturnType
    }

    private def resolveWithScalaGenerics(method: Method): Option[Class[_]] =
      scala.util
        .Try {
          mirror
            .classSymbol(method.getDeclaringClass)
            .info
            .decls
            .filter(isNonConstructorMethod)
            .find(d => customMirror.methodToJava(d) === method)
            .map(_.asMethod)
            .filter(_.returnType.typeSymbol.isClass)
            .map(methodSymbol => mirror.runtimeClass(methodSymbol.returnType.typeSymbol.asClass))
        }
        .toOption
        .flatten

    private def resolveWithJavaGenerics(method: Method): Option[Class[_]] =
      scala.util.Try {
        GenericsResolver.resolve(invocation.getMock.getClass).`type`(method.getDeclaringClass).method(method).resolveReturnClass()
      }.toOption
  }

  private def isNonConstructorMethod(d: ru.Symbol): Boolean = d.isMethod && !d.isConstructor

  def extraInterfaces[T](implicit $wtt: WeakTypeTag[T], $ct: ClassTag[T]): List[Class[_]] =
    scala.util
      .Try {
        $wtt.tpe match {
          case RefinedType(types, _) =>
            types.map($wtt.mirror.runtimeClass).collect {
              case c: Class[_] if c != clazz($ct) && c.isInterface => c
            }
          case _ => List.empty
        }
      }
      .toOption
      .getOrElse(List.empty)

  def markMethodsWithLazyArgs(clazz: Class[_]): Unit =
    Extractors.computeIfAbsent(
      clazz,
      new function.Function[Class[_], ArgumentExtractor] {
        override def apply(t: Class[_]): ArgumentExtractor =
          scala.util
            .Try {
              ArgumentExtractor {
                mirror
                  .classSymbol(clazz)
                  .info
                  .decls
                  .collect {
                    case s if isNonConstructorMethod(s) =>
                      (customMirror.methodToJava(s), s.typeSignature.paramLists.flatten.zipWithIndex.collect {
                        case (p, idx) if p.typeSignature.toString.startsWith("=>") => idx
                      }.toSet)
                  }
                  .toSeq
                  .filter(_._2.nonEmpty)
              }
            }
            .toOption
            .getOrElse(ArgumentExtractor.Empty)
      }
    )

  def readDeclaredField[T](o: AnyRef, field: String): Option[T] =
    scala.util.Try {
      val f = o.getClass.getDeclaredField(field)
      f.setAccessible(true)
      f.get(o).asInstanceOf[T]
    }.toOption

}
