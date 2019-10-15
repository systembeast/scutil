package com.sysbeast.scutil

import java.io.File
import java.lang.reflect.Modifier
import java.util.jar.JarFile

import com.sysbeast.scutil.FileUtil._

import scala.jdk.CollectionConverters._

object ClassUtil {
  implicit class ClassUtilStringHelper(s: String) {
    def loadClass = {
      try {
        Some(Class.forName(s))
      } catch {
        case e: Throwable =>
          println("load failed " + s)
          e.printStackTrace()
          None
      }
    }
  }



  def findClasses[T](cls: Class[T], loc: File):  List[Class[_ <:T]] = {
    println("findClasses ", loc)
    if(loc.isDirectory)
      findClasses(cls, loc.deepFindExt(".class").map(_.getAbsolutePath).map(_.substring(loc.getAbsolutePath.length + 1)).map(_.dropRight(".class".length)).map(_.replace('/', '.')).map(_.replace('\\','.')))
    else
      findClasses(cls, new JarFile(loc))
  }

  def findClasses[T](cls: Class[T], jar: JarFile):  List[Class[_ <:T]] = findClasses(cls,jar.entries().asScala.toList.map(_.getName).map(_.replace('/','.')).filter(_.endsWith(".class")).map(_.dropRight(".class".length)))
  def findClasses[T](cls: Class[T], names: List[String]):  List[Class[_ <:T]] = names
    .filterNot(_.contains("module-info"))
    .filterNot(_.contains("org.eclipse"))
    .map(_.loadClass)
    .filter(_.isDefined)
    .map(_.get)
    .filter(c=> !Modifier.isAbstract(c.getModifiers) )
    .filter(c=> cls.isAssignableFrom(c)  )
    .map(_.asSubclass(cls))
}