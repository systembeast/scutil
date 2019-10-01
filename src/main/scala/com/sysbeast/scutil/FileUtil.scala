package com.sysbeast.scutil

import java.io._
import java.security.MessageDigest
import java.util.Properties

import scala.collection.mutable
import scala.io.Source

object FileUtil {
  implicit class FileUtilHelper(f: File) {
    def loadProperties(): Properties = {
      val ps = new Properties()
      val is = new FileInputStream(f)
      ps.load(is)
      is.close()
      ps
    }
    def save(ps: Properties): Unit = {
      val os = new FileOutputStream(f)
      ps.store(os,"by imsg");
      os.close()
    }
    def saveText(text: String): Unit = {
      val ps = new PrintStream(f)
      ps.print(text)
      ps.flush()
      ps.close()
    }
    def asText: String = {
      val src = Source.fromFile(f, "utf-8")
      val code = src.getLines().toList.mkString("\n")
      src.close()
      code
    }
    def empty = {
      if(f.isDirectory) {
        val subs = f.listFiles()
        if(subs!=null) {
          subs.foreach(s => s.rmrf)
        }
      }
    }
    def rmrf : Unit= {
      if(f.isFile) f.delete()
      else {
        val subs  = f.listFiles()
        if(subs!=null) {
          subs.foreach(_.rmrf)
          f.delete()
        }
      }
    }
    def moveTo(dst: File): File = {
      dst.getParentFile.mkdirs()
      val is = new FileInputStream(f)
      val os = new FileOutputStream(dst)
      var done = false
      var to_read = f.length()
      while(!done && to_read > 0) {
        val buf = new Array[Byte](1024)
        val r = is.read(buf, 0, 1024)
        if(r > 0) {
          os.write(buf, 0, r)
          to_read -= r
        } else {
          done = true
        }
      }
      os.close()
      is.close()
      if(to_read > 0) {
        dst.delete()
        throw new IOException("move broken")
      } else {
        f.delete()
      }
      dst
    }
    def copyTo(dst: File): File = {
      dst.getParentFile.mkdirs()
      val is = new FileInputStream(f)
      val os = new FileOutputStream(dst)
      var done = false
      var to_read = f.length()
      while(!done && to_read > 0) {
        val buf = new Array[Byte](1024)
        val r = is.read(buf, 0, 1024)
        if(r > 0) {
          os.write(buf, 0, r)
          to_read -= r
        } else {
          done = true
        }
      }
      os.close()
      is.close()
      if(to_read > 0) {
        dst.delete()
        throw new IOException("move broken")
      }
      dst
    }
    def sameWith(other: File): Boolean = {
      formatDir(f.getAbsolutePath) == formatDir(other.getAbsolutePath)
    }
    def trimParent(parent: File): String = {
      val newThisFile = new File(formatDir(f.getAbsolutePath))
      val newParentFile = new File(formatDir(parent.getAbsolutePath))
      val str = newThisFile.getAbsolutePath.substring(newParentFile.getAbsolutePath.length)
      new File(if(str=="") "/" else str).getAbsolutePath
    }
    def trimParent(parent: String): String = {
      val newThisFile = new File(formatDir(f.getAbsolutePath))
      val newParentFile = new File(formatDir(parent))
      val str = newThisFile.getAbsolutePath.substring(newParentFile.getAbsolutePath.length)
      new File(if(str=="") "/" else str).getAbsolutePath
    }
    def findFiles: List[File] = {

      if(f.isDirectory) {
        val subs = f.listFiles()

        if(subs == null) Nil
        else subs.toList.flatMap(_.findFiles).toList
      }
      else List(f)
    }
    def findEverything: List[File] = {
      if(f.isDirectory) {
        val subs = f.listFiles()
        if(subs == null) List(f)
        else f :: subs.toList.flatMap(_.findEverything).toList
      }
      else List(f)
    }
    def deepFindExt(ext: String):List[File] = findFiles.filter(_.getName.endsWith(ext))
    def getMd5: String = {
      val md5 = MessageDigest.getInstance("MD5")
      f.eachBuffer {
        (buf, size) => md5.update(buf, 0, size)
      }
      md5.digest().map(b=>"%02X".format(b)).mkString("")
    }
    def eachBuffer(func: (Array[Byte], Int) => Unit)= {
      if(f.exists() && f.isFile) {
        val fis = new FileInputStream(f)
        val buf = new Array[Byte](1024)
        var done = false;
        var to_read = f.length()
        while (!done && to_read > 0) {
          val r = fis.read(buf, 0, 1024)
          if (r <= 0) {
            done = true
          } else {
            to_read -= r;
            func(buf, r)
          }
        }
        fis.close()
      }
    }
  }
  def formatDir(dir: String) = {
    val stack = new mutable.Stack[String]
    dir.split("/").foreach {
      e => e match {
        case ".." => if(stack.size > 0) stack.pop();
        case _ => stack.push(e)
      }
    }
    stack.toList.reverse.filter(_!="."). mkString("/")
  }


}