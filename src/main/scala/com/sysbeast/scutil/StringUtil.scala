package com.sysbeast.scutil

object StringUtil {
  implicit class StringUtilHelper(s: String) {
    def wrapQuote = "\"" + s + "\""
  }
}
