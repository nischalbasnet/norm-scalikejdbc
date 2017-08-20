package com.nischal.macros

object Helper
{
  def debug(message: Any): Unit =
  {
    println(s"${scala.Console.GREEN}||=================xx=================xx=================xx=================||")
    println(message.toString)
    println(s"||=================xx=================xx=================xx=================||${scala.Console.RESET}")
  }
}
