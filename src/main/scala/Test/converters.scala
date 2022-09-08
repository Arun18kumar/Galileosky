package Test

import java.lang.Long

object converters {
  def dec(x: String): Int = {Integer.parseInt(x, 16)}
  def hex32(x: String): Int = {Integer.parseInt(x, 32)}
  def big (x : String) : Long = {Long.parseLong(x , 16)}
  def ascii(x : String) : Any = {val u = x.replaceAll("..", "$0 ").split(" ")
    var s = ""
    u.foreach(f => {
      s = s + Integer.parseInt(f , 16).toChar
    })
    s
  }
  def rev(x:String): String = {
    val s = x.replaceAll("..", "$0 ").split("\\s").reverse.mkString
    s
  }
  def sign(x:String) :String = {
    val s = Long.valueOf(x,16).toShort
    s.toString
  }
}