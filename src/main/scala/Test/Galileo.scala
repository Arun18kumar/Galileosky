package Test

import Test.converters.{ascii, big, dec, rev, sign}
import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions.{col, from_json, udf}
import org.apache.spark.sql.types.{StringType, StructField, StructType}

import scala.collection.mutable.{ListBuffer, Map}
import scala.util.control.Breaks.{break, breakable}

object Galileo extends Galileosky {
  var mp = Map[String,Any]()
  var fullmp : Map[Int,Map[String,Any]] = Map[Int,Map[String,Any]]()
  var str = ""
  var recstr = ""
  var ptr = 0
  var str_len = 0
  var flag: Boolean = false
  val ptradd: Int => Int = (n:Int)=> {
    if(str_len > (n*2)){
      ptr + (n*2)
    }else {
      flag = true
      ptr + (n*2)+n -1
    }
  }
  override def mapfunc(tagname: String, len: Int, conversion: String, form: String): Any = conversion match {
    case "dec" if form == "rev" =>
      val cutstr = recstr.substring(ptr,ptradd(len)).trim
      ptr = ptradd(len)
      mp += (tagname -> dec(rev(cutstr)))
      dec(rev(cutstr))
    case "dec" =>
      val cutstr = recstr.substring(ptr,ptradd(len)).trim
      ptr = ptradd(len)
      mp += (tagname -> dec(cutstr))
      dec(cutstr)
    case "ascii" =>
      var cutstr = recstr.substring(ptr,ptradd(len)).trim
      if (cutstr.endsWith("00")) {cutstr = cutstr.substring(0, cutstr.length - 2)}
      ptr = ptradd(len)
      mp += (tagname -> ascii(cutstr))
      ascii(cutstr).toString
    case "sign" =>
      val cutstr = recstr.substring(ptr,ptradd(len)).trim
      ptr = ptradd(len)
      mp += (tagname -> sign(cutstr))
      sign(cutstr)
    case "big" =>
      val cutstr = recstr.substring(ptr,ptradd(len)).trim
      ptr = ptradd(len)
      mp += (tagname -> big(cutstr))
      big(cutstr).toString
    case "" if tagname == "Co-ordinates" =>
      var cutstr = recstr.substring(ptr,ptradd(len)).trim
      ptr = ptradd(len)
      cutstr = cutstr.replaceAll("\\s", "")
      cutstr = cutstr.replaceAll("..", "$0 ")
      val sat = dec(cutstr.substring(0, 2))
      mp += ("Satelite no" -> sat)
      cutstr = cutstr.split("\\s").reverse.mkString
      val lon = dec(cutstr.substring(0, 8)).toDouble / 1000000
      val lat = dec(cutstr.substring(8, 16)).toDouble / 1000000
      mp += ("Longitude" -> lon)
      mp += ("Lattitude" -> lat)
      List(sat,lon,lat)
    case "" if tagname == "Speed" =>
      val cutstr = recstr.substring(ptr,ptradd(len)).trim
      ptr = ptradd(len)
      mp += ("Speed" -> dec(rev(cutstr.substring(0, 4))).toDouble / 10)
      mp += ("Direction" -> dec(rev(cutstr.substring(4, 8))).toDouble / 10)
      List(dec(rev(cutstr.substring(0, 4))).toDouble / 10,dec(rev(cutstr.substring(4, 8))).toDouble / 10)
    case "" if tagname == "User Array" =>
      recstr = recstr.replaceAll("\\s", "")
      val f = recstr.indexOf("EA") + 2
      var ea = recstr.substring(f, f + 2)
      val k = dec(ea)
      ptr = ptradd(k+1)
      ea = recstr.substring(f + 2, f + 2 + k * 2)
      val o = ea.replaceAll("..", "$0 ")
      mp += ("User Array" -> o)
    case _ => None
  }
  def main(args:Array[String]): Unit = {
    Logger.getLogger("org.apache.spark").setLevel(Level.ERROR)
    val spark = SparkSession.builder()
      .master("local[*]")
      .appName("Multi-record")
      .getOrCreate()

    val df = spark.read
      .format("kafka")
      .option("kafka.bootstrap.servers","localhost:9092")
      .option("subscribe","package")
      .option("startingOffsets","earliest")
      .load()

    val schema = StructType(List(StructField("record", StringType)))

    val valuedf = df.select(from_json(col("value").cast(StringType),schema).alias("value"))

    val expdf = valuedf.selectExpr("value.record")

//    expdf.show()
    val convertDF = udf(galileoskyfunc)

    expdf.select(convertDF(col("record")) as "result").show()
  }

  val galileoskyfunc: String => String = (Stri: String) => {
    var e = 0
    str = Stri.toUpperCase()
    str = str.replaceAll("\\s", "")
    val header = str.substring(0, 8)
    str = str.replace(header, "")
    val footer = str.reverse.substring(0, 4).reverse
    str = str.replace(footer, "")
    val pack = new ListBuffer[String]
    breakable {
      for (i <- 0 to str.length) {
        var k = 0
        var rec = ""
        if (str.isEmpty) {
          break
        }
        else {
          var v = str.substring(2, 6)
          v = v.replaceAll("..", "$0 ").split(" ").reverse.mkString
          v = v.replaceAll("\\s", "")
          k = (dec(v) + 2 + 3) * 2
          rec = str.substring(0, k)
          pack += rec
          str = str.replace(rec, "")
        }
      }
    }
    pack.foreach(f => {
      var stri = f
      stri = stri.replaceAll("..", "$0 ")
      breakable {
        for (i <- 0 to stri.length) {
          if (stri.contains("FE")) {
            stri = stri.replaceAll("\\s", "")
            val fn = stri.indexOf("FE")
            var k = stri.substring(fn + 2, fn + 6)
            k = k.replaceAll("..", "$0 ").split(" ").reverse.mkString
            val min = Integer.parseInt(k, 16)
            val sub = stri.substring(0, fn + 6 + min * 2)
            stri = stri.replaceAll(sub, "")
            mp = Map[String, Any]()
            recstr = sub.replaceAll("\\s", "")
            str_len = recstr.length
            ptr = 0
            while (ptr < str_len && !flag) {
              val tag = recstr.toUpperCase().substring(ptr, ptr + 2).trim
              ptr = ptr + 2
              if (tag == "FE") {
                val ext_len = recstr.substring(ptr, ptradd(2)).trim
                val milestone: Int = ptr
                mp += ("Extended tag length" -> dec(rev(ext_len)))
                ptr = ptradd(2)
                while (!flag && ptr - milestone < dec(rev(ext_len)) * 2) {
                  val exttag = recstr.substring(ptr, ptradd(2)).trim.replaceAll(" ", "")
                  ptr = ptradd(2)
                  map(exttag)()
                }
              }
              map(tag)()
            }
            fullmp.put(e, mp)
            e += 1
          }
        }
      }
  })
    fullmp.foreach {
      case (k,v) =>
//        println("Record : "+k)
        v.foreach {
          case (key,value) => println(key + " -> " + value)
        }
        println()
        println("----------------------------------------------------------")
        println()
    }
    fullmp.toString()
  }
}
