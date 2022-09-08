package Test
import Test.Galileo.{flag, mapfunc, ptr, ptradd, recstr, str_len}
import Test.converters.{dec, rev}
import org.scalatest.flatspec.AnyFlatSpec

class ScalaTest extends AnyFlatSpec{
  recstr = "01ea0003333539313539393732323638323036105c92204e1bab62300f5f9181010d514c03330000370a341900350640003a410f2e423e0f4324450700460100470000000048030050f12d5100005200005300005900009000000000c400c500c600c700c800c900ca00d43b040000d60000d70000d80000d90000db00000000dc00000000e200030000e300000000e438000000e738340400e8805304f2e904000000f800000000f900000000fe3d008100b32282000f108300a80184000200850001920034323430323430313132333734353093000094003939373131323231323734383035323630343500ce31"
  str_len = recstr.length
  while (ptr < str_len && !flag) {
    val tag = recstr.toUpperCase().substring(ptr, ptr + 2).trim
    ptr = ptr + 2
    if (tag == "FE") {
      val ext_len = recstr.substring(ptr, ptradd(2)).trim
      val milestone: Int = ptr
      ptr = ptradd(2)
      while (!flag && ptr - milestone < dec(rev(ext_len)) * 2) {
        val exttag = recstr.substring(ptr, ptradd(2)).trim.replaceAll(" ", "")
        ptr = ptradd(2)
        stp(exttag)
      }
    }
    stp(tag)
  }
  def stp(i:String): Unit ={
    i match {
      case "01" => assert(mapfunc("Tag length", 2, "dec", "rev")==234)
      case "03" => assert(mapfunc("IMEI", 15, "ascii", "")=="359159972268206")
      case "10" => assert(mapfunc("Archive Record", 2, "dec", "")==23698)
      case "20" => assert(mapfunc( "Date & time", 4, "dec", "")==1310436194)
      case "30" => assert(mapfunc( "Co-ordinates", 9, "", "")==List(15,55.333133,25.268575))
      case "33" => assert(mapfunc("Speed", 4, "", "")==List(0.0,261.5))
      case "34" => assert(mapfunc("Height", 2, "sign", "rev")=="6400")
      case "35" => assert(mapfunc("Errors", 1, "dec", "")==6)
      case "40" => assert(mapfunc("Status of device", 2, "dec", "")==58)
      case "41" => assert(mapfunc( "Supply Voltage", 2, "dec", "")==3886)
      case "42" => assert(mapfunc("Battery Voltage", 2, "dec", "")==15887)
      case "43" => assert(mapfunc("Tracking device temperature", 1, "dec", "")==36)
      case "45" => assert(mapfunc("Status of outputs", 2, "dec", "")==1792)
      case "46" => assert(mapfunc("Status of inputs", 2, "dec", "")==256)
      case "47" => assert(mapfunc("Driving style", 4, "dec", "")==0)
      case "48" => assert(mapfunc("EXP Device status", 2, "dec", "")==768)
      case "50" => assert(mapfunc("Input voltage 0", 2, "dec", "")==61741)
      case "51" => assert(mapfunc("Input voltage 1", 2, "dec", "")==0)
      case "52" => assert(mapfunc("Input voltage 2", 2, "dec", "")==0)
      case "53" => assert(mapfunc("Input voltage 3", 2, "dec", "")==0)
      case "59" => assert(mapfunc("RS232 1", 2, "dec", "")==0)
      case "90" => assert(mapfunc("iButton key number", 4, "dec", "")==0)
      case "C4" => assert(mapfunc("CAN8BITR0", 1, "dec", "")==0)
      case "C5" => assert(mapfunc("CAN8BITR1", 1, "dec", "")==0)
      case "C6" => assert(mapfunc("CAN8BITR2", 1, "dec", "")==0)
      case "C7" => assert(mapfunc("CAN8BITR3", 1, "dec", "")==0)
      case "C8" => assert(mapfunc("CAN8BITR4", 1, "dec", "")==0)
      case "C9" => assert(mapfunc("CAN8BITR5", 1, "dec", "")==0)
      case "CA" => assert(mapfunc("CAN8BITR6", 1, "dec", "")==0)
      case "D4" => assert(mapfunc("Total mileage in m", 4, "dec", "rev")==1083)
      case "D6" => assert(mapfunc("CAN16BITR0", 2, "dec", "")==0)
      case "D7" => assert(mapfunc("CAN16BITR1", 2, "dec", "")==0)
      case "D8" => assert(mapfunc("CAN16BITR2", 2, "dec", "")==0)
      case "D9" => assert(mapfunc("CAN16BITR3", 2, "dec", "")==0)
      case "DB" => assert(mapfunc("CAN16BITR0 time", 4, "dec", "rev")==0)
      case "DC" => assert(mapfunc("CAN16BITR1 fuel level", 4, "dec", "rev")==0)
      case "E2" => assert(mapfunc("User data 0", 4, "big", "")=="196608")
      case "E3" => assert(mapfunc("User data 1", 4, "big", "")=="0")
      case "E4" => assert(mapfunc("User data 2", 4, "big", "")=="939524096")
      case "E7" => assert(mapfunc("User data 5", 4, "big", "")=="942932992")
      case "E8" => assert(mapfunc("User data 6", 4, "big", "")=="2152924402")
      case "E9" => assert(mapfunc("User data 7", 4, "big", "")=="67108864")
      case "F8" => assert(mapfunc("CAN32BITR13", 4, "dec", "")==0)
      case "F9" => assert(mapfunc("CAN32BITR14", 4, "dec", "")==0)
      case "FE" => assert(mapfunc("Extended tag length", 2, "", "")==None)
      case "8100" => assert(mapfunc("Cell identifier (CID)", 2, "dec", "")==45858)
      case "8200" => assert(mapfunc("Local area code (LAC)", 2, "dec", "")==3856)
      case "8300" => assert(mapfunc("Country code (MCC)", 2, "dec", "")==43009)
      case "8400" => assert(mapfunc("Operator code (MNC)", 2, "dec", "")==512)
      case "8500" => assert(mapfunc("RSSI", 1, "dec", "")==1)
      case "9200" => assert(mapfunc("Active SIM IMSI", 15, "ascii", "")=="424024011237450")
      case "9300" => assert(mapfunc("Active SIM card number", 1, "dec", "")==0)
      case "9400" => assert(mapfunc("Active SIM CCID tag", 20, "ascii", "")=="9971122127480526045")
      case _ => None
    }
  }
}
