package Test

class Galileosky {

  def mapfunc(Name: String, len: Int,conversion:String,form : String): Any = conversion match {
    case _ => ""
  }
  val map = Map(
    "01" -> (() => mapfunc("Tag length", 2, "dec", "rev")),
    "02" -> (() =>mapfunc("Firmware version", 2, "dec", "rev")),
    "03" -> (() =>mapfunc("IMEI", 15, "ascii", "")),
    "04" -> (() =>mapfunc("Identifier of a device", 2, "dec", "")),
    "10" -> (() =>mapfunc( "Archive Record", 2, "dec", "")),
    "20" -> (() =>mapfunc( "Date & time", 4, "dec", "")),
    "30" -> (() =>mapfunc("Co-ordinates", 9, "", "")),
    "33" -> (() =>mapfunc("Speed", 4, "", "")),
    "34" -> (() =>mapfunc("Height", 2, "sign", "rev")),
    "35" -> (() =>mapfunc( "Errors", 1, "dec", "")),
    "40" -> (() =>mapfunc("Status of device", 2, "dec", "")),
    "41" -> (() =>mapfunc("Supply Voltage", 2, "dec", "")),
    "42" -> (() =>mapfunc( "Battery Voltage", 2, "dec", "")),
    "43" -> (() =>mapfunc( "Tracking device temperature", 1, "dec", "")),
    "45" -> (() =>mapfunc( "Status of outputs", 2, "dec", "")),
    "46" -> (() =>mapfunc( "Status of inputs", 2, "dec", "")),
    "47" -> (() =>mapfunc( "Driving style", 4, "dec", "")),
    "48" -> (() =>mapfunc( "EXP Device status", 2, "dec", "")),
    "50" -> (() =>mapfunc( "Input voltage 0", 2, "dec", "")),
    "51" -> (() =>mapfunc( "Input voltage 1", 2, "dec", "")),
    "52" -> (() =>mapfunc( "Input voltage 2", 2, "dec", "")),
    "53" -> (() =>mapfunc( "Input voltage 3", 2, "dec", "")),
    "58" -> (() =>mapfunc( "RS232 0", 2, "dec", "")),
    "59" -> (() =>mapfunc( "RS232 1", 2, "dec", "")),
    "70" -> (() =>mapfunc( "Thermometer identifier", 2, "", "")),
    "71" -> (() =>mapfunc( "Thermometer identifier", 2, "", "")),
    "72" -> (() =>mapfunc( "Thermometer identifier", 2, "", "")),
    "73" -> (() =>mapfunc( "Thermometer identifier", 2, "", "")),
    "74" -> (() =>mapfunc( "Thermometer identifier", 2, "", "")),
    "75" -> (() =>mapfunc( "Thermometer identifier", 2, "", "")),
    "76" -> (() =>mapfunc( "Thermometer identifier", 2, "", "")),
    "77" -> (() =>mapfunc("Thermometer identifier", 2, "", "")),
    "90" -> (() =>mapfunc( "iButton key number", 4, "dec", "")),
    "C0" -> (() =>mapfunc( "CAN_A0", 4, "", "")),
    "C1" -> (() =>mapfunc( "CAN_A0", 4, "", "rev")),
    "C2" -> (() =>mapfunc( "CAN_B0", 4, "dec", "")),
    "C3" -> (() =>mapfunc( "CAN_B1", 4, "dec", "")),
    "C4" -> (() =>mapfunc( "CAN8BITR0", 1, "dec", "")),
    "C5" -> (() =>mapfunc("CAN8BITR1", 1, "dec", "")),
    "C6" -> (() =>mapfunc( "CAN8BITR2", 1, "dec", "")),
    "C7" -> (() =>mapfunc("CAN8BITR3", 1, "dec", "")),
    "C8" -> (() =>mapfunc( "CAN8BITR4", 1, "dec", "")),
    "C9" -> (() =>mapfunc( "CAN8BITR5", 1, "dec", "")),
    "CA" -> (() =>mapfunc( "CAN8BITR6", 1, "dec", "")),
    "CB" -> (() =>mapfunc( "CAN8BITR7", 1, "dec", "")),
    "CC" -> (() =>mapfunc( "CAN8BITR8", 1, "dec", "")),
    "CD" -> (() =>mapfunc("CAN8BITR9", 1, "dec", "")),
    "CE" ->(() => mapfunc( "CAN8BITR10", 1, "dec", "")),
    "CF" -> (() =>mapfunc( "CAN8BIT11", 1, "dec", "")),
    "D0" -> (() =>mapfunc( "CAN8BIT11", 2, "dec", "")),
    "D1" -> (() =>mapfunc( "CAN8BIT11", 2, "dec", "")),
    "D2" -> (() =>mapfunc( "CAN8BIT11", 2, "dec", "")),
    "D3" -> (() =>mapfunc( "second iButton key number", 4, "dec", "rev")),
    "D4" -> (() =>mapfunc( "Total mileage in m", 4, "dec", "rev")),
    "D5" -> (() =>mapfunc( "State of iButton keys", 1, "dec", "")),
    "D6" -> (() =>mapfunc("CAN16BITR0", 2, "dec", "")),
    "D7" -> (() =>mapfunc( "CAN16BITR1", 2, "dec", "")),
    "D8" -> (() =>mapfunc( "CAN16BITR2", 2, "dec", "")),
    "D9" -> (() =>mapfunc( "CAN16BITR3", 2, "dec", "")),
    "DA" -> (() =>mapfunc( "CAN16BITR4", 2, "dec", "")),
    "DB" -> (() =>mapfunc( "CAN16BITR0 time", 4, "dec", "rev")),
    "DC" -> (() =>mapfunc( "CAN16BITR1 fuel level", 4, "dec", "rev")),
    "DD" -> (() =>mapfunc( "CAN16BITR2 user prefix", 4, "dec", "rev")),
    "DE" -> (() =>mapfunc( "CAN16BITR3 user prefix", 4, "dec", "rev")),
    "DF" -> (() =>mapfunc("CAN16BITR4 user prefix", 4, "dec", "rev")),
    "54" -> (() =>mapfunc( "Input 4 values", 2, "dec", "")),
    "55" -> (() =>mapfunc( "Input 5 values", 2, "dec", "")),
    "56" -> (() =>mapfunc( "Input 6 values", 2, "dec", "")),
    "57" -> (() =>mapfunc("Input 7 values", 2, "dec", "")),
    "80" -> (() =>mapfunc( "DS1923 sensor", 3, "dec", "")),
    "81" -> (() =>mapfunc( "DS1923 sensor", 3, "dec", "")),
    "82" -> (() =>mapfunc( "DS1923 sensor", 3, "dec", "")),
    "83" -> (() =>mapfunc( "DS1923 sensor", 3, "dec", "")),
    "84" -> (() =>mapfunc( "DS1923 sensor", 3, "dec", "")),
    "85" -> (() =>mapfunc( "DS1923 sensor", 3, "dec", "")),
    "86" -> (() =>mapfunc( "DS1923 sensor", 3, "dec", "")),
    "87" -> (() =>mapfunc( "DS1923 sensor", 3, "dec", "")),
    "60" -> (() =>mapfunc( "RS485 0 - fuel level", 2, "dec", "")),
    "61" -> (() =>mapfunc( "RS485 1 - fuel level", 2, "dec", "")),
    "62" -> (() =>mapfunc( "RS485 2 - fuel level", 2, "dec", "")),
    "63" -> (() =>mapfunc( "RS485 - fuel level", 3, "dec", "")),
    "64" -> (() => mapfunc( "RS485 - fuel level", 3, "dec", "")),
    "65" -> (() =>mapfunc("RS485 - fuel level", 3, "dec", "")),
    "66" -> (() =>mapfunc( "RS485 - fuel level", 3, "dec", "")),
    "67" -> (() =>mapfunc( "RS485 - fuel level", 3, "dec", "")),
    "68" -> (() =>mapfunc( "RS485 - fuel level", 3, "dec", "")),
    "69" -> (() =>mapfunc( "RS485 - fuel level", 3, "dec", "")),
    "6A" -> (() =>mapfunc( "RS485 - fuel level", 3, "dec", "")),
    "6B" -> (() =>mapfunc( "RS485 - fuel level", 3, "dec", "")),
    "6C" -> (() =>mapfunc( "RS485 - fuel level", 3, "dec", "")),
    "6D" -> (() =>mapfunc( "RS485 - fuel level", 3, "dec", "")),
    "6E" -> (() =>mapfunc( "RS485 - fuel level", 3, "dec", "")),
    "6F" ->(() => mapfunc( "RS485 - fuel level", 3, "dec", "")),
    "88" -> (() =>mapfunc( "Extended data RS232[0]", 1, "dec", "")),
    "89" -> (() =>mapfunc( "Expanded data RS232[1]", 1, "dec", "")),
    "8A" -> (() =>mapfunc( "RS232 temperature 0", 1, "dec", "")),
    "8B" -> (() =>mapfunc( "RS232 temperature 1", 1, "dec", "")),
    "8C" -> (() =>mapfunc( "RS232 temperature 2", 1, "dec", "")),
    "A0" -> (() =>mapfunc( "CAN8BITR15", 1, "dec", "")),
    "A1" -> (() =>mapfunc( "CAN8BITR16", 1, "dec", "")),
    "A2" -> (() =>mapfunc( "CAN8BITR17", 1, "dec", "")),
    "A3" -> (() =>mapfunc( "CAN8BITR18", 1, "dec", "")),
    "A4" -> (() =>mapfunc( "CAN8BITR19", 1, "dec", "")),
    "A5" -> (() =>mapfunc( "CAN8BITR20", 1, "dec", "")),
    "A6" -> (() =>mapfunc( "CAN8BITR21", 1, "dec", "")),
    "A7" -> (() =>mapfunc( "CAN8BITR22", 1, "dec", "")),
    "A8" -> (() =>mapfunc( "CAN8BITR23", 1, "dec", "")),
    "A9" -> (() =>mapfunc( "CAN8BITR24", 1, "dec", "")),
    "AA" -> (() =>mapfunc( "CAN8BITR25", 1, "dec", "")),
    "AB" -> (() =>mapfunc( "CAN8BITR26", 1, "dec", "")),
    "AC" -> (() =>mapfunc( "CAN8BITR27", 1, "dec", "")),
    "AD" -> (() =>mapfunc( "CAN8BITR28", 1, "dec", "")),
    "AE" -> (() =>mapfunc( "CAN8BITR29", 1, "dec", "")),
    "AF" -> (() =>mapfunc( "CAN8BITR30", 1, "dec", "")),
    "B1" -> (() =>mapfunc( "CAN16BITR6", 2, "dec", "")),
    "B2" -> (() =>mapfunc( "CAN16BITR7", 2, "dec", "")),
    "B3" -> (() =>mapfunc( "CAN16BITR8", 2, "dec", "")),
    "B4" -> (() =>mapfunc( "CAN16BITR9", 2, "dec", "")),
    "B5" -> (() =>mapfunc( "CAN16BITR10", 2, "dec", "")),
    "B6" -> (() =>mapfunc( "CAN16BITR11", 2, "dec", "")),
    "B7" -> (() =>mapfunc( "CAN16BITR12", 2, "dec", "")),
    "B8" -> (() =>mapfunc("CAN16BITR13", 2, "dec", "")),
    "B9" -> (() =>mapfunc("CAN16BITR14", 2, "dec", "")),
    "B0" -> (() =>mapfunc( "CAN16BITR5", 2, "dec", "")),
    "F0" -> (() =>mapfunc( "CAN32BITR5", 4, "dec", "")),
    "F1" -> (() =>mapfunc( "CAN32BITR6", 4, "dec", "")),
    "F2" -> (() =>mapfunc( "CAN32BITR7", 4, "big", "")),
    "F3" -> (() =>mapfunc( "CAN32BITR8", 4, "dec", "")),
    "F4" -> (() =>mapfunc( "CAN32BITR9", 4, "dec", "")),
    "F5" -> (() =>mapfunc( "CAN32BITR10", 4, "dec", "")),
    "F6" -> (() =>mapfunc( "CAN32BITR11", 4, "dec", "")),
    "F7" -> (() =>mapfunc( "CAN32BITR12", 4, "dec", "")),
    "F8" -> (() =>mapfunc( "CAN32BITR13", 4, "dec", "")),
    "F9" -> (() =>mapfunc( "CAN32BITR14", 4, "dec", "")),
    "5A" -> (() =>mapfunc( "REP-500 electricity meter", 4, "dec", "")),
    "E2" -> (() =>mapfunc( "User data 0", 4, "big", "")),
    "E3" -> (() =>mapfunc( "User data 1", 4, "big", "")),
    "E4" -> (() =>mapfunc( "User data 2", 4, "big", "")),
    "E5" -> (() =>mapfunc( "User data 3", 4, "big", "")),
    "E6" -> (() =>mapfunc( "User data 4", 4, "big", "")),
    "E7" -> (() =>mapfunc( "User data 5", 4, "big", "")),
    "E8" -> (() =>mapfunc( "User data 6", 4, "big", "")),
    "E9" -> (() =>mapfunc( "User data 7", 4, "big", "")),
    "EA" -> (() =>mapfunc("User Array",1,"","")),
    "FE" -> (() => mapfunc( "Extended tag length", 2, "", "")),
    "0100" -> (() =>mapfunc( "Tag Modbus 0", 4, "dec", "")),
    "0200" -> (() =>mapfunc( "Tag Modbus 1", 4, "dec", "")),
    "0300" -> (() =>mapfunc("Tag Modbus 2", 4, "dec", "")),
    "0400" -> (() =>mapfunc("Tag Modbus 3", 4, "dec", "")),
    "0500" -> (() =>mapfunc("Tag Modbus 4", 4, "dec", "")),
    "0600" -> (() =>mapfunc("Tag Modbus 5", 4, "dec", "")),
    "0700" -> (() =>mapfunc("Tag Modbus 6", 4, "dec", "")),
    "0800" -> (() =>mapfunc("Tag Modbus 7", 4, "dec", "")),
    "0900" -> (() =>mapfunc("Tag Modbus 8", 4, "dec", "")),
    "0A00" -> (() =>mapfunc("Tag Modbus 9", 4, "dec", "")),
    "0B00" -> (() =>mapfunc("Tag Modbus 10", 4, "dec", "")),
    "0C00" -> (() =>mapfunc("Tag Modbus 11", 4, "dec", "")),
    "0D00" -> (() =>mapfunc("Tag Modbus 12", 4, "dec", "")),
    "0E00" -> (() =>mapfunc("Tag Modbus 13", 4, "dec", "")),
    "0300" -> (() =>mapfunc("Tag Modbus 14", 4, "dec", "")),
    "1000" -> (() =>mapfunc("Tag Modbus 15", 4, "dec", "")),
    "1100" -> (() =>mapfunc("Tag Modbus 16", 4, "dec", "")),
    "1200" -> (() =>mapfunc("Tag Modbus 17", 4, "dec", "")),
    "1300" -> (() =>mapfunc("Tag Modbus 18", 4, "dec", "")),
    "1400" -> (() =>mapfunc("Tag Modbus 19", 4, "dec", "")),
    "1500" -> (() =>mapfunc("Tag Modbus 20", 4, "dec", "")),
    "1600" -> (() =>mapfunc("Tag Modbus 21", 4, "dec", "")),
    "1700" -> (() =>mapfunc("Tag Modbus 22", 4, "dec", "")),
    "1800" -> (() =>mapfunc("Tag Modbus 23", 4, "dec", "")),
    "1900" -> (() =>mapfunc("Tag Modbus 24", 4, "dec", "")),
    "1A00" -> (() =>mapfunc("Tag Modbus 25", 4, "dec", "")),
    "1B00" -> (() =>mapfunc("Tag Modbus 26", 4, "dec", "")),
    "1C00" -> (() =>mapfunc("Tag Modbus 27", 4, "dec", "")),
    "1D00" -> (() =>mapfunc("Tag Modbus 28", 4, "dec", "")),
    "1E00" -> (() =>mapfunc("Tag Modbus 29", 4, "dec", "")),
    "1F00" -> (() =>mapfunc("Tag Modbus 30", 4, "dec", "")),
    "2000" -> (() =>mapfunc( "Tag Modbus 31", 4, "dec", "")),
    "2100" -> (() =>mapfunc( "Tag Bluetooth 0", 4, "dec", "")),
    "2200" -> (() =>mapfunc( "Tag Bluetooth 1", 4, "dec", "")),
    "2300" -> (() =>mapfunc( "Tag Bluetooth 2", 4, "dec", "")),
    "2400" -> (() =>mapfunc( "Tag Bluetooth 3", 4, "dec", "")),
    "2500" -> (() =>mapfunc( "Tag Bluetooth 4", 4, "dec", "")),
    "2600" -> (() =>mapfunc( "Tag Bluetooth 5", 4, "dec", "")),
    "2700" -> (() =>mapfunc( "Tag Bluetooth 6", 4, "dec", "")),
    "2800" -> (() =>mapfunc( "Tag Bluetooth 7", 4, "dec", "")),
    "2900" -> (() =>mapfunc( "Tag Bluetooth 8", 4, "dec", "")),
    "2A00" -> (() =>mapfunc( "Tag Bluetooth 9", 4, "dec", "")),
    "2B00" -> (() =>mapfunc( "Tag Bluetooth 10", 4, "dec", "")),
    "2C00" -> (() =>mapfunc( "Tag Bluetooth 11", 4, "dec", "")),
    "2D00" -> (() =>mapfunc( "Tag Bluetooth 12", 4, "dec", "")),
    "2E00" -> (() =>mapfunc( "Tag Bluetooth 13", 4, "dec", "")),
    "2F00" -> (() =>mapfunc( "Tag Bluetooth 14", 4, "dec", "")),
    "3000" -> (() =>mapfunc( "Tag Bluetooth 15", 4, "dec", "")),
    "3100" -> (() =>mapfunc( "Tag Bluetooth 16", 4, "dec", "")),
    "3200" -> (() =>mapfunc( "Tag Bluetooth 17", 4, "dec", "")),
    "3300" -> (() =>mapfunc( "Tag Bluetooth 18", 4, "dec", "")),
    "3400" -> (() =>mapfunc( "Tag Bluetooth 19", 4, "dec", "")),
    "3500" -> (() =>mapfunc( "Tag Bluetooth 20", 4, "dec", "")),
    "3600" -> (() =>mapfunc( "Tag Bluetooth 21", 4, "dec", "")),
    "3700" -> (() =>mapfunc( "Tag Bluetooth 22", 4, "dec", "")),
    "3800" -> (() =>mapfunc( "Tag Bluetooth 23", 4, "dec", "")),
    "3900" -> (() =>mapfunc( "Tag Bluetooth 24", 4, "dec", "")),
    "3A00" -> (() =>mapfunc( "Tag Bluetooth 25", 4, "dec", "")),
    "3B00" -> (() =>mapfunc( "Tag Bluetooth 26", 4, "dec", "")),
    "3C00" -> (() =>mapfunc( "Tag Bluetooth 27", 4, "dec", "")),
    "3D00" -> (() =>mapfunc( "Tag Bluetooth 28", 4, "dec", "")),
    "3E00" -> (() =>mapfunc( "Tag Bluetooth 29", 4, "dec", "")),
    "3F00" -> (() =>mapfunc( "Tag Bluetooth 30", 4, "dec", "")),
    "4000" -> (() =>mapfunc( "Tag Bluetooth 31", 4, "dec", "")),
    "4100" -> (() =>mapfunc( "Tag Bluetooth 32", 4, "dec", "")),
    "4200" -> (() =>mapfunc( "Tag Bluetooth 33", 4, "dec", "")),
    "4300" -> (() =>mapfunc( "Tag Bluetooth 34", 4, "dec", "")),
    "4400" -> (() =>mapfunc( "Tag Bluetooth 35", 4, "dec", "")),
    "4500" -> (() =>mapfunc( "Tag Bluetooth 36", 4, "dec", "")),
    "4600" -> (() =>mapfunc( "Tag Bluetooth 37", 4, "dec", "")),
    "4700" -> (() =>mapfunc( "Tag Bluetooth 38", 4, "dec", "")),
    "4800" -> (() =>mapfunc( "Tag Bluetooth 39", 4, "dec", "")),
    "4900" -> (() =>mapfunc( "Tag Bluetooth 40", 4, "dec", "")),
    "4A00" -> (() =>mapfunc( "Tag Bluetooth 41", 4, "dec", "")),
    "4B00" -> (() =>mapfunc( "Tag Bluetooth 42", 4, "dec", "")),
    "4C00" -> (() =>mapfunc( "Tag Bluetooth 43", 4, "dec", "")),
    "4D00" -> (() =>mapfunc( "Tag Bluetooth 44", 4, "dec", "")),
    "4E00" -> (() =>mapfunc( "Tag Bluetooth 45", 4, "dec", "")),
    "4F00" -> (() =>mapfunc( "Tag Bluetooth 46", 4, "dec", "")),
    "5000" -> (() =>mapfunc("Tag Bluetooth 47", 4, "dec", "")),
    "5100" -> (() =>mapfunc( "Tag Bluetooth 48", 4, "dec", "")),
    "5200" -> (() =>mapfunc( "Tag Bluetooth 49", 4, "dec", "")),
    "5300" -> (() =>mapfunc( "Tag Bluetooth 50", 4, "dec", "")),
    "5400" -> (() =>mapfunc( "Tag Bluetooth 51", 4, "dec", "")),
    "5500" -> (() =>mapfunc( "Tag Bluetooth 52", 4, "dec", "")),
    "5600" -> (() =>mapfunc( "Tag Bluetooth 53", 4, "dec", "")),
    "5700" -> (() =>mapfunc( "Tag Bluetooth 54", 4, "dec", "")),
    "5800" -> (() =>mapfunc( "Tag Bluetooth 55", 4, "dec", "")),
    "5900" -> (() =>mapfunc( "Tag Bluetooth 56", 4, "dec", "")),
    "5A00" -> (() =>mapfunc( "Tag Bluetooth 57", 4, "dec", "")),
    "5B00" -> (() =>mapfunc( "Tag Bluetooth 58", 4, "dec", "")),
    "5C00" -> (() =>mapfunc( "Tag Bluetooth 59", 4, "dec", "")),
    "5D00" -> (() =>mapfunc( "Tag Bluetooth 60", 4, "dec", "")),
    "5E00" -> (() =>mapfunc( "Tag Bluetooth 61", 4, "dec", "")),
    "5F00" -> (() =>mapfunc( "Tag Bluetooth 62", 4, "dec", "")),
    "6000" -> (() =>mapfunc("Tag Bluetooth 63", 4, "dec", "")),
    "6100" -> (() =>mapfunc("Tag Modbus 32", 4, "dec", "")),
    "6200" -> (() =>mapfunc("Tag Modbus 33", 4, "dec", "")),
    "6300" -> (() =>mapfunc("Tag Modbus 34", 4, "dec", "")),
    "6400" -> (() =>mapfunc("Tag Modbus 35", 4, "dec", "")),
    "6500" -> (() =>mapfunc("Tag Modbus 36", 4, "dec", "")),
    "6600" -> (() =>mapfunc("Tag Modbus 37", 4, "dec", "")),
    "6700" -> (() =>mapfunc("Tag Modbus 38", 4, "dec", "")),
    "6800" -> (() =>mapfunc("Tag Modbus 39", 4, "dec", "")),
    "6900" -> (() =>mapfunc("Tag Modbus 40", 4, "dec", "")),
    "6A00" -> (() =>mapfunc("Tag Modbus 41", 4, "dec", "")),
    "6B00" -> (() =>mapfunc("Tag Modbus 42", 4, "dec", "")),
    "6C00" -> (() =>mapfunc("Tag Modbus 43", 4, "dec", "")),
    "6D00" -> (() =>mapfunc("Tag Modbus 44", 4, "dec", "")),
    "6E00" -> (() =>mapfunc("Tag Modbus 45", 4, "dec", "")),
    "6F00" -> (() =>mapfunc("Tag Modbus 46", 4, "dec", "")),
    "7000" -> (() =>mapfunc("Tag Modbus 47", 4, "dec", "")),
    "7100" -> (() =>mapfunc("Tag Modbus 48", 4, "dec", "")),
    "7200" -> (() =>mapfunc("Tag Modbus 49", 4, "dec", "")),
    "7300" -> (() =>mapfunc("Tag Modbus 50", 4, "dec", "")),
    "7400" -> (() =>mapfunc("Tag Modbus 51", 4, "dec", "")),
    "7500" -> (() =>mapfunc("Tag Modbus 52", 4, "dec", "")),
    "7600" -> (() =>mapfunc("Tag Modbus 53", 4, "dec", "")),
    "7700" -> (() =>mapfunc("Tag Modbus 54", 4, "dec", "")),
    "7800" -> (() =>mapfunc("Tag Modbus 55", 4, "dec", "")),
    "7900" -> (() =>mapfunc("Tag Modbus 56", 4, "dec", "")),
    "7A00" -> (() =>mapfunc("Tag Modbus 57", 4, "dec", "")),
    "7B00" -> (() =>mapfunc("Tag Modbus 58", 4, "dec", "")),
    "7C00" -> (() =>mapfunc("Tag Modbus 59", 4, "dec", "")),
    "7D00" -> (() =>mapfunc("Tag Modbus 60", 4, "dec", "")),
    "7E00" -> (() =>mapfunc("Tag Modbus 61", 4, "dec", "")),
    "7F00" -> (() =>mapfunc("Tag Modbus 62", 4, "dec", "")),
    "8000" -> (() =>mapfunc( "Tag Modulus 63", 4, "dec", "")),
    "8100" -> (() =>mapfunc("Cell identifier (CID)", 2, "dec", "")),
    "8200" -> (() =>mapfunc("Local area code (LAC)", 2, "dec", "")),
    "8300" -> (() => mapfunc("Country code (MCC)", 2, "dec", "")),
    "8400" ->(()=> mapfunc( "Operator code (MNC)", 2, "dec", "")),
    "8500" ->(() => mapfunc( "RSSI", 1, "dec", "")),
    "8600" ->(() => mapfunc( "Temperature sensor extended value tag 0", 4, "dec", "")),
    "8700" ->(() => mapfunc( "Temperature sensor extended value tag 1", 4, "dec", "")),
    "8800" ->(() => mapfunc( "Temperature sensor extended value tag 2", 4, "dec", "")),
    "8900" ->(() => mapfunc( "Temperature sensor extended value tag 3", 4, "dec", "")),
    "8A00" ->(() => mapfunc( "Temperature sensor extended value tag 4", 4, "dec", "")),
    "8B00" ->(() => mapfunc("Temperature sensor extended value tag 5", 4, "dec", "")),
    "8C00" ->(() => mapfunc( "Temperature sensor extended value tag 6", 4, "dec", "")),
    "8D00" ->(() => mapfunc( "Temperature sensor extended value tag 7", 4, "dec", "")),
    "8E00" ->(() => mapfunc( "GPS satellite information tag", 4, "dec", "")),
    "8F00" ->(() => mapfunc( "GLONASS satellite information tag", 4, "dec", "")),
    "9000" ->(() => mapfunc("BAIDOU satellite information tag", 4, "dec", "")),
    "9100" ->(() => mapfunc( "GALILEO satellite information tag", 4, "dec", "")),
    "9200" ->(() => mapfunc("Active SIM IMSI", 15, "ascii", "")),
    "9300" ->(() => mapfunc( "Active SIM card number", 1, "dec", "")),
    "9400" ->(() => mapfunc( "Active SIM CCID tag", 20, "ascii", "")),
  )
}
