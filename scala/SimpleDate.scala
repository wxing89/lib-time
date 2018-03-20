package com.xwtech.tools.date

class SimpleDate(var year: Int,
                 var month: Int,
                 var day: Int,
                 var hour: Int,
                 var minute: Int,
                 var second: Int) {

  import SimpleDate._

  def diffYear(s: SimpleDate): Int = { year - s.year }
  def diffMonth(s: SimpleDate): Int = { (year - s.year) * 12 + (month - s.month) }
  def diffDay(s: SimpleDate): Int = {
    if (year < s.year) {
      0 - s.diffDay(this)
    } else {
      var days = day - s.day
      for (y <- s.year until year) {
        days += dayInYear(SimpleDate.leapYear(y))
      }

      val leap1 = SimpleDate.leapYear(year)
      for (m <- 1 until month) { days += dayInMonth(leap1)(m) }
      val leap2 = SimpleDate.leapYear(s.year)
      for (m <- 1 until s.month) { days -= dayInMonth(leap2)(m) }
      days
    }
  }
  def diffHour(s: SimpleDate): Int = { this.diffDay(s) * 24 + hour - s.hour }
  def diffMinute(s: SimpleDate): Int = { this.diffHour(s) * 60 + minute - s.minute }
  def diffSecond(s: SimpleDate): Int = { this.diffMinute(s) * 60 + second - s.second }

  def addYear(value: Int): SimpleDate = {
    SimpleDate(year + value, month, day, hour, minute, second)
  }

  /**
    * add `num` day to current SimpleDate
    *
    * @param num days(can be negative) to add
    * @return new SimpleDate
    */
  def addDay(num: Int): SimpleDate = {
    var day = this.day + num
    var leap = SimpleDate.leapYear(this.year)
    if (day > 0 && day < dayInMonth(leap)(this.month))
      this.day = day
    else {
      for (i <- 1 until this.month) day += dayInMonth(leap)(i)
      if (day > 0) {
        while (day > dayInYear(SimpleDate.leapYear(this.year))) {
          day -= dayInYear(SimpleDate.leapYear(this.year))
          this.year += 1
        }
      } else {
        while (day <= 0) {
          day += dayInYear(SimpleDate.leapYear(this.year))
          this.year -= 1
        }
      }
      leap = SimpleDate.leapYear(this.year)
      this.month = 1
      while (day > dayInMonth(leap)(this.month)) {
        day -= dayInMonth(leap)(this.month)
        this.month += 1
      }
      this.day = day
    }
    this
  }

  def addHour(num: Int): SimpleDate = {
    val hour = this.hour + num
    if (hour >= 0 && hour < 24) {
      this.hour = hour
    } else {
      this.hour = hour % 24 + { if (hour < 0) 24 else 0 }
      this.addDay(hour / 24 - { if (hour < 0) 1 else 0 })
    }
    this
  }

  def addMinute(num: Int): SimpleDate = {
    val minute = this.minute + num
    if (minute >= 0 && minute < 60) {
      this.minute = minute
    } else {
      this.minute = minute % 60 + { if (minute < 0) 60 else 0 }
      this.addHour(minute / 60 - { if (minute < 0) 1 else 0 })
    }
    this
  }

  def addSecond(num: Int): SimpleDate = {
    val second = this.second + num
    if (second >= 0 && second < 60) {
      this.second = second
    } else {
      this.second = second % 60 + { if (second < 0) 60 else 0 }
      this.addMinute(second / 60 - { if (second < 0) 1 else 0 })
    }
    this
  }

  override def toString: String = "%04d-%02d-%02d %02d:%02d:%02d".format(year, month, day, hour, minute, second)

  /**
    * parameter: $dateFormat can be any combination of (yyyy, mm, dd, hh, mi, ss)
    * where yyyy:year, mm:month, dd:day, hh:hour, mi:minute, ss:second
    */
  def formatDate(dateFormat: String): String = {
    val dt = new StringBuilder(dateFormat)
    var index = dateFormat.indexOf("yyyy")
    if (index != -1) {
      dt.replace(index, index + 4, year.formatted("%04d"))
    }
    index = dateFormat.indexOf("mm")
    if (index != -1) {
      dt.replace(index, index + 2, month.formatted("%02d"))
    }
    index = dateFormat.indexOf("dd")
    if (index != -1) {
      dt.replace(index, index + 2, day.formatted("%02d"))
    }
    index = dateFormat.indexOf("hh")
    if (index != -1) {
      dt.replace(index, index + 2, hour.formatted("%02d"))
    }
    index = dateFormat.indexOf("mi")
    if (index != -1) {
      dt.replace(index, index + 2, minute.formatted("%02d"))
    }
    index = dateFormat.indexOf("ss")
    if (index != -1) {
      dt.replace(index, index + 2, second.formatted("%02d"))
    }
    dt.result()
  }

}

object SimpleDate {
  def apply(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int): SimpleDate = {
    new SimpleDate(year, month, day, hour, minute, second)
  }

  def apply(timeStr: String, format: String): SimpleDate = {
    var index = 0

    index = format.indexOf("yyyy")
    val year = timeStr.substring(index, index + 4).toInt

    index = format.indexOf("mm")
    val month = timeStr.substring(index, index + 2).toInt

    index = format.indexOf("dd")
    val day = timeStr.substring(index, index + 2).toInt

    index = format.indexOf("hh")
    val hour = timeStr.substring(index, index + 2).toInt

    index = format.indexOf("mi")
    val minute = timeStr.substring(index, index + 2).toInt

    index = format.indexOf("ss")
    val second = timeStr.substring(index, index + 2).toInt

    new SimpleDate(year, month, day, hour, minute, second)
  }

  private val dayInYear = Array(365, 366)

  private val dayInMonth = Array(
    Array(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
    Array(0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

  private def leapYear(year: Int): Int = { if (year % 4 == 0 && year % 100 != 0 || year % 400 == 0) 1 else 0 }

  def isLeapYear(year: Int): Boolean = { if (leapYear(year) == 1) true else false }

  def getDiffTime(timeStr1: String, timeStr2: String, format: String="yyyy-mm-dd hh:mi:ss"): Int = {
    SimpleDate(timeStr1, format).diffSecond(SimpleDate(timeStr2, format))
  }

  def getDiffTime(timeStr1: String, format1: String, timeStr2: String, format2: String): Int = {
    SimpleDate(timeStr1, format1).diffSecond(SimpleDate(timeStr2, format2))
  }

  def transTimeFormat(timeStr: String, format1: String, format2: String): String = {
    SimpleDate(timeStr, format1).formatDate(format2)
  }

  def getDateScore(timeStr: String, format: String="yyyy-mm-dd hh:mi:ss"): Long ={
    SimpleDate(timeStr, format).formatDate("yyyymmddhhmiss").toLong
  }
}

