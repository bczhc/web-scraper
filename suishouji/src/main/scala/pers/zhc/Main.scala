package pers.zhc

import com.google.gson.{Gson, JsonArray, JsonObject}

import java.net.{URL, URLConnection}
import java.nio.charset.StandardCharsets
import java.util.{Calendar, TimeZone}
import scala.jdk.CollectionConverters._

/** @author
  *   bczhc
  */
object Main {
  val COOKIE =
    ???
  val CARD_ID = ???

  val gson = new Gson()

  System.load("/home/bczhc/code/jni/build/libjni-lib.so")

  def main(args: Array[String]): Unit = {
    val dateStrs = new DatesRequest().fetchDateStrs
    println(dateStrs)

    val records = new DetailsRequest(dateStrs).fetch

    println(records)

    val db = new Database("./ssj-journal.db")
    db.db.beginTransaction()
    records.foreach(db.insert)
    db.db.commit()
    db.close()
  }

  class BaseRequest {
    val HEADER = Map(
      ("content-type", "application/x-www-form-urlencoded; charset=UTF-8"),
      ("Cookie", COOKIE)
    )

    def writeConnection(c: URLConnection, data: Array[Byte]): Unit = {
      val os = c.getOutputStream
      os.write(data)
      os.close()
    }

    def setHeaders(c: URLConnection): Unit = {
      for (e <- HEADER) {
        c.addRequestProperty(e._1, e._2)
      }
    }
  }

  class DatesRequest extends BaseRequest {
    def fetchDateStrs: List[RecordDate] = resolveDatesMillis(fetchDatesMillis)

    private def resolveDatesMillis(time: Seq[Long]): List[RecordDate] = {
      val calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT+8"))
      time
        .map({ t =>
          calendar.setTimeInMillis(t)
          val year = calendar.get(Calendar.YEAR)
          val month = calendar.get(Calendar.MONTH) + 1
          val day = calendar.get(Calendar.DAY_OF_MONTH)
          RecordDate(year, month, day)
        })
        .toList
    }

    private def fetchDatesMillis: List[Long] = {
      val page1 = fetchPageDatesMillis(1)
      val obj = gson.fromJson(page1, classOf[JsonObject])
      val pageCount = obj.get("pageCount").getAsInt

      (1 to pageCount)
        .flatMap({ i =>
          val obj = gson.fromJson(fetchPageDatesMillis(i), classOf[JsonObject])
          obj
            .get("list")
            .getAsJsonArray
            .asScala
            .map({ e =>
              e.getAsJsonObject
                .get("date")
                .getAsJsonObject
                .get("time")
                .getAsLong
            })
            .toList
        })
        .toList
    }

    // noinspection DuplicatedCode
    private def fetchPageDatesMillis(page: Int): String = {
      val params =
        s"m=compare&beginDate=2000.1.1&endDate=2022.03.25&cardId=$CARD_ID&page=$page"

      val url = new URL("https://www.sui.com/report.rmi")
      val c = url.openConnection()
      c.setDoOutput(true)
      setHeaders(c)
      writeConnection(c, params.getBytes)

      new String(c.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    }
  }

  class DetailsRequest(dates: Seq[RecordDate]) extends BaseRequest {
    // noinspection DuplicatedCode
    private def fetch(dateStr: String) = {
      val params = s"m=detail&date=$dateStr&cardId=$CARD_ID"

      val url = new URL("https://www.sui.com/report.rmi")
      val c = url.openConnection()
      c.setDoOutput(true)
      setHeaders(c)
      writeConnection(c, params.getBytes)

      new String(c.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    }

    def fetch: List[Record] = {
      val records = dates
        .flatMap({ d =>
          val dateStr = d.toDateString

          println(dateStr)

          val fetched = fetch(dateStr)
          val arr = gson.fromJson(fetched, classOf[JsonArray])
          arr.asScala
            .map({ o =>
              val obj = o.getAsJsonObject
              val memo = Seq(obj.get("memo"))
                .map({ x =>
                  if (x.isJsonNull) "" else x.getAsString
                })
                .head
              val name = obj.get("name").getAsString
              val amount = obj.get("money").getAsBigDecimal
              val amountLong =
                (BigDecimal(amount) * BigDecimal(100)).toLongExact

              val amountEnum = name match {
                case "收入" =>
                  OriginalAmount.Income(amountLong)
                case "支出" =>
                  OriginalAmount.Expense(amountLong)
                case "余额变更" =>
                  OriginalAmount.BalanceChangeIncrement(amountLong)
              }
              Record(d, amountEnum, memo)
            })
            .toList
        })
        .toList

      var sum = 0L
      records.reverse
        .map({ r =>
          val newAmount = r.amount match {
            case OriginalAmount.BalanceChangeIncrement(increment) =>
              val newBalance = sum + increment
              sum = newBalance
              Amount.BalanceChange(newBalance)
            case OriginalAmount.Expense(amount) =>
              sum -= amount
              Amount.Expense(amount)
            case OriginalAmount.Income(amount) =>
              sum += amount
              Amount.Income(amount)
          }
          Record(r.date, newAmount, r.memo)
        })
    }
  }

  abstract class Amount {
    val typeInt: Int

    def getAmount: Long = {
      this match {
        case Amount.Income(amount)            => amount
        case Amount.Expense(amount)           => amount
        case Amount.BalanceChange(newBalance) => newBalance
      }
    }
  }
  object Amount {
    case class Income(amount: Long) extends Amount {
      override val typeInt: Int = 1
    }
    case class Expense(amount: Long) extends Amount {
      override val typeInt: Int = 2
    }
    case class BalanceChange(newBalance: Long) extends Amount {
      override val typeInt: Int = 3
    }
  }

  object OriginalAmount {
    case class Income(amount: Long) extends Amount {
      override val typeInt: Int = 1
    }
    case class Expense(amount: Long) extends Amount {
      override val typeInt: Int = 2
    }
    case class BalanceChangeIncrement(increment: Long) extends Amount {
      override val typeInt: Int = 3
    }
  }

  case class Record(
      date: RecordDate,
      amount: Amount,
      memo: String
  )

  case class RecordDate(
      year: Int,
      month: Int,
      day: Int
  ) {
    def toDateString: String = s"$year-$month-$day"
    def toDateInt: Int = year * 10000 + month * 100 + day
  }
}
