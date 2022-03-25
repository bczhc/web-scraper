package pers.zhc

import pers.zhc.jni.sqlite.SQLite3

/** @author
  *   bczhc
  */
class Database(path: String) {
  val db: SQLite3 = SQLite3.open(path)

  db.exec(
    """CREATE TABLE IF NOT EXISTS record
(
    "date" TEXT NOT NULL,
    -- income: 1; expense: 2; balance change: 3
    "type" INTEGER,
    amount TEXT NOT NULL,
    memo   TEXT NOT NULL
)"""
  )

  private val insertStatement = db.compileStatement(
    "INSERT INTO record (\"date\", \"type\", amount, memo) VALUES (?, ?, ?, ?)"
  )

  def insert(record: Main.Record): Unit = {
    insertStatement.reset()
    insertStatement.bind(
      Array(
        record.date.toDateString,
        record.amount.typeInt,
        (BigDecimal(record.amount.getAmount) / BigDecimal(100))
          .setScale(2)
          .toString(),
        record.memo
      )
    )
    insertStatement.step()
  }

  def close(): Unit = {
    insertStatement.release()
    db.close()
  }
}
