package com.mycujoo

import scala.io.Source
import play.api.libs.json._
import java.io._

object JsonParser {
  import com.mycujoo.Type._

  def main(args: Array[String]): Unit = {
    val template = "https://s3-eu-west-1.amazonaws.com/mycujoo-assignments/be-assignment/[NAME_OF_THE_SUBJECT].json"

    val listOfTopics = parsingFromUrl("https://s3-eu-west-1.amazonaws.com/mycujoo-assignments/be-assignment/subjects.json")
      .as[List[String]]

    val preparedUrls = listOfTopics.map(template.replaceAll("\\[NAME_OF_THE_SUBJECT\\]", _))

    val sql = preparedUrls.flatMap { url =>
      val schema = parsingFromUrl(url) \ "schema"
      if(schema.isDefined) {
        schema.get match {
          case str:JsString =>
            val record = Json.parse(str.as[String])
            Some(recordToSql(record.as[Record]))
          case _ =>
            None
        }
      } else None
    }.mkString("\n")

    val pw = new PrintWriter(new File("src/main/resources/create.sql"))
    pw.write(sql)
    pw.close
  }

  private[mycujoo] def parsingFromUrl(url: String) = {
    val html = Source.fromURL(url)
    Json.parse(html.mkString)
  }

  private[mycujoo] def recordToSql(record: Record) = {
    val template = "CREATE TABLE <name> (<fields>);"
    val allFields = record.fields.map { f =>
      val name = f.`type` match {
        case enum: MyEnum =>
          val smbls = enum.symbols.map(str => s""""$str"""").mkString(",")
          s"${enum.name}($smbls)"
        case other =>
          other.name
      }
      val nullable = if (f.`type`.notNull) " NOT NULL" else ""
      s"${f.name} $name$nullable"
    }.mkString(",")

    template.replace("<name>", record.name).replace("<fields>", allFields)
  }
}