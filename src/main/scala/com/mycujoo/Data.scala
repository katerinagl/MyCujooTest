package com.mycujoo

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Field(name: String, `type`: Type)

case class Record(`type`: String, name: String, fields: Seq[Field])

sealed trait Type {
  val name = ""
  val notNull: Boolean = true
}

object Type {
  val allPossibleTypes: Array[String] = Array(MyInt(), MyString(), MyLong(), MyBoolean()).map(_.name)

  private def matchTypeFromString(str: String, notNull: Boolean): Type = {
    str match {
      case "int" => MyInt(notNull)
      case "boolean" => MyBoolean(notNull)
      case "long" => MyLong(notNull)
      case _ => MyString(notNull)
    }
  }

  implicit object typeReader extends Reads[Type] {
    def reads(json: JsValue): JsResult[Type] = json match {
      case JsString(s) => JsSuccess(matchTypeFromString(s, true))
      case JsObject(obj) =>
        obj("type") match {
          case JsString(value) if value == "enum" =>
            val symbolsJson = obj("symbols").as[Array[String]]
            JsSuccess(MyEnum(symbols = symbolsJson))
          case _ =>
            JsSuccess(MyString())
        }
      case JsArray(arr) =>
        val allStrings = arr.map {
          case a: JsString => a.as[String]
          case _: JsObject => "string"
        }
        val notNull = !allStrings.contains("null")
        val str = allStrings.find(Type.allPossibleTypes.contains).getOrElse("string")
        JsSuccess(matchTypeFromString(str, notNull))

      case _ => JsSuccess(MyString())
    }
  }

  implicit val fieldsReader: Reads[Field] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "type").read[Type]
    ) (Field.apply _)

  implicit val recordReader: Reads[Record] = (
    (JsPath \ "type").read[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "fields").read[Seq[Field]]
    ) (Record.apply _)
}

case class MyInt(override val notNull: Boolean = true) extends Type {
  override val name = "int"
}

case class MyString(override val notNull: Boolean = true) extends Type {
  override val name = "varchar(255)"
}

case class MyLong(override val notNull: Boolean = true) extends Type {
  override val name = "long"
}

case class MyBoolean(override val notNull: Boolean = true) extends Type {
  override val name = "boolean"
}

case class MyEnum(override val notNull: Boolean = true, symbols: Array[String]) extends Type {
  override val name = "enum"
}