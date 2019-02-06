package com.mycujoo

import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.Json

import scala.io.Source

class DataSpec extends WordSpec with Matchers {

  import Type._

  "Record reader " should {
    val file = Source.fromFile("src/test/resources/test.json")
    val json = Json.parse(file.mkString)

    val record = json.as[Record]

    val expected = Record("record", "team", Vector(Field("id", MyInt()), Field("name", MyString()),
      Field("gender", MyEnum(symbols = Array("MEN", "WOMEN", "MIXED"))), Field("active", MyBoolean()),
      Field("entity", MyString()), Field("short_name", MyLong(false)), Field("seasons", MyString())))

    "parse json correctly " in {
      record.name shouldBe expected.name
      record.`type` shouldBe expected.`type`
      record.fields.length shouldBe expected.fields.length
      record.fields.map(_.name) should contain allElementsOf expected.fields.map(_.name)
      record.fields.map(_.`type`.name) should contain allElementsOf expected.fields.map(_.`type`.name)
    }

    "parse enum symbols correctly " in {
      record.fields.filter(_.name == "gender").head.`type` shouldBe a[MyEnum]
      record.fields.filter(_.name == "gender").head.`type`.asInstanceOf[MyEnum].symbols should contain allElementsOf Array("MEN", "WOMEN", "MIXED")
    }

    "parse nullable correctly " in {
      record.fields.filter(_.name == "gender").head.`type`.notNull === true
      record.fields.filter(_.name == "short_name").head.`type`.notNull === false
    }
  }

  "Convert sql from record " should {
    "generate correct sql " in {
      val record = Record("record", "team", Vector(Field("id", MyInt()), Field("name", MyString()),
        Field("gender", MyEnum(symbols = Array("MEN", "WOMEN", "MIXED"))), Field("active", MyBoolean()),
        Field("entity", MyString()), Field("short_name", MyLong(false)), Field("seasons", MyString())))

      val expectedSql = "CREATE TABLE team (id int NOT NULL,name varchar(255) NOT NULL,gender enum(\"MEN\",\"WOMEN\",\"MIXED\") NOT NULL,active boolean NOT NULL,entity varchar(255) NOT NULL,short_name long,seasons varchar(255) NOT NULL);"

      val sqlString = JsonParser.recordToSql(record)

      sqlString shouldBe expectedSql
    }
  }
}
