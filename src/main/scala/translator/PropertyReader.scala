package translator

import java.io.InputStream
import scala.collection.JavaConversions.propertiesAsScalaMap
import java.util.Properties
import scala.Option
import scala.Predef._
import scala.Some

object PropertyReader {

  def read(inputStream: InputStream): Map[String, String] = {
    val properties: Properties = new Properties()
    properties.load(inputStream)
    Map(properties.toSeq: _*)
  }

  def inferPrefix(properties: Map[String, String], minLength: Int): Option[String] = {
    def inferPrefix(values: Iterable[String], minLength: Int, result: Option[String]): Option[String] = {
      val matches = values.filter(_.length >= minLength)
      if (matches.isEmpty) result
      else {
        val valuesByPrefix = matches.groupBy(_ take minLength)
        val valuesOfMostCommonPrefix = valuesByPrefix.maxBy(_._2.size)
        if (valuesOfMostCommonPrefix._2.size < 2) result
        else inferPrefix(valuesByPrefix(valuesOfMostCommonPrefix._1), minLength + 1, Some(valuesOfMostCommonPrefix._1))
      }
    }

    inferPrefix(properties.values, minLength, None)
  }

}
