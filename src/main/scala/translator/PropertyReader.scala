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
        val (prefix, elements) = valuesByPrefix.maxBy(_._2.size)
        if (elements.size < 2) result
        else inferPrefix(valuesByPrefix(prefix), minLength + 1, Some(prefix))
      }
    }

    inferPrefix(properties.values, minLength, None)
  }

}
