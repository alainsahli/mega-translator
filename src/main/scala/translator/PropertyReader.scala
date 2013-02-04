package translator

import java.io.InputStream
import scala.collection.JavaConversions.propertiesAsScalaMap
import java.util.Properties

object PropertyReader {

  def read(inputStream: InputStream): Map[String, String] = {
    val properties: Properties = new Properties()
    properties.load(inputStream)
    Map(properties.toSeq: _*)
  }

  def inferPrefix(properties: Map[String, String]): Option[String] = {
    val valuesToSearch = properties.filter(_._2.length >= 3)
    if (valuesToSearch.isEmpty) None
    else {
      val valuePrefixes = valuesToSearch.values.groupBy(_.substring(0, 3))
      val mostCommonPrefix = valuePrefixes.maxBy(_._2.size)._1
      val valuesWithMostCommonPrefix = valuesToSearch.filter(_._2.startsWith(mostCommonPrefix)).map(_._2)

      Some(findCommonPrefix(valuesWithMostCommonPrefix, mostCommonPrefix))
    }
  }

  def findCommonPrefix(values: Iterable[String], prefix: String): String = {
    if (values.filter(_.startsWith(prefix)).size == values.size) {
      val head = values.head
      if (head.size == prefix.size) prefix
      else findCommonPrefix(values, values.head.substring(0, prefix.length + 1))
    }
    else
      prefix.substring(0, prefix.length - 1)
  }
}
