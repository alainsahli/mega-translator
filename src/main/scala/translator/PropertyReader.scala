package translator

import java.io.{FileInputStream, File, FileOutputStream, InputStream}
import scala.collection.JavaConversions.propertiesAsScalaMap
import java.util.Properties
import scala.Option
import scala.Predef._
import scala.Some
import com.google.common.io.{Files, ByteStreams}
import java.security.MessageDigest

object PropertyReader {

  def save(inputStream: InputStream, fileName: String): TranslationFileDetails = {
    // todo (etst) decide where to put file on file system
    val file = new File(fileName)

    copyStream(inputStream, file)
    val properties = read(new FileInputStream(file))
    val hashCode = getHashCode(properties.keySet)
    val prefix = inferPrefix(properties, 3)

    val parent = new File("root", hashCode)
    parent.mkdirs()

    Files.move(file, new File(parent, fileName))

    TranslationFileDetails(fileName, hashCode, prefix.getOrElse(""))
  }

  private def getHashCode(keys: Set[String]): String = {
    val messageDigest = MessageDigest.getInstance("MD5")
    val bytes = messageDigest.digest(keys.mkString.getBytes("UTF-8"))
    BigInt(bytes).toString(36)
  }

  private def copyStream(inputStream: InputStream, file: File) {
    ByteStreams.copy(inputStream, new FileOutputStream(file))
  }

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
        if (elements.size < 2 || result.isDefined && values.size != elements.size) result
        else inferPrefix(elements, minLength + 1, Some(prefix))
      }
    }

    inferPrefix(properties.values, minLength, None)
  }

}

case class TranslationFileDetails(fileName: String, keysHashCode: String, prefix: String)
