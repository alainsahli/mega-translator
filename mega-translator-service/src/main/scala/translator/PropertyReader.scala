package translator

import java.io.{FileInputStream, File, FileOutputStream, InputStream}
import scala.collection.JavaConversions.propertiesAsScalaMap
import java.util.Properties
import scala.Option
import scala.Predef._
import scala.Some
import com.google.common.io.{Closeables, ByteStreams}
import java.security.MessageDigest

object PropertyReader {

  def save(inputStream: InputStream, fileName: String): TranslationFileDetails = {
    // todo (etst) decide where to put file on file system
    val tempFile = File.createTempFile("mega-translator", ".tmp")

    copyStream(inputStream, tempFile)
    val properties = read(new FileInputStream(tempFile))
    val hashCode = getHashCode(properties.keySet)
    val prefix = inferPrefix(properties, 3)

    val parent = new File("root", hashCode)
    parent.mkdirs()

    copyStream(new FileInputStream(tempFile), new File(parent, fileName))
    tempFile.delete()

    TranslationFileDetails(fileName, hashCode, prefix.getOrElse(""))
  }

  private def getHashCode(keys: Set[String]): String = {
    val messageDigest = MessageDigest.getInstance("MD5")
    val orderedKeys = keys.toList.sorted.mkString.getBytes("UTF-8")
    val bytes = messageDigest.digest(orderedKeys)
    BigInt(bytes).toString(36)
  }

  private def copyStream(inputStream: InputStream, file: File) {
    var outputStream: FileOutputStream = null
    try {
      outputStream = new FileOutputStream(file)
      ByteStreams.copy(inputStream, outputStream)
    }
    finally {
      Closeables.closeQuietly(inputStream)
      Closeables.closeQuietly(outputStream)
    }
  }

  def read(inputStream: InputStream): Map[String, String] = {
    val properties: Properties = new Properties()

    try {
      properties.load(inputStream)
    } finally {
      Closeables.closeQuietly(inputStream)
    }

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
