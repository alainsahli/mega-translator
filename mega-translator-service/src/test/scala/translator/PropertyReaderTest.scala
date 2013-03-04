package translator

import java.io.ByteArrayInputStream
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.PrivateMethodTester

class PropertyReaderTest extends AssertionsForJUnit with PrivateMethodTester {

  @Test
  def saveEmptyFileReturnsEmptyTranslationDetails() {
    val translationFileDetails = PropertyReader.save(new ByteArrayInputStream(new Array[Byte](0)), "testFile")
    assert(translationFileDetails === TranslationFileDetails("testFile", getHashCode(Set()), ""))
  }

  @Test
  def saveNonEmptyFileReturnsNonEmptyTranslationDetails() {
    val translationFileDetails = PropertyReader.save(new ByteArrayInputStream("key1=(fr)a value\nkey2=(fr)some value".getBytes), "testFile")
    assert(translationFileDetails === TranslationFileDetails("testFile", getHashCode(Set("key1", "key2")), "(fr)"))
  }


  @Test
  def givenAnEmptyStreamReturnAnEmptyList() {
    val emptyInputStream = createInputStream()
    assert(PropertyReader.read(emptyInputStream) === Map())
  }

  @Test
  def givenAStreamWithOnePropertyReturnListOfSizeOne() {
    val key = "key"
    val value = "value"
    val inputStream = createInputStream((key, value))
    assert(PropertyReader.read(inputStream) === Map(key -> value))
  }

  @Test
  def givenASingleValueThereIsNoCommonPrefix() {
    val inputStream = createInputStream(("key1", "*a value"))
    val properties = PropertyReader.read(inputStream)
    assert(PropertyReader.inferPrefix(properties, 1) === None)
  }

  @Test
  def givenValuesWithTooShortCommonPrefixReturnNoCommonPrefix() {
    val inputStream = createInputStream(("key1", "**value"), ("key2", "**other value"))
    val properties = PropertyReader.read(inputStream)
    assert(PropertyReader.inferPrefix(properties, 3) === None)
  }

  @Test
  def givenValuesWithCommonPrefixReturnCommonPrefix() {
    val inputStream = createInputStream(("key1", "(fr)a value"), ("key2", "(fr) another value"))
    val properties = PropertyReader.read(inputStream)
    assert(PropertyReader.inferPrefix(properties, 4) === Some("(fr)"))
  }

  @Test
  def givenValuesWithCommonPrefixReturnsMostCommonPrefix() {
    val inputStream = createInputStream(("key1", "(fr)a value"), ("key2", "(fr) another value"), ("key3", "(fr) yet another value"))
    val properties = PropertyReader.read(inputStream)
    assert(PropertyReader.inferPrefix(properties, 4) === Some("(fr)"))
  }

  @Test
  def givenValueThatEqualsCommonPrefixReturnCommonPrefix() {
    val inputStream = createInputStream(("key1", "(fr)value"), ("key2", "(fr)"))
    val properties = PropertyReader.read(inputStream)
    assert(PropertyReader.inferPrefix(properties, 4) === Some("(fr)"))
  }

  private def createInputStream(keyValuePairs: (String, String)*) = {
    val properties = keyValuePairs.map {
      case (key: String, value: String) => s"$key=$value"
    }.mkString("\n")
    new ByteArrayInputStream(properties.getBytes)
  }

  private def getHashCode(keys: Set[String]): String = {
    val decorateGetHashCode = PrivateMethod[String]('getHashCode)
    PropertyReader invokePrivate decorateGetHashCode(keys)
  }

}
