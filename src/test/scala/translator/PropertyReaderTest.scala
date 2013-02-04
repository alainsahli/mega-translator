package translator

import java.io.ByteArrayInputStream
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class PropertyReaderTest extends AssertionsForJUnit {

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
  def givenValuesWithCommonPrefixReturnCommonPrefix() {
    val inputStream = createInputStream(("key1", "(fr)a value"), ("key2", "(fr) another value"))
    val properties = PropertyReader.read(inputStream)
    assert(PropertyReader.inferPrefix(properties) === Some("(fr)"))
  }

  @Test
  def givenValueThatEqualsCommonPrefixReturnCommonPrefix() {
    val inputStream = createInputStream(("key1", "(fr)value"), ("key2", "(fr)"))
    val properties = PropertyReader.read(inputStream)
    assert(PropertyReader.inferPrefix(properties) === Some("(fr)"))
  }

  @Test
  def givenEmptyValueReturnEmptyCommonPrefix() {
    val inputStream = createInputStream(("key1", ""))
    val properties = PropertyReader.read(inputStream)
    assert(PropertyReader.inferPrefix(properties) === None)
  }

  private def createInputStream(keyValuePairs: (String, String)*) = {
    val properties = keyValuePairs.map {
      case (key: String, value: String) => s"$key=$value"
    }.mkString("\n")
    new ByteArrayInputStream(properties.getBytes)
  }

}
