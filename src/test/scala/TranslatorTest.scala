import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class TranslatorTest extends AssertionsForJUnit {

  //  describe("When adding two integers") {
  //    it("the sum should be calculated") {
  //      assert(Translator.sum(2, 3) === 5)
  //    }
  //  }

  @Test
  def sum() {
    assert(Translator.sum(2, 3) === 5)
  }

}
