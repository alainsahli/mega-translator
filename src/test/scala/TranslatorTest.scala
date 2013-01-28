import org.scalatest.FunSpec

class TranslatorTest extends FunSpec {

  describe("When adding two integers") {
    it("the sum should be ccalculated") {
      assert(Translator.sum(2, 3) === 5)
    }
  }

}
