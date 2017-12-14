package demo

import demo.Bowling.score
import org.scalatest.{FunSpec, Matchers}

class BowlingTest extends FunSpec with Matchers {

  describe("Bowler Score Calculation ") {

    it("Given X X X X X X X X X X X X (12 rolls: 12 strikes) = 10 frames * 30 points = 300") {
      score("XXXXXXXXXXXX") shouldBe 300
    }

    it("Given 9- 9- 9- 9- 9- 9- 9- 9- 9- 9- (20 rolls: 10 pairs of 9 and miss) = 10 frames * 9 points = 90") {
      score("9-9-9-9-9-9-9-9-9-9-") shouldBe 90
    }

    it("Given -- -- -- -- -- -- -- -- -- -- (20 rolls: 10 pairs of 0 ) = 0 frames * 0 points = 0") {
      score("--------------------") shouldBe 0
    }

    it("Given 54 54 54 54 54 54 54 54 54 54 (20 rolls: 10 pairs of 9 ) = 10 frames * 9 points = 90") {
      score("54545454545454545454") shouldBe 90
    }

    it("Given X -- -/ -- X -1 1/ -1 X 1- (17 rolls: 7 pairs ) = 10+0+10+0+11+1+10+1+11+1 points = 55") {
      score("X---/--X-11/-1X1-") shouldBe 55
    }

    it("Given 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5 (21 rolls: 10 pairs of 5 and spare, with a final 5) = 10 frames * 15 points = 150") {
      score("5/5/5/5/5/5/5/5/5/5/5") shouldBe 150
    }

    it("Given -/ -/ -/ -/ -/ -/ -/ -/ -/ -/ X (21 rolls: 10 pairs of nill and a spare, with a final strike) = 10 frames * 15 points = 110") {
      score("-/-/-/-/-/-/-/-/-/-/X") shouldBe 110
    }


    it("should throw IllegalArgumentException if a character different than a number or X-/ is found") {
      intercept[IllegalArgumentException] {
        score("-/K/-/-/-/-/-/-/-/-/V")
      }
    }
    it("should throw IllegalArgumentException if a string less than 10 characters is provided.") {
      intercept[IllegalArgumentException] {
        score("-/-/-/-/-")
      }
    }
    it("should throw IllegalArgumentException if a string more  than 22 characters is provided.") {
      intercept[IllegalArgumentException] {
        score("-/-/-/-/-/-/-/-/54545454545454545454")
      }
    }

  }
}