/*
 * A very simple test collection for IIRFilter module.
 *
 * See README.md for license details.
 */

package chisel.lib.iirfilter

import chisel3._
import chisel3.experimental.VecLiterals._
import chisel3.util.log2Ceil
import chiseltest._

import org.scalatest.flatspec.AnyFlatSpec

class IIRFilterNumeratorTest extends AnyFlatSpec with ChiselScalatestTester {
  "IIRFilter numerator" should "work" in {

    val inputWidth = 4
    val coefWidth = 3
    val coefDecimalWidth = 0
    val num = Seq(2, 1, 0, 3)
    val den = Seq(0, 0)
    val outputWidth = inputWidth + coefWidth + log2Ceil(num.length + den.length) + 1

    test(
      new IIRFilter(
        inputWidth = inputWidth,
        coefWidth = coefWidth,
        coefDecimalWidth = coefDecimalWidth,
        outputWidth = outputWidth,
        numeratorNum = num.length,
        denominatorNum = den.length
      )
    ) { dut =>
      val inputDriver = new DecoupledDriver(dut.io.input)
      inputDriver.setSinkClock(dut.clock) // TODO remove me in Chisel > 6.0.x
      inputDriver.setSourceClock(dut.clock) // TODO remove me in Chisel > 6.0.x

      val outputDriver = new DecoupledDriver(dut.io.output)
      outputDriver.setSinkClock(dut.clock) // TODO remove me in Chisel > 6.0.x
      outputDriver.setSourceClock(dut.clock) // TODO remove me in Chisel > 6.0.x

      dut.io.num.poke(Vec.Lit(num.map(_.S(coefWidth.W)): _*))
      dut.io.den.poke(Vec.Lit(den.map(_.S(coefWidth.W)): _*))

      dut.io.output.ready.poke(true.B)

      // Sample 1: Write 1. on input port
      inputDriver.enqueue(1.S)
      outputDriver.expectDequeue(2.S)

      // Sample 2: Write 1. on input port
      inputDriver.enqueue(1.S)
      outputDriver.expectDequeue(3.S)

      // Sample 3: Write 0. on input port
      inputDriver.enqueue(0.S)
      outputDriver.expectDequeue(1.S)

      // Sample 4: Write 0. on input port
      inputDriver.enqueue(0.S)
      outputDriver.expectDequeue(3.S)
    }
  }
}

class IIRFilterDenominatorTest extends AnyFlatSpec with ChiselScalatestTester {
  "IIRFilter denominator" should "work" in {

    val inputWidth = 4
    val coefWidth = 3
    val coefDecimalWidth = 0
    val num = Seq(1, 0)
    val den = Seq(2, 3, 1)
    val outputWidth = inputWidth + coefWidth + log2Ceil(num.length + den.length) + 1

    test(
      new IIRFilter(
        inputWidth = inputWidth,
        coefWidth = coefWidth,
        coefDecimalWidth = coefDecimalWidth,
        outputWidth = outputWidth,
        numeratorNum = num.length,
        denominatorNum = den.length
      )
    ) { dut =>
      val inputDriver = new DecoupledDriver(dut.io.input)
      inputDriver.setSinkClock(dut.clock) // TODO remove me in Chisel > 6.0.x
      inputDriver.setSourceClock(dut.clock) // TODO remove me in Chisel > 6.0.x

      val outputDriver = new DecoupledDriver(dut.io.output)
      outputDriver.setSinkClock(dut.clock) // TODO remove me in Chisel > 6.0.x
      outputDriver.setSourceClock(dut.clock) // TODO remove me in Chisel > 6.0.x

      dut.io.num.poke(Vec.Lit(num.map(_.S(coefWidth.W)): _*))
      dut.io.den.poke(Vec.Lit(den.map(_.S(coefWidth.W)): _*))

      dut.io.output.ready.poke(true.B)

      // Sample 1: Write 1. on input port
      inputDriver.enqueue(1.S)
      outputDriver.expectDequeue(1.S)

      // Sample 2: Write 1. on input port
      inputDriver.enqueue(1.S)
      outputDriver.expectDequeue(-1.S)

      // Sample 3: Write 1. on input port
      inputDriver.enqueue(1.S)
      outputDriver.expectDequeue(0.S)

      // Sample 4: Write 1. on input port
      inputDriver.enqueue(1.S)
      outputDriver.expectDequeue(3.S)

      // Sample 5: Write 0. on input port
      inputDriver.enqueue(0.S)
      outputDriver.expectDequeue(-5.S)
    }
  }
}

class IIRFilterReadyTest extends AnyFlatSpec with ChiselScalatestTester {
  "IIRFilter" should "work" in {

    val inputWidth = 4
    val coefWidth = 3
    val coefDecimalWidth = 0
    val num = Seq(1, 2, 0)
    val den = Seq(0, 0)
    val outputWidth = inputWidth + coefWidth + log2Ceil(num.length + den.length) + 1

    test(
      new IIRFilter(
        inputWidth = inputWidth,
        coefWidth = coefWidth,
        coefDecimalWidth = coefDecimalWidth,
        outputWidth = outputWidth,
        numeratorNum = num.length,
        denominatorNum = den.length
      )
    ) { dut =>
      val inputDriver = new DecoupledDriver(dut.io.input)
      inputDriver.setSinkClock(dut.clock) // TODO remove me in Chisel > 6.0.x
      inputDriver.setSourceClock(dut.clock) // TODO remove me in Chisel > 6.0.x

      val outputDriver = new DecoupledDriver(dut.io.output)
      outputDriver.setSinkClock(dut.clock) // TODO remove me in Chisel > 6.0.x
      outputDriver.setSourceClock(dut.clock) // TODO remove me in Chisel > 6.0.x

      dut.io.num.poke(Vec.Lit(num.map(_.S(coefWidth.W)): _*))
      dut.io.num.poke(Vec.Lit(num.map(_.S(coefWidth.W)): _*))
      dut.io.den.poke(Vec.Lit(den.map(_.S(coefWidth.W)): _*))

      dut.io.output.ready.poke(false.B)

      // Sample 1: Write 1. on input port
      inputDriver.enqueue(1.S)

      outputDriver.waitForValid()

      val extraClockCycles = 10
      for (i <- 0 until extraClockCycles) {
        dut.io.output.valid.expect(true.B)
        dut.io.input.ready.expect(false.B)
        dut.clock.step(1)
      }

      dut.io.output.ready.poke(true.B)

      outputDriver.expectDequeue(1.S)
    }
  }
}
