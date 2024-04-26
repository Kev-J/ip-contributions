/*
 *
 * A fixed point IIR filter module.
 *
 * Author: Kevin Joly (kevin.joly@armadeus.com)
 *
 */

package chisel.lib.iirfilter

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._

/*
 * IIR filter module
 *
 * Apply filter on input samples passed by ready/valid handshake. Numerators
 * and denominators are to be set prior to push any input sample.
 *
 * All the computations are done in fixed point. The user should manage the
 * input decimal width by himself. Output width should be sufficient in order
 * to not overflow (i.e. in case of overshoot). A minimum output width of
 * inputWidht+coefWidth+log2Ceil(numeratorNum + denominatorNum) + 1 is
 * requested.
 *
 */
class IIRFilter(
  inputWidth:       Int,
  coefWidth:        Int,
  coefDecimalWidth: Int,
  outputWidth:      Int,
  numeratorNum:     Int,
  denominatorNum:   Int)
    extends Module {
  val io = IO(new Bundle {
    /*
     * Input samples
     */
    val input = Flipped(Decoupled(SInt(inputWidth.W)))
    /*
     * Numerator's coefficients b[0], b[1], ...
     */
    val num = Input(Vec(numeratorNum, SInt(coefWidth.W)))
    /*
     * The first coefficient of the denominator should be omitted and should be a[0] == 1.
     * a[1], a[2], ...
     */
    val den = Input(Vec(denominatorNum, SInt(coefWidth.W)))
    /*
     * Filtered samples. Fixed point format is:
     * (outputWidth-coefDecimalWidth).coefDecimalWidth
     * Thus, output should be right shifted to the right of 'coefDecimalWidth' bits.
     */
    val output = Decoupled(SInt(outputWidth.W))
  })

  assert(coefWidth >= coefDecimalWidth)

  val minOutputWidth = inputWidth + coefWidth + log2Ceil(numeratorNum + denominatorNum) + 1
  assert(outputWidth >= minOutputWidth)

  val coefNumAddr = RegInit(1.U(math.max(numeratorNum, denominatorNum).W))
  val coefNumGet = RegInit(0.U(math.max(numeratorNum, denominatorNum).W))
  val coefNumFetch = RegInit(0.U(math.max(numeratorNum, denominatorNum).W))
  val coefNumMult = RegInit(0.U(math.max(numeratorNum, denominatorNum).W))

  object IIRFilterState extends ChiselEnum {
    val Idle, ComputeNum, ComputeDen, ShiftOutputSum, ComputeOutput, Valid = Value
  }

  val state = RegInit(IIRFilterState.Idle)

  switch(state) {
    is(IIRFilterState.Idle) {
      when(io.input.valid) {
        state := IIRFilterState.ComputeNum
      }
    }
    is(IIRFilterState.ComputeNum) {
      when(coefNumMult(numeratorNum - 1)) {
        state := IIRFilterState.ComputeDen
      }
    }
    is(IIRFilterState.ComputeDen) {
      when(coefNumMult(denominatorNum - 1)) {
        state := IIRFilterState.ShiftOutputSum
      }
    }
    is(IIRFilterState.ShiftOutputSum) {
      state := IIRFilterState.ComputeOutput
    }
    is(IIRFilterState.ComputeOutput) {
      state := IIRFilterState.Valid
    }
    is(IIRFilterState.Valid) {
      when(io.output.ready) {
        state := IIRFilterState.Idle
      }
    }
  }

  // Input memory
  val inputReg = RegInit(0.S(inputWidth.W))
  val inputMem = Mem(numeratorNum, SInt(inputWidth.W))
  val inputMemReadAddr = RegInit(0.U(log2Ceil(numeratorNum).W))
  val inputMemWriteAddr = RegInit(0.U(log2Ceil(numeratorNum).W))
  val inputMemAddr = Wire(UInt(log2Ceil(numeratorNum).W))
  val inputMemOut = Wire(SInt(inputWidth.W))
  val inputRdWr = inputMem(inputMemAddr)

  when(state === IIRFilterState.ComputeNum) {
    inputMemAddr := inputMemReadAddr
  }.otherwise {
    inputMemAddr := inputMemWriteAddr
  }

  inputMemOut := DontCare

  when((state === IIRFilterState.Idle) && io.input.valid) {
    inputRdWr := io.input.bits // Delayed write
  }.otherwise {
    inputMemOut := inputRdWr
  }

  when((state === IIRFilterState.Idle) && io.input.valid) {
    when(inputMemWriteAddr === 0.U) {
      inputMemWriteAddr := (numeratorNum - 1).U
    }.otherwise {
      inputMemWriteAddr := inputMemWriteAddr - 1.U
    }
  }

  // Output memory
  val outputReg = RegInit(0.S(outputWidth.W))
  val outputMem = Mem(denominatorNum, SInt(outputWidth.W))
  val outputMemReadAddr = RegInit(0.U(log2Ceil(denominatorNum).W))
  val outputMemWriteAddr = RegInit(0.U(log2Ceil(denominatorNum).W))
  val outputMemAddr = Wire(UInt(log2Ceil(denominatorNum).W))
  val outputMemOut = Wire(SInt(outputWidth.W))
  val outputRdWr = outputMem(outputMemAddr)

  when(state === IIRFilterState.ComputeDen) {
    outputMemAddr := outputMemReadAddr
  }.otherwise {
    outputMemAddr := outputMemWriteAddr
  }

  outputMemOut := DontCare

  when((state === IIRFilterState.Valid) && (RegNext(state) =/= IIRFilterState.Valid)) {
    when(outputMemWriteAddr === 0.U) {
      outputMemWriteAddr := (denominatorNum - 1).U
    }.otherwise {
      outputMemWriteAddr := outputMemWriteAddr - 1.U
    }
    outputRdWr := outputReg
  }.otherwise {
    outputMemOut := outputRdWr
  }

  // Stage 1: Set addresses
  when(state === IIRFilterState.ComputeNum) {
    when(coefNumMult(numeratorNum - 1)) {
      coefNumAddr := 1.U
    }.otherwise {
      coefNumAddr := coefNumAddr << 1
    }
  }.elsewhen(state === IIRFilterState.ComputeDen) {
    coefNumAddr := coefNumAddr << 1
  }.otherwise {
    coefNumAddr := 1.U
  }

  when(state === IIRFilterState.ComputeNum) {
    when(inputMemReadAddr === (numeratorNum - 1).U) {
      inputMemReadAddr := 0.U
    }.otherwise {
      inputMemReadAddr := inputMemReadAddr + 1.U
    }
  }.elsewhen(state === IIRFilterState.ComputeDen) {
    when(outputMemAddr === (denominatorNum - 1).U) {
      outputMemReadAddr := 0.U
    }.otherwise {
      outputMemReadAddr := outputMemReadAddr + 1.U
    }
  }.elsewhen(state === IIRFilterState.Idle) {
    inputMemReadAddr := inputMemWriteAddr
  }.elsewhen((state === IIRFilterState.Valid) && (RegNext(state) =/= IIRFilterState.Valid)) {
    outputMemReadAddr := outputMemWriteAddr
  }

  // Stage 2: get coef num
  when(state === IIRFilterState.ComputeNum) {
    when(coefNumMult(numeratorNum - 1)) {
      coefNumGet := 0.U
    }.otherwise {
      coefNumGet := coefNumAddr
    }
  }.elsewhen(state === IIRFilterState.ComputeDen) {
    when(coefNumMult(denominatorNum - 1)) {
      coefNumGet := 0.U
    }.otherwise {
      coefNumGet := coefNumAddr
    }
  }.otherwise {
    coefNumGet := 0.U
  }

  val coefNum = RegInit(0.U(log2Ceil(math.max(numeratorNum, denominatorNum)).W))
  coefNum := PriorityEncoder(coefNumAddr)

  val multInRegTmp = RegInit(0.S(outputWidth.W))

  when(state === IIRFilterState.ComputeNum) {
    multInRegTmp := inputMemOut
  }.elsewhen(state === IIRFilterState.ComputeDen) {
    multInRegTmp := outputMemOut
  }

  // Stage 3: Fetch operands
  when(state === IIRFilterState.ComputeNum) {
    when(coefNumMult(numeratorNum - 1)) {
      coefNumFetch := 0.U
    }.otherwise {
      coefNumFetch := coefNumGet
    }
  }.elsewhen(state === IIRFilterState.ComputeDen) {
    when(coefNumMult(denominatorNum - 1)) {
      coefNumFetch := 0.U
    }.otherwise {
      coefNumFetch := coefNumGet
    }
  }.otherwise {
    coefNumFetch := 0.U
  }

  val multInReg = RegInit(0.S(outputWidth.W))
  val multCoefReg = RegInit(0.S(coefWidth.W))
  multInReg := multInRegTmp
  when(state === IIRFilterState.ComputeNum) {
    multCoefReg := io.num(coefNum)
  }.elsewhen(state === IIRFilterState.ComputeDen) {
    multCoefReg := io.den(coefNum)
  }

  // Stage 4: Get multiplication output
  when(state === IIRFilterState.ComputeNum) {
    when(coefNumMult(numeratorNum - 1)) {
      coefNumMult := 0.U
    }.otherwise {
      coefNumMult := coefNumFetch
    }
  }.elsewhen(state === IIRFilterState.ComputeDen) {
    when(coefNumMult(denominatorNum - 1)) {
      coefNumMult := 0.U
    }.otherwise {
      coefNumMult := coefNumFetch
    }
  }.otherwise {
    coefNumMult := 0.U
  }

  val multOutReg = RegInit(0.S((outputWidth + coefWidth).W))

  multOutReg := multInReg * multCoefReg

  // Stage 5: Accumulate
  val inputSum = RegInit(0.S((inputWidth + coefWidth + log2Ceil(numeratorNum)).W))
  val outputSum = RegInit(0.S((outputWidth + coefWidth + log2Ceil(denominatorNum)).W))
  when(state === IIRFilterState.ComputeNum) {
    when(coefNumMult(0)) {
      inputSum := multOutReg
    }.otherwise {
      inputSum := inputSum +& multOutReg
    }
  }.elsewhen(state === IIRFilterState.ComputeDen) {
    when(coefNumMult(0)) {
      outputSum := multOutReg
    }.otherwise {
      outputSum := outputSum +& multOutReg
    }
  }.elsewhen(state === IIRFilterState.ShiftOutputSum) {
    outputSum := outputSum >> coefDecimalWidth
  }.elsewhen(state === IIRFilterState.Idle) {
    inputSum := 0.S
    outputSum := 0.S
  }

  val outputDiff = Wire(SInt(outputWidth.W))
  when(state === IIRFilterState.ComputeOutput) {
    outputReg := outputDiff
  }

  outputDiff := inputSum -& outputSum

  io.input.ready := state === IIRFilterState.Idle
  io.output.valid := state === IIRFilterState.Valid
  io.output.bits := outputReg
}
