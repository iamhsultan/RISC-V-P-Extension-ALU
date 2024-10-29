import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import Bits8._
import scala.util.Random

class PextALUTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "PextALU"

  it should "perform all operations with random inputs and display waveforms" in {
    test(new PextALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      // Map ALUops to names for VCD annotation and logging
      val operationMap = Map(
        ALUops.PADDB -> "PADDB",
        ALUops.PAADDB -> "PAADDB",
        ALUops.PAADDUB -> "PAADDUB",
        ALUops.PSADDUB -> "PSADDUB",
        ALUops.PADDW -> "PADDW",
        ALUops.PSUBB -> "PSUBB",
        ALUops.PASUBB -> "PASUBB",
        ALUops.PASUBUB -> "PASUBUB",
        ALUops.PSSUBB -> "PSSUBB",
        ALUops.PSSUBUB -> "PSSUBUB"
      )

      // Test each operation
      for ((operation, name) <- operationMap) {
        // Generate random 32-bit values for Rs1 and Rs2
        val rs1Val = Random.nextInt().abs.toLong.U(32.W)
        val rs2Val = Random.nextInt().abs.toLong.U(32.W)

        // Apply inputs to DUT
        dut.io.Rs1.poke(rs1Val)
        dut.io.Rs2.poke(rs2Val)
        dut.io.operation.poke(operation)

        // Step the clock and observe the output
        dut.clock.step(1)

        // Log inputs and outputs in hexadecimal format
        println(s"Testing $operation with Rs1 = ${rs1Val}, Rs2 = ${rs2Val}")
        println(s"Result: Rd = ${dut.io.Rd.peek().litValue}, vxsat = ${dut.io.vxsat.peek().litValue}")

        // Step the clock for clarity in waveform
        dut.clock.step(2)
      }
    }
  }
}



///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


