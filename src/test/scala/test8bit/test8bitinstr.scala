import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PAdd8Tester extends AnyFlatSpec with ChiselScalatestTester {
  "PAdd8" should "correctly add 32-bit inputs with 8-bit chunks" in {
    test(new PAdd8) { dut =>
      // Test Case 1
      val rs1 = 0x00204060.U // First 32-bit word (Byte 4: 0x00, Byte 3: 0x20, Byte 2: 0x40, Byte 1: 0x60)
      val rs2 = 0x01030507.U // Second 32-bit word (Byte 4: 0x01, Byte 3: 0x03, Byte 2: 0x05, Byte 1: 0x07)
      
      val expectedSum = 0x01234567.U // Expected result 
 
      dut.io.rs1.poke(rs1)
      dut.io.rs2.poke(rs2)
     
      dut.clock.step()

      dut.io.rd.expect(expectedSum)

      // Test Case 2 - with  carry between bytes
      val rs1_carry = "hFF_00_F0_F0".U // First 32-bit word
      val rs2_carry = "h01_FF_10_11".U // Second 32-bit word

      val expectedSum_carry = "h01_00_01_01".U // Expected result with carry handling
      
      dut.io.rs1.poke(rs1_carry)
      dut.io.rs2.poke(rs2_carry)

      dut.clock.step()

      dut.io.rd.expect(expectedSum_carry)
    }
  }
}
