import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

///============================TESTER====PAADD.B -- SIMD 8bit Addition=========================///

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

///============================TESTER====PAADD.B -- SIMD 8-bit Signed Averaging Addition=========================///

class PAADD8Tester extends AnyFlatSpec with ChiselScalatestTester {
  "PAADD8" should "correctly compute signed averaging addition" in {
    test(new PAADD8) { dut =>
      // Test Case 1: Rs1 = 0x7F_00_80_00, Rs2 = 0x7F_00_80_00
      dut.io.Rs1.poke("h7F008000".U) // Rs1 = 127, 0, -128, 0
      dut.io.Rs2.poke("h7F008000".U) // Rs2 = 127, 0, -128, 0
      dut.clock.step()
      // Expected Rd = (127+127)>>1, (0+0)>>1, (-128-128)>>1, (0+0)>>1 = 127, 0, -128, 0 => Rd = 0x7F008000
      dut.io.Rd.expect("h7F008000".U)

      // Test Case 2: Rs1 = 0xFF_01_02_80, Rs2 = 0x00_FF_7E_7F
      dut.io.Rs1.poke("hFF010280".U) // Rs1 = -1, 1, 2, -128
      dut.io.Rs2.poke("h00FF7E7F".U) // Rs2 = 0, -1, 126, 127
      dut.clock.step()
      // Expected Rd = (-1+0)>>1, (1-1)>>1, (2+126)>>1, (-128+127)>>1 => -1, 0, 64, -1 => Rd = 0xFF0040FF
      dut.io.Rd.expect("hFF0040FF".U)

      // Test Case: Normal Addition and Averaging
      dut.io.Rs1.poke("h12_34_56_78".U) 
      dut.io.Rs2.poke("h10_20_30_40".U)  
      dut.clock.step()
      // Expected Rd = 0x22 >> 1 = 0x11, 0x54 >> 1 = 0x2A, 0x86 >> 1 = 0x43, 0xB8 >> 1 = 0x5C
      dut.io.Rd.expect("h11_2A_43_5C".U)

      // Test Case: Addition with Overflow
      dut.io.Rs1.poke("h7F_7F_7F_7F".U) 
      dut.io.Rs2.poke("h7F_7F_7F_7F".U) 
      dut.clock.step()
      // Expected 8bit elements in Rd  = 0xFE >> 1 = 0x7F
      dut.io.Rd.expect("h7F_7F_7F_7F".U)

      // Test Case: Addition with Negative Values
      dut.io.Rs1.poke("h80_80_80_80".U)  
      dut.io.Rs2.poke("h80_80_80_80".U) 
      dut.clock.step()
      // Expected Rd  = 
      dut.io.Rd.expect("h80_80_80_80".U)

      // Test Case: Mixed Positive and Negative Values
      dut.io.Rs1.poke("hFF_7F_80_01".U) 
      dut.io.Rs2.poke("h01_80_7F_FF".U) 
      dut.clock.step()
      // Expected Rd  = 
      dut.io.Rd.expect("h00_FF_FF_00".U)
    }
  }
}


///============================TESTER====PAADDU.B -- SIMD 8-bit Unsigned Averaging Addition=========================///

