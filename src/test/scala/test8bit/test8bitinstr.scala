import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

///============================TESTER====PAADD.B -- SIMD 8bit Addition=========================///

class PADDBTester extends AnyFlatSpec with ChiselScalatestTester {
  "PADDB" should "correctly compute 8bit addition" in {
    test(new PADDB) { dut =>
      // Test Case 1
      val Rs1 = 0x00204060.U // First 32-bit word (Byte 4: 0x00, Byte 3: 0x20, Byte 2: 0x40, Byte 1: 0x60)
      val Rs2 = 0x01030507.U // Second 32-bit word (Byte 4: 0x01, Byte 3: 0x03, Byte 2: 0x05, Byte 1: 0x07)     
      val expectedSum = 0x01234567.U // Expected result 
 
      dut.io.Rs1.poke(Rs1)
      dut.io.Rs2.poke(Rs2)
     
      dut.clock.step()

      dut.io.Rd.expect(expectedSum)

      // Test Case 2 - with  carry between bytes
      val rs1_carry = "hEF_10_F0_F0".U // First 32-bit word
      val rs2_carry = "h01_FF_10_11".U // Second 32-bit word
      val expectedSum_carry = "hF0_0F_00_01".U // Expected result with carry handling
      
      dut.io.Rs1.poke(rs1_carry)
      dut.io.Rs2.poke(rs2_carry)

      dut.clock.step()

      dut.io.Rd.expect(expectedSum_carry)

      // Test Case 3 - signed values  
      dut.io.Rs1.poke("h00_00_00_F6".U)
      dut.io.Rs2.poke("h00_00_00_F3".U)
      dut.clock.step()
      dut.io.Rd.expect("h00_00_00_E9".U)
    }
  }
}

///============================TESTER====PAADD.B -- SIMD 8-bit Signed Averaging Addition=========================///

class PAADDBTester extends AnyFlatSpec with ChiselScalatestTester {
  "PAADDB" should "correctly compute Signed Averaging Addition" in {
    test(new PAADDB) { dut =>
      //Test Case : Rs1 = 0x7F_00_80_00, Rs2 = 0x7F_00_80_00
      dut.io.Rs1.poke(0x7F008000.S) // Rs1 = 127, 0, -128, 0
      dut.io.Rs2.poke(0x7F008000.S) // Rs2 = 127, 0, -128, 0
      dut.clock.step()
      // Expected Rd = (127+127)>>1, (0+0)>>1, (-128-128)>>1, (0+0)>>1 = 127, 0, -128, 0 => Rd = 0x7F008000
      dut.io.Rd.expect(0x7F008000.S)

      // Test Case: typical
      dut.io.Rs1.poke(0x80_F2_80_E4.S)     // Rs1 = 80 --> -128,F2 --> -14, 80 --> -128, E4 --> -28
      dut.io.Rs2.poke(0xF8_F6_08_0E.S)     // Rs2 = F8 -->   -8,F6 --> -10, 08 -->   +8, 0E --> +14 
      dut.clock.step()
                                  // Expected Rd  =         -68 is BC , -12 is F4 , -60 is C4 ,  -7 is F9 in 2's Complement form.
      dut.io.Rd.expect(0xBC_F4_C4_F9.S)

      // Test Case : Rs1 = 0xFF_01_02_80, Rs2 = 0x00_FF_7E_7F
      dut.io.Rs1.poke(0xFF010280.S) // Rs1 = -1, 1, 2, -128
      dut.io.Rs2.poke(0x00FF7E7F.S) // Rs2 = 0, -1, 126, 127
      dut.clock.step()
      // Expected Rd = (-1+0)>>1, (1-1)>>1, (2+126)>>1, (-128+127)>>1 => -1, 0, 64, -1 => Rd = 0xFF0040FF
      dut.io.Rd.expect(0xFF0040FF.S)

      // Test Case: Normal Addition and Averaging
      dut.io.Rs1.poke(0x12_34_56_78.S) 
      dut.io.Rs2.poke(0x10_20_30_40.S)  
      dut.clock.step()
      // Expected Rd = 0x22 >> 1 = 0x11, 0x54 >> 1 = 0x2A, 0x86 >> 1 = 0x43, 0xB8 >> 1 = 0x5C
      dut.io.Rd.expect(0x11_2A_43_5C.S)

      // Test Case: signed decimal
      dut.io.Rs1.poke(-128.S) 
      dut.io.Rs2.poke(8.S) 
      dut.clock.step()
      //  -128 + 8 = -120 ==> Averaged result is -60
      dut.io.Rd.expect(-60.S)
    }
  }
}


///============================TESTER====PAADDU.B -- SIMD 8-bit Unsigned Averaging Addition=========================///
class PAADDUBTester extends AnyFlatSpec with ChiselScalatestTester {
  "PAADDUB" should "correctly compute Unsigned Averaging Addition" in {
    test(new PAADDUB) { dut =>
      
      // Test Case:  Rs1 = 0x7F_00_80_00, Rs2 = 0x7F_00_80_00
      dut.io.Rs1.poke("h00_7F_80_40".U) // Rs1 = 127, 0, -128, 0
      dut.io.Rs2.poke("h00_7F_80_80".U) // Rs2 = 127, 0, -128, 0
      dut.clock.step()
      // Expected Rd = (127+127)>>1, (0+0)>>1, (-128-128)>>1, (0+0)>>1 = 127, 0, -128, 0 => Rd = 0x7F008000
      dut.io.Rd.expect("h00_7F_80_60".U)

    }
  }
}
//=============================TESTER=====PSADDU.B -- SIMD 8Bit Unsigned Saturating Addition========================///
class PSADDUBTester extends AnyFlatSpec with ChiselScalatestTester {
  "PSADDUB" should "correctly compute Unsigned Saturating Addition" in {
    test(new PSADDUB) { dut =>
      
      // Test Case:  
      dut.io.Rs1.poke("hFF_FF_80_40".U) // 
      dut.io.Rs2.poke("h02_01_80_80".U) // 
      dut.clock.step()
      //for addition result exceeding the 8bit register limit of FF, output saturates to FF. Eg 127 + 127 = 255 i.e., h80+h80 != h100 but hFF
      dut.io.Rd.expect("hFF_FF_FF_C0".U)

    }
  }
}

///============================TESTER====PADD.W -- SIMD 32-bit Addition=========================///
class PADDWTester extends AnyFlatSpec with ChiselScalatestTester {
  "PADDW" should "correctly compute 32bit addition" in {
    test(new PADDW) { dut =>
      // Test Case 1
      val Rs1 = 0x00204060.U // First 32-bit word (Byte 4: 0x00, Byte 3: 0x20, Byte 2: 0x40, Byte 1: 0x60)
      val Rs2 = 0x01030507.U // Second 32-bit word (Byte 4: 0x01, Byte 3: 0x03, Byte 2: 0x05, Byte 1: 0x07)      
      val expectedSum = 0x01234567.U // Expected result 
 
      dut.io.Rs1.poke(Rs1)
      dut.io.Rs2.poke(Rs2)
     
      dut.clock.step()

      dut.io.Rd.expect(expectedSum)

      // Test Case 2 - with  carry between bytes
      val rs1_carry = "hFF_00_F0_F0".U // First 32-bit word
      val rs2_carry = "h01_FF_10_11".U // Second 32-bit word
      val expectedSum_carry = "h01_00_01_01".U // Expected result with carry handling
      
      dut.io.Rs1.poke(rs1_carry)
      dut.io.Rs2.poke(rs2_carry)

      dut.clock.step()

      dut.io.Rd.expect(expectedSum_carry)
    }
  }
}

//===============================================================================================================================================//
//SUBTRACTION
//===============================================================================================================================================//
