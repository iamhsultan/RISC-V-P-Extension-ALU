import chisel3._
import chisel3.util._
import Bits8._
//import scala.annotation.switch
//import chisel3.experimental.ChiselEnum

// Defining ALU operations as enumeration type using ChiselEnum
object ALUops extends ChiselEnum {
    val PADDB , PAADDB , PAADDUB , PADDW , PSADDUB , PSUBB , PASUBB , PASUBUB , PSSUBB , PSSUBUB = Value
}

// Port declaration of ALU
class PextALU extends Module { 
    val io = IO(new Bundle {
        val Rs1       = Input(UInt(32.W))
        val Rs2       = Input(UInt(32.W))
        val operation = Input(ALUops())
        val Rd        = Output(UInt(32.W))
        val vxsat     = Output(UInt(1.W))
    })

    // Default values
    io.Rd    := 0.U
    io.vxsat := 0.U

    // ALU operation selection 
    switch(io.operation) {
        is(ALUops.PADDB) {
            val add8      = Module(new PADDB())
            add8.io.Rs1   := io.Rs1
            add8.io.Rs2   := io.Rs2
            io.Rd         := add8.io.Rd
            io.vxsat      := add8.io.vxsat  
        }
        is(ALUops.PAADDB) {
            val sAvAdd8     = Module(new PAADDB())
            sAvAdd8.io.Rs1 := io.Rs1
            sAvAdd8.io.Rs2 := io.Rs2
            io.Rd          := sAvAdd8.io.Rd
            io.vxsat       := sAvAdd8.io.vxsat  
        }
        is(ALUops.PAADDUB) {
            val uAvAdd8     = Module(new PAADDUB())
            uAvAdd8.io.Rs1 := io.Rs1
            uAvAdd8.io.Rs2 := io.Rs2
            io.Rd          := uAvAdd8.io.Rd
            io.vxsat       := uAvAdd8.io.vxsat  
        }
        is(ALUops.PSADDUB) {
            val uSatAdd8     = Module(new PSADDUB())
            uSatAdd8.io.Rs1 := io.Rs1
            uSatAdd8.io.Rs2 := io.Rs2
            io.Rd           := uSatAdd8.io.Rd
            io.vxsat        := uSatAdd8.io.vxsat  
        }
        is(ALUops.PADDW) {
            val add32       = Module(new PADDW())
            add32.io.Rs1   := io.Rs1
            add32.io.Rs2   := io.Rs2
            io.Rd          := add32.io.Rd
            io.vxsat       := add32.io.vxsat  
        }
        //------------SUBTRACTION---------------//
        is(ALUops.PSUBB) {
            val sub8     = Module(new PSUBB())
            sub8.io.Rs1 := io.Rs1
            sub8.io.Rs2 := io.Rs2
            io.Rd       := sub8.io.Rd
            io.vxsat    := sub8.io.vxsat  
        }
        is(ALUops.PASUBB) {
            val sAvSub8     = Module(new PASUBB())
            sAvSub8.io.Rs1 := io.Rs1
            sAvSub8.io.Rs2 := io.Rs2
            io.Rd          := sAvSub8.io.Rd
            io.vxsat       := sAvSub8.io.vxsat  
        }
        is(ALUops.PASUBUB) {
            val uAvSub8     = Module(new PASUBUB())
            uAvSub8.io.Rs1 := io.Rs1
            uAvSub8.io.Rs2 := io.Rs2
            io.Rd          := uAvSub8.io.Rd
            io.vxsat       := uAvSub8.io.vxsat  
        }
        is(ALUops.PSSUBB) {
            val sSatSub8     = Module(new PSSUBB())
            sSatSub8.io.Rs1 := io.Rs1
            sSatSub8.io.Rs2 := io.Rs2
            io.Rd           := sSatSub8.io.Rd
            io.vxsat        := sSatSub8.io.vxsat  
        }
        is(ALUops.PSSUBUB) {
            val uSatSub8     = Module(new PSSUBUB())
            uSatSub8.io.Rs1 := io.Rs1
            uSatSub8.io.Rs2 := io.Rs2
            io.Rd           := uSatSub8.io.Rd
            io.vxsat        := uSatSub8.io.vxsat  
        }
    }

}

object ALUMain extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new PextALU(), Array("--target-dir", "generated"))
}