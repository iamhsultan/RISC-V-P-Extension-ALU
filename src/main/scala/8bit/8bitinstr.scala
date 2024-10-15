import chisel3._
import chisel3.util._

                            ///--N-bit Adder generator--/// 
class Adder(width : Int) extends Module {
    val io = IO(new Bundle {
        val a    = Input(UInt(width.W))
        val b    = Input(UInt(width.W))
        val cin  = Input(UInt(1.W))

        val s    = Output(UInt(width.W))
        val cout = Output(UInt(1.W))
    })
    //function of class Adder//
    val cout_s = Wire(UInt((width+1).W))  // Width + 1 takes care of the carry bit as well
    cout_s  := Cat(0.U(1.W),io.a) + Cat(0.U(1.W),io.b) + Cat(0.U(1.W), io.cin)      // concatenating extra bit to capture the carry out from MSB bit
    io.s    := cout_s((width-1),0)      // Seperating Sum 
    io.cout := cout_s(width)            // Seperating Carry
}

                            ///--PADD.B -- SIMD 8bit Addition--///
class PAdd8 extends Module {
    val io = IO(new Bundle {
        val rs1 = Input(UInt(32.W))
        val rs2 = Input(UInt(32.W))

        val rd  = Output(UInt(32.W))

    })

    val A8      = Seq.fill(4)(Module(new Adder(8)))  // generate four 8bit adders
    val carryin = Wire(Vec(5,UInt(1.W)))    // Generate five wires for internal carrys between adders. c0 to c4
    val swire   = Wire(Vec(4,UInt(8.W)))  // Generate 32 wires to hold the sum bits  
   
    //Function of class PAdd8 which adds 8-bit elements of 32-bit words//
    carryin(0) := 0.U   // initial carry for first Adder(8) 
    for (x <- 0 until 4) {
            
        A8(x).io.a   := io.rs1((x*8+7) , (x*8+0))     // 8bit elements of 32bit word assigned to their respective adders
        A8(x).io.b   := io.rs2((x*8+7) , (x*8+0))
        A8(x).io.cin := carryin(x)      // assigned carryin for the adder

        swire(x)     := A8(x).io.s      // 8bit sum from adders assigned to internal wires
        carryin(x+1) := A8(x).io.cout       // carry out from current adder assigned to carry in of next adder      
        
    }

    io.rd := Cat(swire(3),swire(2),swire(1),swire(0))
    //cout := carryin(4)    //the carry out from the component to be assined depending on the instruction 
}


object GenerateVerilog extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new PAdd8(), Array("--target-dir", "generated"))
}