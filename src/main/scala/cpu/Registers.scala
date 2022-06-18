package cpu

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4Config

case class DataRegisterCmd(XLEN: BitCount = 32 bits, numRegisters: Int = 32)
    extends Bundle {
  val REG_ADDR_LEN = log2Up(numRegisters) bits

  val RAddr1 = UInt(REG_ADDR_LEN)
  val RAddr2 = UInt(REG_ADDR_LEN)

  val WAddr = UInt(REG_ADDR_LEN)
  val WData = Bits(XLEN)
  val WValid = Bool()

}

case class DataRegisterRsp(XLEN: BitCount = 32 bits) extends Bundle {

  val RData1 = Bits(XLEN)
  val RData2 = Bits(XLEN)
}

class DataRegisters(
    numRegisters: Int = 32,
    XLEN: BitCount = 32 bits,
    resetToZero: Boolean = true
) extends Component {
  val REG_ADDR_LEN = log2Up(numRegisters) bits

  val io = new Bundle {
    val cmd = slave Stream (DataRegisterCmd(XLEN, numRegisters))
    val rsp = master Stream (DataRegisterRsp(XLEN))
  }

  val w_addr = UInt(REG_ADDR_LEN)
  w_addr := 0
  when(io.cmd.WValid) {
    w_addr := io.cmd.WAddr
  }

  val registers = (1 until numRegisters).map(i => {
    val writeEnable = w_addr === U(i)
    val reg = RegNextWhen(io.cmd.WData, writeEnable)
    if (resetToZero)
      reg.init(B(0))
    reg
  })

  val register_vec = Vec(List(B(0, 32 bits)) ++ registers)

  val rsp = DataRegisterRsp(XLEN)
  rsp.RData1 := register_vec(io.cmd.RAddr1)
  rsp.RData2 := register_vec(io.cmd.RAddr2)

}
