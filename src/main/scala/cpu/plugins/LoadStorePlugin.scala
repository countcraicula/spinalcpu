package cpu.plugins

import spinal.core._
import spinal.lib._
import _root_.cpu._
import spinal.lib.bus.amba4.axi.Axi4Shared
import spinal.lib.bus.amba4.axi.Axi4WriteOnly
import spinal.lib.bus.amba4.axi.Axi4ReadOnly
import spinal.lib.bus.amba4.axi.Axi4Config
import spinal.lib.bus.amba4.axi.Axi4Aw
import spinal.lib.bus.amba4.axi.Axi4W

object LoadCtrl extends SpinalEnum {
  val LB, LBU, LH, LHU, LW = newElement()
}

case class LoadCmd(XLEN: BitCount = BitCount(32)) extends Bundle {
  val base = UInt(XLEN)
  val offset = SInt(XLEN)
  val rdAddr = UInt(5 bits)
  val control = LoadCtrl()
}

case class LoadRsp(XLEN: BitCount = 32 bits) extends Bundle {
  val address = UInt(XLEN)
  val rdAddr = UInt(5 bits)
  var numBytes = Bits(2 bits)
  var writeMask = Bits(4 bits)
}

class Load(XLEN: BitCount = 32 bits) extends Component {
  val cmd = Stream(LoadCmd(XLEN))
  val rsp = Stream(LoadRsp(XLEN))
  rsp.address := cmd.base + cmd.offset.asUInt
  rsp.rdAddr := cmd.rdAddr

  switch(cmd.control) {
    is(LoadCtrl.LB) {
      rsp.numBytes := 1
      rsp.writeMask := B"0001"
    }
    is(LoadCtrl.LBU) {
      rsp.numBytes := 1
      rsp.writeMask := B"0010"
    }
    is(LoadCtrl.LH) {
      rsp.numBytes := 2
      rsp.writeMask := B"0011"
    }
    is(LoadCtrl.LHU) {
      rsp.numBytes := 2
      rsp.writeMask := B"1100"
    }
    is(LoadCtrl.LW) {
      rsp.numBytes := 4
      rsp.writeMask := B"1111"
    }
  }
}

object StoreCtrl extends SpinalEnum {
  val SB, SH, SW = newElement()
}

case class StoreCmd(XLEN: BitCount = 32 bits) extends Bundle {
  val rs = Bits(XLEN)
  val address = Bits(XLEN)
  val control = StoreCtrl()
}

case class StoreRsp(XLEN: BitCount = 32 bits) extends Bundle {
  val data = Bits(XLEN)
  val address = UInt(XLEN)
  val numBytes = UInt(2 bits)
  val writeMask = Bits(4 bits)
}

class Store(XLEN: BitCount = 32 bits) extends Component {
  val cmd = Stream(StoreCmd(XLEN))
  val rsp = Stream(StoreRsp(XLEN))

  rsp.address := (cmd.address(XLEN.value - 1 downto 2) ## B"00").asUInt
  val laneMask = cmd.address(1 downto 0).asUInt

  switch(cmd.control) {
    is(StoreCtrl.SB) {
      rsp.data := Cat(
        cmd.rs(7 downto 0),
        cmd.rs(7 downto 0),
        cmd.rs(7 downto 0),
        cmd.rs(7 downto 0)
      )
      rsp.writeMask := (B"0001" << laneMask).resize(4)
      rsp.numBytes := U(1)
    }
    is(StoreCtrl.SH) {
      rsp.data := Cat(
        cmd.rs(15 downto 0),
        cmd.rs(15 downto 0)
      )
      rsp.writeMask := (B"0011" << (laneMask >> 1)).resize(4)
      rsp.numBytes := U(2)
    }
    is(StoreCtrl.SW) {
      rsp.data := cmd.rs
      rsp.writeMask := B"1111"
      rsp.numBytes := U(3)
    }
  }
}

class LoadStorePlugin extends Plugin {
  var load: Load = null
  var store: Store = null
  override def setup(core: CPU): Unit = {
    load = new Load(core.XLEN)
    store = new Store(core.XLEN)
  }

  override def build(core: CPU): Unit = {
    var decoder = core.instructionDecoder

    def load_action(
        instruction: (CPUStage, CoreInstructionArgs) => CoreInstructionCmd
    ): (CPUStage, CoreInstructionArgs) => Unit = {
      (state: CPUStage, args: CoreInstructionArgs) =>
        {
          instruction(state, args)
        }
    }

    def store_action(
        instruction: (CPUStage, CoreInstructionArgs) => CoreInstructionCmd
    ): (CPUStage, CoreInstructionArgs) => Unit = {
      (state: CPUStage, args: CoreInstructionArgs) =>
        {
          instruction(state, args)
        }
    }

    import Instructions._

    for (instruction <- List(LB, LBU, LH, LHU, LW)) {
      val (mask, action) = instruction
      decoder.registerInstruction(
        Stage.Execute1,
        mask,
        load_action(action.build)
      )
    }

    for (instruction <- List(SB, SH, SW)) {
      val (mask, action) = instruction
      decoder.registerInstruction(
        Stage.Execute1,
        mask,
        store_action(action.build)
      )
    }
  }
}
