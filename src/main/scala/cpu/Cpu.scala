package cpu

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import plugins._
import java.util.Base64.Decoder
import spinal.lib.bus.amba4.axi.Axi4ReadOnly
import spinal.lib.bus.amba4.axi.Axi4Config
import spinal.lib.bus.amba4.axi.Axi4Shared

object AluSrcA extends SpinalEnum {
  val PC, RS1 = newElement()
}

object AluSrcB extends SpinalEnum {
  val RS2, IMM = newElement()
}

case class CPUConfig(
    instrWidth: BitCount = BitCount(32),
    dataWidth: BitCount = BitCount(32),
    addressWidth: BitCount = BitCount(32),
    numRegisters: Int = 32
)

case class InstructionCmd()(implicit config: CPUConfig) extends Bundle {
  val pc = UInt(config.addressWidth)
}

case class InstructionRsp()(implicit config: CPUConfig) extends Bundle {
  val pc          = UInt(config.addressWidth)
  val instruction = Bits(config.instrWidth)
}

case class InstructionBus()(implicit config: CPUConfig)
    extends Bundle
    with IMasterSlave {
  val cmd = Stream(InstructionCmd())
  val rsp = Stream(InstructionRsp())

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  def toAxi4ReadOnly(axi4Config: Axi4Config): Axi4ReadOnly = {
    val mm         = Axi4ReadOnly(axi4Config)
    val pendingCmd = RegInit(False)
    pendingCmd := (pendingCmd && !mm.readRsp.valid) || mm.readCmd.fire
    val haltCmd =
      rsp.isStall || (pendingCmd && !mm.readRsp.valid) // Don't overflow the backupFifo and don't have more than one pending cmd

    mm.readCmd.valid := cmd.valid && !haltCmd
    mm.readCmd.addr  := cmd.pc(mm.readCmd.addr.getWidth - 1 downto 2) @@ U"00"
    cmd.ready        := mm.readCmd.ready && !haltCmd

    val backupFifoIn = Stream(InstructionRsp())
    backupFifoIn.valid       := mm.readRsp.valid
    backupFifoIn.instruction := mm.readRsp.data
    backupFifoIn.pc          := RegNextWhen(cmd.pc, cmd.ready)

    rsp </< backupFifoIn // 1 depth of backup fifo, zero latency
    mm.readRsp.ready := True

    mm
  }
}

case class DataCmd()(implicit config: CPUConfig) extends Bundle {
  val address = UInt(config.addressWidth)
  val data    = Bits(config.dataWidth)
  val size    = UInt(2 bits)
  val write   = Bool()
}

case class DataRsp()(implicit config: CPUConfig) extends Bundle {
  val data = Bits(config.dataWidth)
}

case class DataBus()(implicit config: CPUConfig)
    extends Bundle
    with IMasterSlave {
  val cmd = Stream(DataCmd())
  val rsp = Stream(DataRsp())

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  def toAxi4Shared(axi4Config: Axi4Config): Axi4Shared = {
    val mm         = new Axi4Shared(axi4Config)
    val cmdPreFork = cmd.stage
    val pendingMax = 7
    val pendingCmd = CounterUpDown(
      pendingMax + 1,
      mm.sharedCmd.fire,
      (mm.readRsp.fire && mm.readRsp.last) || mm.writeRsp.fire
    )
    val pendingIsWrite = RegNextWhen(mm.sharedCmd.write, mm.sharedCmd.fire)

    val (cmdFork, dataFork) = StreamFork2(
      cmdPreFork.haltWhen(
        (pendingCmd =/= 0 && (pendingIsWrite ^ cmdPreFork.write)) || pendingCmd === pendingMax
      )
    )
    val dataStage = dataFork.throwWhen(!dataFork.write)
    mm.writeData.arbitrationFrom(dataStage)
    mm.writeData.last := True
    mm.writeData.data := dataStage.data
    mm.writeData.strb := (dataStage.size.mux(
      U(0)    -> B"0001",
      U(1)    -> B"0011",
      default -> B"1111"
    ) << dataStage.address(1 downto 0)).resized
    val contextIn = Stream(UInt(2 bit))
    contextIn.valid   := cmdFork.fire && !cmdFork.write
    contextIn.payload := cmdFork.address(1 downto 0)

    val contextOut = contextIn.m2sPipe().s2mPipe()
    contextOut.ready := rsp.fire

    cmdFork.ready := mm.sharedCmd.ready
    rsp.valid     := mm.readRsp.valid
    rsp.data      := mm.readRsp.data
    switch(contextOut.payload) {
      is(1) {
        rsp.data(7 downto 0) := mm.readRsp.data(15 downto 8)
      }
      is(2) {
        rsp.data(15 downto 0) := mm.readRsp.data(31 downto 16)
      }
      is(3) {
        rsp.data(7 downto 0) := mm.readRsp.data(31 downto 24)
      }
    }
    mm
  }
}

case class FetchOutput()(implicit config: CPUConfig) extends Bundle {
  val instruction = Bits(config.instrWidth)
  val pc          = UInt(config.addressWidth)
}

case class Execute0Output()(implicit config: CPUConfig) extends Bundle {}

case class Execute1Output()(implicit config: CPUConfig) extends Bundle {}

class CPU(config: CPUConfig, plugins: List[Plugin]) extends Component {
  implicit val c   = config
  val REG_ADDR_LEN = log2Up(config.numRegisters) bits
  val ALIGNMENT    = (config.dataWidth / (8 bits)).value
  val XLEN         = config.dataWidth

  val currentInstruction = Bits(config.instrWidth)

  val iCmd = Stream(InstructionCmd()) simPublic
  val iRsp = Stream(InstructionRsp()) simPublic

  val dCmd = Stream(DataCmd()) simPublic
  val dRsp = Stream(DataRsp()) simPublic

  var registers = new Area {

    val registers =
      Mem.fill(config.numRegisters)(B(0, 32 bits)) simPublic

    // registers.setTechnology(registerFile)

    val rd_addr = UInt(REG_ADDR_LEN)

    def setRDAddr(addr: UInt): Unit = {
      rd_addr := addr
    }

    def writeRD(data: Bits): Unit = {
      registers(rd_addr) := data
    }
  }

  var instructionDecoder: DecoderService = null

  val cpu = this
  var alu = new IntAlu

  for (plugin <- plugins)
    plugin.setup(cpu)

  for (plugin <- plugins)
    plugin.build(cpu)

  val prefetch = new PrefetchStage {
    val halt   = Bool
    val pc     = RegInit(U(0, config.addressWidth)) simPublic
    val inc    = RegInit(False)
    val pcNext = pc + Mux(inc, U(4), U(0))
    val pcLoad = Flow(pc) // Allow jumps.
    // when(pcLoad.valid) {
    //   pcNext := pcLoad.payload
    // }

    iCmd.payload.pc := pcNext
    iCmd.valid      := !halt

    when(iCmd.fire || pcLoad.fire) {
      pc := pcNext
    }

    when(iCmd.fire) {
      inc := True
    } elsewhen (pcLoad.valid) {
      inc := False
    }
    for (plugin <- plugins)
      plugin.buildStage(this.asInstanceOf[PrefetchStage])

  }.setName("prefetch")
  val fetch = new FetchStage {
    val pendingPrefetch =
      CounterUpDown(stateCount = 4, incWhen = iCmd.fire, decWhen = iRsp.fire)
    when(pendingPrefetch === 3) {
      iCmd.valid := False
    }
    val outInst = Stream(FetchOutput())
    outInst.arbitrationFrom(iRsp)
    outInst.pc          := iRsp.pc
    outInst.instruction := iRsp.instruction
    for (plugin <- plugins)
      plugin.buildStage(this.asInstanceOf[FetchStage])
  }

  val decode = new DecodeStage {
    val inInst   = fetch.outInst.m2sPipe()
    val op1, op2 = Reg(Bits(config.dataWidth))
    def loadRS1(addr: UInt): Unit = {
      op1 := Mux(addr === 0, B(0, 32 bits), registers.registers.readSync(addr))
    }

    def loadRS2(addr: UInt): Unit = {
      op2 := Mux(addr === 0, B(0, 32 bits), registers.registers.readSync(addr))
    }

    def loadImmediate(imm: Bits): Unit = {
      op2 := imm
    }

    def loadPC(offset: SInt): Unit = {
      op1 := (inInst.pc + offset.asUInt).asBits
    }

    val outInst = Stream(CoreInstructionCmd())
    outInst.arbitrationFrom(inInst)
    for (plugin <- plugins)
      plugin.buildStage(this.asInstanceOf[DecodeStage])
  }

  val execute0 = new Execute0Stage {
    val inInst = decode.outInst.m2sPipe()

    val outInst = Stream(Execute0Output())
    outInst.arbitrationFrom(inInst)
    for (plugin <- plugins)
      plugin.buildStage(this.asInstanceOf[Execute0Stage])
  }

  val execute1 = new Execute1Stage {
    val inInst = execute0.outInst.m2sPipe()

    val outInst = Stream(Execute1Output())
    outInst.arbitrationFrom(inInst)
    for (plugin <- plugins)
      plugin.buildStage(this.asInstanceOf[Execute1Stage])
  }

  val writeback = new Area {
    val inInst = execute1.outInst.m2sPipe()

  }

}
