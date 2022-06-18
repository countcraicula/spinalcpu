package cpu.plugins

import spinal.core._
import spinal.lib.{master, slave}
import cpu._
import spinal.lib.Flow

case object CoreInstruction extends SpinalEnum {
  val ADD, SUB, AND, OR, XOR, SLL, SRL, SRA, SLT, SLTU, ADDI, ANDI, ORI, XORI,
      SLLI, SRLI, SRAI, SLTI, SLTIU, LB, LBU, LH, LHU, LW, SB, SH, SW, BEQ, BEG,
      BEGU, BLT, BLTU, BNE, JAL, JALR, AUIPC, LUI, EBREAK, ECALL = newElement()
}

case class CoreInstructionCmd() extends Bundle {
  val aluOp1 = Flow(Bits(32 bits))
  val aluOp2 = Flow(Bits(32 bits))
  val regWrite = Bool
  val regWriteAddr = UInt(5 bits)
  val memWrite = Bool
  val pcWrite = Bool
}

case class CoreInstructionRsp() extends Bundle {}

class CoreInstructionArgs(instruction: Bits) {
  def opcode: Bits = instruction(6 downto 0)
  def funct3: Bits = instruction(14 downto 12)
  def funct7: Bits = instruction(31 downto 25)
  def rs1: UInt = instruction(19 downto 15).asUInt
  def rs2: UInt = instruction(24 downto 20).asUInt
  def rd: UInt = instruction(11 downto 7).asUInt
  def imm_i_type = instruction(31 downto 20).asSInt.resize(32).asUInt
  def imm_s_type = (instruction(31 downto 25) ## instruction(
    11 downto 7
  )).asSInt.resize(32).asUInt
  def imm_u_type =
    (instruction(31 downto 12) ## B((11 downto 0).map(_ => False))).asUInt
  def imm_b_type = (instruction(31) ## instruction(7) ## instruction(
    30 downto 25
  ) ## instruction(11 downto 8)).asSInt.resize(32)
  def imm_j_type = (instruction(31) ## instruction(19 downto 12) ## instruction(
    20
  ) ## instruction(30 downto 21)).asSInt.resize(32)

}

trait CoreInstructionCmdBuilder {
  def build(stage: Any, i: CoreInstructionArgs): CoreInstructionCmd = {
    val cmd = CoreInstructionCmd()
    if (stage.isInstanceOf[DecodeStage]) {
      cmd := buildDecode(stage.asInstanceOf[DecodeStage], i)
    }
    cmd
  }
  def buildDecode(
      stage: DecodeStage,
      i: CoreInstructionArgs
  ): CoreInstructionCmd
}

object RTypeInstruction extends CoreInstructionCmdBuilder {
  def buildDecode(
      stage: DecodeStage,
      i: CoreInstructionArgs
  ): CoreInstructionCmd = {
    val cmd = new CoreInstructionCmd()
    stage.loadRS1(i.rs1)
    stage.loadRS2(i.rs2)
    cmd.regWrite := True
    cmd.regWriteAddr := i.rd
    cmd.pcWrite := False
    cmd.memWrite := False
    cmd
  }
}

object ITypeInstruction extends CoreInstructionCmdBuilder {
  def buildDecode(
      stage: DecodeStage,
      i: CoreInstructionArgs
  ): CoreInstructionCmd = {
    IMMInstruction(stage, i, i.imm_i_type)
  }
}

object IMMInstruction {
  def apply(
      stage: DecodeStage,
      i: CoreInstructionArgs,
      aluOp2: UInt
  ): CoreInstructionCmd = {
    val cmd = new CoreInstructionCmd()
    stage.loadRS1(i.rs1)
    cmd.aluOp1.valid := True
    stage.loadImmediate(aluOp2.asBits)
    cmd.regWrite := True
    cmd.regWriteAddr := i.rd
    cmd.pcWrite := False
    cmd.memWrite := False
    cmd
  }
}

object IStarTypeInstruction extends CoreInstructionCmdBuilder {
  def buildDecode(
      stage: DecodeStage,
      i: CoreInstructionArgs
  ): CoreInstructionCmd = {
    IMMInstruction(stage, i, i.rs2)
  }
}

object STypeInstruction extends CoreInstructionCmdBuilder {
  def buildDecode(
      stage: DecodeStage,
      i: CoreInstructionArgs
  ): CoreInstructionCmd = {
    val cmd = IMMInstruction(stage, i, i.rs2)
    cmd.regWrite := False
    cmd.memWrite := True
    cmd
  }
}

object BTypeInstruction extends CoreInstructionCmdBuilder {
  def buildDecode(
      stage: DecodeStage,
      i: CoreInstructionArgs
  ): CoreInstructionCmd = {
    val cmd = IMMInstruction(stage, i, i.imm_b_type.asUInt)
    cmd.regWrite := False
    cmd.memWrite := False
    cmd.pcWrite := True
    cmd
  }
}

object UTypeInstruction extends CoreInstructionCmdBuilder {
  def buildDecode(
      stage: DecodeStage,
      i: CoreInstructionArgs
  ): CoreInstructionCmd = {
    val cmd = new CoreInstructionCmd()
    stage.loadPC(S(0))
    stage.loadImmediate(i.imm_u_type.asBits)
    cmd
  }
}

object JTypeInstruction extends CoreInstructionCmdBuilder {
  def buildDecode(
      stage: DecodeStage,
      i: CoreInstructionArgs
  ): CoreInstructionCmd = {
    val cmd = new CoreInstructionCmd()
    stage.loadRS1(i.rs1)
    stage.loadImmediate(i.imm_j_type.asUInt.asBits)
    cmd.aluOp2.valid := True
    cmd.regWrite := False
    cmd.pcWrite := True
    cmd.memWrite := False
    cmd
  }
}

object ETypeInstruction extends CoreInstructionCmdBuilder {
  def buildDecode(
      stage: DecodeStage,
      i: CoreInstructionArgs
  ): CoreInstructionCmd = {
    val cmd = new CoreInstructionCmd()
    stage.loadRS1(i.rs1)
    stage.loadImmediate(i.imm_i_type.asBits)
    cmd.regWrite := False
    cmd.pcWrite := False
    cmd.memWrite := False
    cmd
  }
}

case class CoreInstructionDecodeCmd() extends Bundle {
  val instruction = Bits(32 bits)
}

case class CoreInstructionDecodeRsp(XLEN: BitCount = 32 bits) extends Bundle {
  val instruction = CoreInstruction()
  val cmd = CoreInstructionCmd()
}

class InstructionDecoderPlugin(
    XLEN: BitCount = BitCount(32),
    numRegisters: Int = 32
) extends Plugin
    with DecoderService {

  val REG_ADDR_LEN = log2Up(numRegisters) bits

  var instructions: Array[
    (
        (Stage.Stage, MaskedLiteral),
        (CPUStage, CoreInstructionArgs) => Unit
    )
  ] =
    Array()

  var cpu: CPU = null

  def registerInstruction(
      stage: Stage.Stage,
      instruction: MaskedLiteral,
      action: (CPUStage, CoreInstructionArgs) => Unit
  ): Unit = {
    instructions ++= List(((stage, instruction), action))
  }

  override def setup(core: CPU): Unit = {
    core.instructionDecoder = this
    cpu = core
  }

  override def buildStage(stage: CPUStage): Unit = {
    new Area {
      switch(cpu.currentInstruction) {
        for (((s, instruction), action) <- instructions) {
          is(instruction) {
            val cmd =
              action(stage, new CoreInstructionArgs(cpu.currentInstruction))
          }
        }
      }
    }
  }
}
