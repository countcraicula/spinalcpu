package cpu

import spinal.core._
import plugins._
import scala.runtime.RichLong

object Instructions {
  def ADD = RTypeInstruction(0x33, 0x0, 0x00)
  def SUB = RTypeInstruction(0x33, 0x0, 0x20)
  def AND = RTypeInstruction(0x33, 0x7, 0x00)
  def OR = RTypeInstruction(0x33, 0x6, 0x00)
  def XOR = RTypeInstruction(0x33, 0x4, 0x00)
  def SLL = RTypeInstruction(0x33, 0x1, 0x00)
  def SRL = RTypeInstruction(0x33, 0x5, 0x00)
  def SRA = RTypeInstruction(0x33, 0x5, 0x20)
  def SLT = RTypeInstruction(0x33, 0x2, 0x00)
  def SLTU = RTypeInstruction(0x33, 0x3, 0x00)

  def ADDI = ITypeInstruction(0x13, 0x0)
  def ANDI = ITypeInstruction(0x13, 0x7)
  def ORI = ITypeInstruction(0x13, 0x6)
  def XORI = ITypeInstruction(0x13, 0x4)
  def SLLI = RTypeInstruction(0x13, 0x1, 0x00)
  def SRLI = RTypeInstruction(0x13, 0x5, 0x00)
  def SRAI = RTypeInstruction(0x13, 0x5, 0x20)
  def SLTI = ITypeInstruction(0x13, 0x2)
  def SLTIU = ITypeInstruction(0x13, 0x3)

  def LB = ITypeInstruction(0x3, 0x0)
  def LBU = ITypeInstruction(0x3, 0x4)
  def LH = ITypeInstruction(0x3, 0x1)
  def LHU = ITypeInstruction(0x3, 0x5)
  def LW = ITypeInstruction(0x3, 0x2)

  def SB = STypeInstruction(0x23, 0x0)
  def SH = STypeInstruction(0x23, 0x1)
  def SW = STypeInstruction(0x23, 0x2)

  def BEQ = BTypeInstruction(0x63, 0x0)
  def BEG = BTypeInstruction(0x63, 0x5)
  def BEGU = BTypeInstruction(0x63, 0x7)
  def BLT = BTypeInstruction(0x63, 0x4)
  def BLTU = BTypeInstruction(0x63, 0x6)
  def BNE = BTypeInstruction(0x63, 0x1)

  def JAL = JTypeInstruction(0x6f)
  def JALR = ITypeInstruction(0x6f, 0x0)

  def AUIPC = UTypeInstruction(0x17)
  def LUI = UTypeInstruction(0x37)

  def EBREAK = ETypeInstruction(0x73, 0x0, 0x0)
  def ECALL = ETypeInstruction(0x73, 0x0, 0x1)
}

object RTypeInstruction {
  def apply(
      opcode: Long,
      func3: Long,
      func7: Long
  ): (MaskedLiteral, CoreInstructionCmdBuilder) = {
    val value = BigInt(func7 << 25 | func3 << 12 | opcode)
    val mask = (0x7f.toLong << 25 | 0x7.toLong << 12 | 0x7f.toLong)
    (new MaskedLiteral(value, BigInt(mask), 32), plugins.RTypeInstruction)
  }
}

object ITypeInstruction {
  def apply(
      opcode: Long,
      func3: Long
  ): (MaskedLiteral, CoreInstructionCmdBuilder) = {
    val value = BigInt(func3 << 12 | opcode)
    val mask = BigInt(0x7.toLong << 12 | 0x7f.toLong)
    (new MaskedLiteral(value, mask, 32), plugins.ITypeInstruction)
  }
}

object IStarTypeInstruction {
  def apply(opcode: Long, func3: Long, func7: Long) = {
    val value = BigInt(func3 << 12 | opcode)
    val mask = BigInt(0x7.toLong << 12 | 0x7f.toLong)
    (new MaskedLiteral(value, mask, 32), plugins.IStarTypeInstruction)
  }
}

object STypeInstruction {
  def apply(
      opcode: Long,
      func3: Long
  ): (MaskedLiteral, CoreInstructionCmdBuilder) = {
    val value = BigInt(func3 << 12 | opcode)
    val mask = BigInt(0x7.toLong << 12 | 0x7f.toLong)
    (new MaskedLiteral(value, mask, 32), plugins.STypeInstruction)
  }
}

object BTypeInstruction {
  def apply(
      opcode: Long,
      func3: Long
  ): (MaskedLiteral, CoreInstructionCmdBuilder) = {
    val value = BigInt(func3 << 12 | opcode)
    val mask = BigInt(0x7.toLong << 12 | 0x7f.toLong)
    (new MaskedLiteral(value, mask, 32), plugins.BTypeInstruction)
  }
}

object UTypeInstruction {
  def apply(opcode: Long): (MaskedLiteral, CoreInstructionCmdBuilder) = {
    val value = BigInt(opcode)
    val mask = BigInt(0x7f)
    (new MaskedLiteral(value, mask, 32), plugins.UTypeInstruction)
  }
}

object JTypeInstruction {
  def apply(opcode: Long): (MaskedLiteral, CoreInstructionCmdBuilder) = {
    val value = BigInt(opcode)
    val mask = BigInt(0x7f)
    (new MaskedLiteral(value, mask, 32), plugins.JTypeInstruction)
  }
}

object ETypeInstruction {
  def apply(
      opcode: Long,
      func3: Long,
      imm: Long
  ): (MaskedLiteral, CoreInstructionCmdBuilder) = {
    val value = BigInt(imm << 20 | func3 << 12 | opcode)
    val mask = BigInt(0xfff.toLong << 20 | 0x7.toLong << 12 | 0x7f.toLong)
    (new MaskedLiteral(value, mask, 32), plugins.ETypeInstruction)
  }
}
