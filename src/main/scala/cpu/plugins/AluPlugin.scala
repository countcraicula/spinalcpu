package cpu.plugins

import spinal.core._
import spinal.lib._
import _root_.cpu._

object AluCtrl extends SpinalEnum {
  val NOP, ADD, SUB, XOR, OR, AND, SetLessThan, SetLessThanU = newElement()
}

case class AluCmd(XLEN: BitCount = 32 bits) extends Bundle {
  val operandA = Bits(XLEN)
  val operandB = Bits(XLEN)
  val control = AluCtrl()
}

case class AluRsp(XLEN: BitCount = 32 bits) extends Bundle {
  val result = Bits(XLEN)
  val resultIsZero = Bool()
  val resultIsNegative = Bool()
  val equal = Bool()
  val lessThan = Bool()
}

class IntAlu(XLEN: BitCount = 32 bits) extends Component {
  val io = new Bundle {
    val cmd = slave Stream AluCmd(XLEN)
    val rsp = master Stream AluRsp(XLEN)
  }

  val a = io.cmd.operandA.asUInt
  val b = io.cmd.operandB.asUInt

  val xor = (a ^ b).asBits
  val and = (a & b).asBits
  val or = (a | b).asBits

  val doSubtraction = False

  val xorMask = Cat((0 until XLEN.value).map(_ => doSubtraction.asBits))
  val addSub: Bits =
    (a + (b.asBits ^ xorMask).asUInt + U(doSubtraction)).asBits

  val result = Bits(XLEN)

  val unsignedLess = False
  val relevantMSB = Mux(unsignedLess, b.msb, a.msb)
  val aLessThanB = Mux(a.msb === b.msb, addSub.msb, relevantMSB)

  val addSubIsZero = !addSub.orR

  switch(io.cmd.control) {
    is(AluCtrl.NOP) {
      result := B(0)
    }
    is(AluCtrl.ADD) {
      doSubtraction := False
      result := addSub
    }
    is(AluCtrl.SUB) {
      doSubtraction := True
      result := addSub
    }
    is(AluCtrl.XOR) {
      result := xor
    }
    is(AluCtrl.OR) {
      result := or
    }
    is(AluCtrl.AND) {
      result := and
    }
    is(AluCtrl.SetLessThan) {
      doSubtraction := True
      unsignedLess := False
      result := aLessThanB.asBits.resize(XLEN)
    }
    is(AluCtrl.SetLessThanU) {
      doSubtraction := True
      unsignedLess := True
      result := aLessThanB.asBits.resize(XLEN)
    }

  }

  var resultBundle = AluRsp(XLEN)
  resultBundle.result := result
  resultBundle.resultIsNegative := result.msb
  resultBundle.resultIsZero := addSubIsZero
  resultBundle.equal := addSubIsZero
  resultBundle.lessThan := aLessThanB
  io.rsp << io.cmd.translateWith(resultBundle)

}

class AluPlugin extends Plugin {
  var alu: IntAlu = null

  override def setup(core: CPU): Unit = {
    alu = new IntAlu(core.XLEN)
  }

  override def build(core: CPU): Unit = {
    def alu_actions(
        instruction: (CPUStage, CoreInstructionArgs) => CoreInstructionCmd
    ): (CPUStage, CoreInstructionArgs) => Unit = {
      (stage: CPUStage, args: CoreInstructionArgs) =>
        new Area {
          val cmd = instruction(stage, args)

        }.setName("alu_r_type_action")
    }

    import _root_.cpu.Instructions
    val rTypeInstructions = List(
      Instructions.ADD,
      Instructions.SUB,
      Instructions.XOR,
      Instructions.OR,
      Instructions.AND,
      Instructions.SLL,
      Instructions.SRL,
      Instructions.SRA,
      Instructions.SLT,
      Instructions.SLTU
    )

    for (instruction <- rTypeInstructions) {
      val (mask, argsFunc) = instruction
      core.instructionDecoder.registerInstruction(
        Stage.Execute0,
        mask,
        alu_actions(argsFunc.build)
      )
    }

    val iTypeSignedInstructions = List(
      Instructions.ADDI,
      Instructions.ANDI,
      Instructions.ORI,
      Instructions.XORI,
      Instructions.SLTI
    )

    for (instruction <- iTypeSignedInstructions) {
      val (mask, argsFunc) = instruction
      core.instructionDecoder.registerInstruction(
        Stage.Execute0,
        mask,
        alu_actions(argsFunc.build)
      )
    }

    val iTypeUnsignedInstructions = List(
      Instructions.SLLI,
      Instructions.SRLI,
      Instructions.SRAI,
      Instructions.SLTIU
    )
    for (instruction <- iTypeUnsignedInstructions) {
      val (mask, argsFunc) = instruction
      core.instructionDecoder.registerInstruction(
        Stage.Execute0,
        mask,
        alu_actions(argsFunc.build)
      )
    }

  }
}
