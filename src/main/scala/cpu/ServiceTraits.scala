package cpu

import spinal.core._
import _root_.cpu.plugins.CoreInstructionArgs
import _root_.cpu.plugins.CoreInstructionCmd
import spinal.lib.Flow

trait Plugin {
  def setup(core: CPU): Unit = {}
  def build(core: CPU): Unit = {}
  def buildStage(stage: CPUStage): Unit = {}
}

object Stage extends Enumeration {
  type Stage = Value
  val Prefetch, Fetch, Decode, Execute0, Execute1 = Value
}

trait DecoderService {
  def registerInstruction(
      stage: Stage.Stage,
      instruction: MaskedLiteral,
      action: (CPUStage, CoreInstructionArgs) => Unit
  ): Unit

  def registerInstructions(
      stage: Stage.Stage,
      instructions: Iterable[
        (
            MaskedLiteral,
            (CPUStage, CoreInstructionArgs) => Unit
        )
      ]
  ): Unit = {
    for ((i, a) <- instructions)
      registerInstruction(stage, i, a)
  }
}

trait CPUStage extends Area {}

trait DecodeStage extends CPUStage {
  def loadRS1(addr: UInt): Unit
  def loadRS2(addr: UInt): Unit
  def loadImmediate(imm: Bits): Unit
  def loadPC(offset: SInt): Unit
}

trait PrefetchStage extends CPUStage {
  val pcLoad: Flow[UInt]
}

trait FetchStage extends CPUStage {}

trait Execute0Stage extends CPUStage {}

trait Execute1Stage extends CPUStage {}
