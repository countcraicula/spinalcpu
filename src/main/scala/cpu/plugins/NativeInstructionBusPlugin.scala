package cpu.plugins

import spinal.core._
import spinal.lib._
import _root_.cpu._

class NativeInstructionBusPlugin extends Plugin {
  var instructionBus: InstructionBus = null

  override def setup(core: CPU): Unit = new Area {
    instructionBus = master(InstructionBus()(core.c))
    instructionBus.cmd << core.iCmd
    instructionBus.rsp >> core.iRsp
  }
}

class NativeDataBusPlugin extends Plugin {
  var dataBus: DataBus = null

  override def setup(core: CPU): Unit = new Area {
    dataBus = master(DataBus()(core.c))
    dataBus.cmd << core.dCmd
    dataBus.rsp >> core.dRsp
  }
}
