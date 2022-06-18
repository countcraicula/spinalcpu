package cpu

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4ReadOnly
import spinal.lib.bus.amba4.axi.Axi4Shared
import spinal.lib.bus.amba4.axi.Axi4Config
import spinal.lib.cpu.riscv.impl.build.RiscvAxi4
import plugins._
import scala.collection.mutable.ArrayBuffer
import spinal.lib.cpu.riscv.impl.CoreInstructionBus

object axi4Config {
  def readOnly: Axi4Config = {
    val config = Axi4Config(
      addressWidth = 32,
      dataWidth = 32,
      useId = false,
      useRegion = false,
      useBurst = true,
      useLock = false,
      useCache = false,
      useProt = false,
      useQos = false,
      useLen = false,
      useStrb = false,
      useResp = false,
      useLast = false
    )
    config
  }
  def shared: Axi4Config = {
    val config = Axi4Config(
      addressWidth = 32,
      dataWidth = 32,
      useId = false,
      useRegion = false,
      useBurst = false,
      useLock = false,
      useCache = false,
      useProt = false,
      useQos = false,
      useLen = false
    )
    config
  }
}

class CpuAxi4(config: CPUConfig, plugins: ArrayBuffer[Plugin])
    extends Component {
  val io = new Bundle {
    val i = master(Axi4ReadOnly(axi4Config.readOnly))
    val d = master(Axi4Shared(axi4Config.shared))
  }
  val instructionBusPlugin = new NativeInstructionBusPlugin
  plugins += instructionBusPlugin
  val dataBusPlugin = new NativeDataBusPlugin
  plugins += dataBusPlugin
  val cpu = new CPU(config, plugins.toList)

  val iBus  = instructionBusPlugin.instructionBus
  val iCore = InstructionBus()(config)
  iCore.cmd << iBus.cmd
  iCore.rsp >> iBus.rsp
  io.i <> iCore.toAxi4ReadOnly(axi4Config.readOnly)

  val dBus  = dataBusPlugin.dataBus
  val dCore = DataBus()(config)
  dCore.cmd << dBus.cmd
  dCore.rsp >> dBus.rsp
  io.d <> dCore.toAxi4Shared(axi4Config.shared)

}
