package cpu

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.com.uart._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.regif._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._

import spinal.lib.memory.sdram.sdr.SdramInterface
import spinal.lib.memory.sdram.sdr.Axi4SharedSdramCtrl
import spinal.lib.memory.sdram.sdr.IS42x320D
import spinal.lib.blackbox.lattice.ecp5.JTAGG
import spinal.lib.com.jtag.Jtag
import spinal.lib.cpu.riscv.impl.build.RiscvAxi4
import spinal.lib.system.debugger.JtagAxi4SharedDebugger
import spinal.lib.system.debugger.SystemDebuggerConfig
import spinal.lib.cpu.riscv.impl.RiscvCore
import spinal.lib.io.InOutWrapper
import _root_.cpu.plugins.AluPlugin
import _root_.cpu.plugins.InstructionDecoderPlugin
import scala.collection.mutable.ArrayBuffer

import spinal.lib.blackbox.lattice.ecp5.DCCA
import _root_.cpu.sim.SdramDriver

class System extends Component {
  val io = new Bundle {
    val asyncReset = in Bool ()

    val axiClock  = in Bool ()
    val jtagClock = in Bool ()

    val jtag  = slave(Jtag())
    val sdram = master(SdramInterface(IS42S16160B.layout)) simPublic

    val uart = master(Uart())
  }

  val resetCtrlClockDomain = ClockDomain(
    clock = io.axiClock,
    config = ClockDomainConfig(
      resetKind = BOOT
    )
  )

  val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
    val axiResetUnbuffered  = False
    val coreResetUnbuffered = False

    val axiResetCounter = Reg(UInt(6 bits)) init (0)
    when(axiResetCounter =/= U(axiResetCounter.range -> true)) {
      axiResetCounter    := axiResetCounter + 1
      axiResetUnbuffered := True
    }
    when(BufferCC(io.asyncReset)) {
      axiResetCounter := 0
    }

    when(axiResetUnbuffered) {
      coreResetUnbuffered := True
    }

    val axiReset  = RegNext(axiResetUnbuffered) simPublic
    val coreReset = RegNext(coreResetUnbuffered)
  }

  val axiClockDomain = ClockDomain(
    clock = io.axiClock,
    reset = resetCtrl.axiReset,
    frequency = FixedFrequency(
      100 MHz
    ) // The frequency information is used by the SDRAM controller
  )

  val coreClockDomain = ClockDomain(
    clock = io.axiClock,
    reset = resetCtrl.coreReset
  )

  val jtagClockDomain = ClockDomain(
    clock = io.jtagClock
  )

  val axi = new ClockingArea(axiClockDomain) {

    val core = coreClockDomain {
      new CpuAxi4(
        CPUConfig(
          dataWidth = 32 bits,
          instrWidth = 32 bits
        ),
        plugins = ArrayBuffer(
          new AluPlugin,
          new InstructionDecoderPlugin
        )
      )
    }

    val sdramCtrl = Axi4SharedSdramCtrl(
      axiDataWidth = 32,
      axiIdWidth = 4,
      layout = IS42S16160B.layout,
      timing = IS42S16160B.timingGrade7,
      CAS = 3
    )

    val ram = Axi4SharedOnChipRam(
      dataWidth = 32,
      byteCount = 4 KiB,
      idWidth = 4
    )

    val apbBridge = new Axi4SharedToApb3Bridge(
      addressWidth = 32,
      dataWidth = 32,
      idWidth = 4
    )

    val jtagCtrl = JtagAxi4SharedDebugger(
      SystemDebuggerConfig(
        memAddressWidth = 32,
        memDataWidth = 32,
        remoteCmdWidth = 1
      )
    )

    val uartCtrlConfig = UartCtrlMemoryMappedConfig(
      uartCtrlConfig = UartCtrlGenerics(
        dataWidthMax = 8,
        clockDividerWidth = 20,
        preSamplingSize = 1,
        samplingSize = 5,
        postSamplingSize = 2
      ),
      txFifoDepth = 16,
      rxFifoDepth = 16
    )

    val uartCtrl = Apb3UartCtrl(uartCtrlConfig)

    var axiCrossbar = Axi4CrossbarFactory()

    axiCrossbar.addSlaves(
      ram.io.axi       -> (0x00000000L, 4 KiB),
      sdramCtrl.io.axi -> (0x40000000L, sdramCtrl.layout.capacity),
      apbBridge.io.axi -> (0xf0000000L, 1 MiB)
    )

    axiCrossbar.addConnections(
      jtagCtrl.io.axi -> List(ram.io.axi, sdramCtrl.io.axi, apbBridge.io.axi)
    )

    axiCrossbar.addPipelining(sdramCtrl.io.axi)((crossbar, ctrl) => {
      crossbar.sharedCmd.halfPipe() >> ctrl.sharedCmd
      crossbar.writeData >/-> ctrl.writeData
      crossbar.writeRsp << ctrl.writeRsp
      crossbar.readRsp << ctrl.readRsp

    })

    axiCrossbar.addPipelining(apbBridge.io.axi)((crossbar, bridge) => {
      crossbar.sharedCmd.halfPipe() >> bridge.sharedCmd
      crossbar.writeData >/-> bridge.writeData
      crossbar.writeRsp << bridge.writeRsp
      crossbar.readRsp << bridge.readRsp

    })

    axiCrossbar.build()

    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        uartCtrl.io.apb -> (0x10000, 4 KiB)
      )
    )

  }
  io.jtag <> axi.jtagCtrl.io.jtag
  io.uart <> axi.uartCtrl.io.uart
  io.sdram <> axi.sdramCtrl.io.sdram
}

class TopLevel extends Component {
  val io = new Bundle {
    val jtag = slave(Jtag())
    val uart = master(Uart())

    val asyncReset = in Bool ()
    val axiClock   = in Bool ()
    val jtagClock  = in Bool ()
  }

  val topClockDomain = ClockDomain(
    clock = io.axiClock,
    reset = io.asyncReset,
    frequency = FixedFrequency(
      1 MHz
    ) // The frequency information is used by the SDRAM controller
  )
  val dut = new ClockingArea(topClockDomain) {
    val system = new System
    system.io.axiClock   := io.axiClock
    system.io.asyncReset := io.asyncReset
    system.io.jtagClock  := io.jtagClock

    val sdram = new SdramDriver(IS42S16160B.layout)

    system.io.jtag <> io.jtag
    system.io.uart <> io.uart
    sdram.io.sdram <> system.io.sdram
  }
}

object System {
  def main(args: Array[String]): Unit = {
    val config = SpinalConfig(verbose = true).dumpWave()
    // val report = config.generateVerilog(new System())
    SimConfig.withWave
      .compile(InOutWrapper(new TopLevel))
      .doSimUntilVoid(dut => {
        val clockThread = fork {
          dut.topClockDomain.fallingEdge()
          dut.topClockDomain.deassertReset()
          sleep(0)

          dut.topClockDomain.assertReset()
          sleep(10)
          dut.topClockDomain.deassertReset()
          sleep(1)
          while (true) {
            dut.topClockDomain.clockToggle()
            sleep(1)
          }
        }
        var mainThread = fork {
          for (i <- 0 until 100000) {
            dut.topClockDomain.waitRisingEdge()
          }
          simSuccess()
        }
      })
  }
}
