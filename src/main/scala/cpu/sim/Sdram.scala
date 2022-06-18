package cpu.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.memory.sdram.sdr.SdramInterface
import spinal.lib.memory.sdram.sdr.SdramTimings
import spinal.lib.memory.sdram.SdramLayout
import spinal.lib.fsm.StateMachine
import spinal.lib.fsm.State
import spinal.lib.fsm.EntryPoint
import spinal.lib.fsm.StateDelay

object SdramCommand {
  def DSEL   = M"1-1----"
  def NOP    = M"1-0111-"
  def BST    = M"1-0110-"
  def READ   = M"1-01010"
  def READA  = M"1-01011"
  def WRITE  = M"1-01000"
  def WRITEA = M"1-01001"
  def ACT    = M"1-0011-"
  def PRE    = M"1-00100"
  def PALL   = M"1-00101"
  def CBR    = M"110001-"
  def REF    = M"100001-"
  def MRS    = M"1-00000"
  def SUSP   = M"-0-----"
}

case class SdramOutputCmd(addressWidth: Int, dataWidth: Int, maskSize: Int)
    extends Bundle {
  val address = UInt(addressWidth bits)
  val mask    = Bits(maskSize bits)
  val data    = Bits(dataWidth bits)
  val write   = Bool
}

class SdramDriver(
    layout: SdramLayout
) extends Component {

  val memory =
    Mem.fill(1 << layout.byteAddressWidth)(
      Bits(layout.dataWidth / layout.bytePerWord bits)
    )

  val io = new Bundle {
    val sdram = slave(SdramInterface(layout)) simPublic
  }

  val addressStream = Stream(
    SdramOutputCmd(
      layout.byteAddressWidth,
      layout.bytePerWord * 8,
      io.sdram.DQM.getWidth
    )
  )
  addressStream.valid := False
  addressStream.write := False
  addressStream.data  := B(0, layout.dataWidth bits)

  val MR = Reg(new Bundle {
    val burstLength      = UInt(3 bits)
    val burstInterleaved = Bool
    val CASLatency       = UInt(3 bits)
    val operatingMode    = Bits(2 bits)
    val writeBurstMode   = Bool
  })

  val fsm = new StateMachine {
    import SdramCommand._
    val counter          = RegInit(U(0, 4 bits))
    val autoRefreshCount = RegInit(U(0, 3 bits))
    val lastCKE          = RegNext(io.sdram.CKE)
    val command = RegNext(
      Cat(
        lastCKE,
        io.sdram.CKE,
        io.sdram.CSn,
        io.sdram.RASn,
        io.sdram.CASn,
        io.sdram.WEn,
        io.sdram.ADDR(layout.columnWidth)
      )
    )

    val rowAddress = RegNextWhen(
      io.sdram.ADDR.asUInt,
      command === ACT
    )
    val baseColAddress = RegNextWhen(
      io.sdram.ADDR(layout.columnWidth - 1 downto 0).asUInt,
      command === READ || command === READA || command === WRITE || command === WRITEA
    )
    val colAddress = Reg(baseColAddress)
    val bankAddress = RegNextWhen(
      io.sdram.BA.asUInt,
      command === ACT
    )
    val bankAddressLast = RegNext(bankAddress)

    addressStream.address := Cat(
      bankAddress,
      colAddress,
      rowAddress
    ).asUInt << (layout.bytePerWord - 1)
    addressStream.mask := io.sdram.DQM

    val data = RegNext(io.sdram.DQ.write & io.sdram.DQ.writeEnable)

    val lastCommand = RegNext(command)
    val colAddressMask = RegNextWhen(
      MR.burstLength
        .mux(
          U(0)    -> U((1 << layout.columnWidth) - 1),
          U(1)    -> U((1 << layout.columnWidth) - 2),
          U(2)    -> U((1 << layout.columnWidth) - 4),
          U(3)    -> U((1 << layout.columnWidth) - 8),
          U(7)    -> U(0),
          default -> U((1 << layout.columnWidth) - 1)
        )
        .resize(layout.columnWidth),
      command === MRS
    )

    val prechargeBankAddress =
      B(Cat(io.sdram.ADDR(layout.columnWidth), bankAddress.asBits)).asUInt

    val idle            = new State
    val rowActive       = new State
    val powerDown       = new State
    val activePowerDown = new State
    val read            = new State
    val write           = new State
    val writeA          = new State
    val readA           = new State
    val writeSuspend    = new State
    val writeSuspendA   = new State
    val readSuspend     = new State
    val readSuspendA    = new State
    val modeRegisterSet = new State
    val selfRefresh     = new State
    val cbrRefresh      = new State
    val precharge       = new State
    val prechargeAll    = new State
    val powerOn         = new StateDelay(200 us) with EntryPoint
    val initialize      = new State

    def burstTest(next: State): Unit = {
      when(
        counter === MR.burstLength.mux(
          0       -> U(0),
          1       -> U(1),
          2       -> U(3),
          3       -> U(7),
          default -> U(1)
        ) && MR.burstLength =/= 7
      ) {
        goto(next)
      }
    }

    def nextColAddress(counter: UInt): UInt = {
      (baseColAddress & colAddressMask | (baseColAddress + counter) & colAddressMask)
    }

    initialize.whenIsActive({
      counter := counter + 1
      when(command === CBR) {
        when(counter === 0 || lastCommand === NOP) {
          autoRefreshCount := autoRefreshCount + 1
        }
        when(autoRefreshCount === 7) {
          goto(idle)
        }
      }
    })
    initialize.onExit({
      autoRefreshCount := 0
      counter          := 0
    })

    idle.whenIsActive({
      switch(command) {
        is(DSEL) {
          goto(powerDown)
        }
        is(ACT) {
          goto(rowActive)
        }
        is(REF) {
          goto(selfRefresh)
        }
        is(CBR) {
          goto(cbrRefresh)
        }
        is(MRS) {
          goto(modeRegisterSet)
        }
      }
    })
    rowActive.whenIsActive({
      counter := counter + 1
      switch(command) {
        is(READ) {
          when(counter >= 3) {
            goto(read)
          }
        }
        is(READA) {
          when(counter >= 3) {
            goto(readA)
          }
        }
        is(WRITE) {
          when(counter >= 3) {
            goto(write)
          }
        }
        is(WRITEA) {
          when(counter >= 3) {
            goto(writeA)
          }
        }
        is(PRE) {
          when(counter >= 7) {
            goto(precharge)
          }
        }
        is(PALL) {
          when(counter >= 7) {
            goto(prechargeAll)
          }
        }
        is(ACT) {
          when(bankAddress === bankAddressLast) {
            when(counter >= 10) {
              goto(rowActive)
            }
          } otherwise {
            when(counter >= 2) {
              goto(rowActive)
            }
          }
        }
      }
    })

    rowActive.onExit(counter := counter)

    read.whenIsActive({
      when(command === SUSP) {
        goto(readSuspend)
      } elsewhen (command === NOP || command === BST) {
        addressStream.valid := True
        burstTest(rowActive)
      } elsewhen (command === READ) {
        goto(read)
      } elsewhen (command === READA) {
        goto(readA)
      } elsewhen (command === WRITE) {
        goto(write)
      } elsewhen (command === WRITEA) {
        goto(writeA)
      }
      colAddress := nextColAddress(counter)
      counter    := counter + 1

    })
    read.onExit(counter := 0)

    readA.whenIsActive({
      when(command === SUSP) {
        goto(readSuspend)
      } elsewhen (command === NOP || command === BST) {
        addressStream.valid := True
        burstTest(precharge)
      } elsewhen (command === READ) {
        goto(read)
      } elsewhen (command === READA) {
        goto(readA)
      } elsewhen (command === WRITE) {
        goto(write)
      } elsewhen (command === WRITEA) {
        goto(writeA)
      }
      colAddress := nextColAddress(counter)
      counter    := counter + 1
    })
    readA.onExit(counter := 0)

    write.whenIsActive({
      when(command === SUSP) {
        goto(readSuspend)
      } elsewhen (command === NOP || command === BST) {
        addressStream.valid := True
        burstTest(rowActive)
      } elsewhen (command === READ) {
        goto(read)
      } elsewhen (command === READA) {
        goto(readA)
      } elsewhen (command === WRITE) {
        goto(write)
      } elsewhen (command === WRITEA) {
        goto(writeA)
      }
      addressStream.write := True
      colAddress          := nextColAddress(counter)
      counter             := counter + 1
    })
    write.onExit(counter := 0)

    writeA.whenIsActive({
      when(command === SUSP) {
        goto(readSuspend)
      } elsewhen (command === NOP || command === BST) {
        addressStream.valid := True
        burstTest(precharge)
      } elsewhen (command === READ) {
        goto(read)
      } elsewhen (command === READA) {
        goto(readA)
      } elsewhen (command === WRITE) {
        goto(write)
      } elsewhen (command === WRITEA) {
        goto(writeA)
      }
      addressStream.write := True
      colAddress          := nextColAddress(counter)
      counter             := counter + 1
    })
    writeA.onExit(counter := 0)

    precharge.whenIsActive({
      counter := 0
      when(counter === 2) {
        goto(idle)
      }
    })
    precharge.onExit(counter := 0)

    writeSuspend.whenIsActive({
      when(command =/= SUSP) {
        goto(write)
      }
    })

    writeSuspendA.whenIsActive({
      when(command =/= SUSP) {
        goto(writeA)
      }
    })

    readSuspend.whenIsActive({
      when(command =/= SUSP) {
        goto(read)
      }
    })

    readSuspendA.whenIsActive({
      when(command =/= SUSP) {
        goto(readA)
      }
    })

    modeRegisterSet.whenIsActive({
      counter := counter + 1
      when(counter === 0) {
        MR.assignFromBits(io.sdram.ADDR, 9, 0)
      }
      when(counter === 1) {
        goto(idle)
      }
    })
    modeRegisterSet.onExit(counter := 0)

    selfRefresh.whenIsActive({
      when(command =/= SUSP) {
        when(counter >= 10) {
          goto(idle)
        }
      }
      counter := counter + 1
    })
    selfRefresh.onExit(counter := 0)

    cbrRefresh.whenIsActive({
      goto(precharge)
    })

  }

  val (readCmd, writeCmd) = StreamFork2(addressStream)

  val outputAddress = readCmd
    .throwWhen(readCmd.write)
    .s2mPipe(stagesCount = 1)
    .m2sPipe()
  for (i <- 0 until layout.bytePerWord) {
    io.sdram.DQ.read(i * 8 + 7 downto i * 8) := memory.readSync(
      outputAddress.payload.address + U(i)
    )
  }
  outputAddress.ready := True

  val writeAddress =
    writeCmd.throwWhen(!writeCmd.write).s2mPipe().m2sPipe()
  for (i <- 0 until layout.bytePerWord) {
    when(writeAddress.mask(i)) {
      memory(writeAddress.payload.address + U(i)) := writeAddress.data(
        i * 8 + 7 downto i * 8
      )
    }
  }
  writeAddress.ready := True
}
