package cpu

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram._
import spinal.lib.memory.sdram.sdr._
import spinal.lib.memory.sdram.SdramGeneration._

object IS42S16160B {
  def layout = SdramLayout(
    generation = SDR,
    bankWidth = 2,
    columnWidth = 8,
    rowWidth = 13,
    dataWidth = 16
  )
  def timingGrade7 = SdramTimings(
    bootRefreshCount = 8,
    tPOW = 200 us,
    tREF = 64 ms,
    tRC = 60 ns,
    tRFC = 60 ns,
    tRAS = 42 ns,
    tRP = 18 ns,
    tRCD = 18 ns,
    cMRD = 2,
    tWR = 6 ns,
    cWR = 1
  )
}
