package top

import com.sun.jna.Library
import com.sun.jna.Native
import com.sun.jna.Platform

trait NOOPDevice extends Library {
  def init_sdl(): Unit
  def update_screen(mem: Array[Int]): Unit
  def read_key(): Int
  def poll_event(): Int
}

object NOOPDevice {
  private val lib: NOOPDevice = {
    System.setProperty("jna.library.path", sys.env("NOOP_HOME") + "/src/test/cpp/libdevice/build")
    Native.loadLibrary("device", classOf[NOOPDevice]).asInstanceOf[NOOPDevice]
  }
  def call = lib
}
