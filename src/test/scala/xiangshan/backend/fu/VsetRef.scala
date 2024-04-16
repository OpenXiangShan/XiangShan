package xiangshan.backend.fu

object VsetRef{
  def VLEN: Int = 128
  def XLEN: Int = 64
  def ELEN: Int = 64
  def isKeepVL(func: Int): Boolean   = (func >> 1 & 1) == 1
  def isSetVlmax(func: Int): Boolean = (func & 1) == 1
  def isVsetivli(func: Int): Boolean = (func & 0xc0) == 0
  def isVsetvl(func: Int): Boolean   = (func >> 6 & 1) == 1

  def test(avlPre: Int, vsew: Int, vlmul: Int, vta: Boolean, vma: Boolean, villPre: Boolean, func: Int, oldVL: Int) = {
    val avl = if(this.isVsetivli(func)) avlPre & 0x1f else avlPre
    val vill = if(this.isVsetvl(func)) villPre else false
    println("[REF] --- avl：" + avl + " vsew:" + vsew + " vlmul:" + vlmul + " vta:" + vta + " vma:" + vma + " vill:" + vill + " func:" + func.toBinaryString + " oldVL:" + oldVL)
    val isKeepVl   = this.isKeepVL(func)
    val isSetVlmax = this.isSetVlmax(func)
    val isVsetivli = this.isVsetivli(func)

    val vsewValue  = 1 << vsew + 3
    val vflmul: Double = if ((vlmul & 0x4) == 0) 1 << vlmul else 1.0 / (1 << ((vlmul ^ 0x7) + 1))
    val vlmax = (VLEN/vsewValue) * vflmul

    println("[REF] --- vsewValue: " + vsewValue + " vflmul: " + vflmul + " vlmax: " + vlmax)

    val villegal = !(vflmul >= 0.125 && vflmul <= 8) || vsewValue > (vflmul min 1.0f) * ELEN || vill

    // set vl
    val vl = if(isVsetivli) {
      if(avl > vlmax) vlmax else avl
    }else if (isKeepVl) {
      oldVL
    } else if (isSetVlmax) {
      vlmax
    } else {
      if(avl > vlmax) vlmax else avl
    }
    if (villegal) {
      (0, true, false, false, 0, 0)
    }else{
      (vl.toInt, villegal, vta, vma, vsew, vlmul)
    }
  }

}
