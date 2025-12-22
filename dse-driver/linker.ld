MEMORY {
  FLASH (rxai) : ORIGIN = 0x10000000, LENGTH = 128M  /* 增加FLASH大小 */
  RAM (rwxa) : ORIGIN = 0x80000000, LENGTH = 64M
}

ENTRY(_start)

SECTIONS {
  . = ORIGIN(FLASH);  /* 确保从FLASH起始地址开始 */
  
  .text : ALIGN(4)
  {
    . = ALIGN(4);
    *(entry)          /* 入口点段 */
    _mainargs = .;
    *(.text)
    *(.text.*)        /* 包含所有text相关段 */
    . = ALIGN(4);
  } >FLASH

  .eh_frame : {       /* 添加eh_frame段 */
    *(.eh_frame)
  } >FLASH
  
  .eh_frame_hdr : {   /* 添加eh_frame_hdr段 */
    *(.eh_frame_hdr)
  } >FLASH

  .rodata : ALIGN(4)
  {
    . = ALIGN(4);
    *(.rodata*)
    . = ALIGN(4);
  } >FLASH

  .data : ALIGN(4)
  {
    . = ALIGN(4);
    sdata = .;
    *(.data)
    *(.data.*)
    *(.sdata)
    *(.sdata.*)
    . = ALIGN(4);
    edata = .;
  } >RAM AT>FLASH

  _data = .;
  _sidata = LOADADDR(.data);

  .bss (NOLOAD) : ALIGN(4)  /* 添加NOLOAD属性 */
  {
    . = ALIGN(4);
    _bss_start = .;
    *(.bss*)
    *(.sbss*)
    *(.scommon)
    . = ALIGN(4);
    _bss_end = .;
  } >RAM

  . = ALIGN(4);
  _end = .;

  . = ORIGIN(RAM) + 0x1000;  /* 栈和堆的设置 */
  _stack_top = .;
  . = _stack_top + 0x8000;
  _stack_pointer = .;
  _heap_start = ALIGN(0x1000);
}