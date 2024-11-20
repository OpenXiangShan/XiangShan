MEMORY {
  FLASH (rxa) : ORIGIN = 0x10000000, LENGTH = 16M
  RAM (rwxa) : ORIGIN = 0x80000000, LENGTH = 64M
}

ENTRY(_start)

SECTIONS {
  .text : 
  {
    *(entry)
    _mainargs = .;
    *(.text)
  } >FLASH
  etext = .;
  _etext = .;
  
  .rodata : 
  {
    *(.rodata*)
  } >FLASH

  .data : 
  {
    sdata = .;
    *(.data)
    *(.sdata)
    edata = .;
  } >RAM AT > FLASH
  _data = .;
  _sidata = LOADADDR(.data);
  
  .bss : 
  {
    _bss_start = .;
    *(.bss*)
    *(.sbss*)
    *(.scommon)
    _bss_end   = .;
  } >RAM AT > FLASH
  _sibss = LOADADDR(.bss);
  end = .;
  _end = .;

  ram_start = .;

  . = ORIGIN(RAM);
  _stack_top = ALIGN(0x1000);
  . = _stack_top + 0x8000;
  _stack_pointer = .;
  end = .;
  _end = .;
  _heap_start = ALIGN(0x1000);
}