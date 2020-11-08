OUTPUT_ARCH( "riscv" )
ENTRY(_start)

SECTIONS
{

  . = 0x00000000;
  .text.trap : { *(.text.trap) }

  . = 0x00000080;
  .text.init : { *(.text.init) }

  . = ALIGN(0x1000);
  .tohost : { *(.tohost) }

  . = ALIGN(0x1000);
  .text : { *(.text) }
  _etext = .;

  . = ALIGN(0x1000);
  .data : { *(.data) }
  .data.string : { *(.data.string)}
  _edata = .;

  .bss : { *(.bss) }
  _end = .;

}
