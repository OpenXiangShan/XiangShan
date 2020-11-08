OUTPUT_ARCH( "riscv" )
ENTRY(_start)

SECTIONS
{
  .text.init : { *(.text.init) }
  . = ALIGN(0x1000);
  .tohost : { *(.tohost) }
  . = ALIGN(0x1000);
  .text : { *(.text) }
  . = ALIGN(0x1000);
  .data : { *(.data) }
  .data.string : { *(.data.string) }
  .bss : { *(.bss) }
	PROVIDE(read_signature = 0xffc00);
  _end = .;
}

