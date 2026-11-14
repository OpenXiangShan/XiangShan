/* Single demand load PC across all forward/reverse stream segments. */
.section .text
.align 4
.global stream_walk
.type stream_walk, @function
stream_walk:
  li a3, 0
1:
  ld a4, 0(a0)
  add a3, a3, a4
  add a0, a0, a2
  addi a1, a1, -1
  bnez a1, 1b
  mv a0, a3
  ret
.size stream_walk, .-stream_walk
