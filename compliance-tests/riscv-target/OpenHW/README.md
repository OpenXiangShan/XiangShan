Failing tests 

All of these tests define a local trap handler.
the location of this trap handler does not fit with the vectored traps used in the cv32e40p
so when a trap occurs, it is going to the wrong location


These require further trap support
MISALIGN_JMP
MISALIGN_LDST
ECALL
EBREAK
