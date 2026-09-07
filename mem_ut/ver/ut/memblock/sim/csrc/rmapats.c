// file = 0; split type = patterns; threshold = 100000; total count = 0.
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "rmapats.h"

void  hsG_0__0 (struct dummyq_struct * I1328, EBLK  * I1323, U  I696);
void  hsG_0__0 (struct dummyq_struct * I1328, EBLK  * I1323, U  I696)
{
    U  I1589;
    U  I1590;
    U  I1591;
    struct futq * I1592;
    struct dummyq_struct * pQ = I1328;
    I1589 = ((U )vcs_clocks) + I696;
    I1591 = I1589 & ((1 << fHashTableSize) - 1);
    I1323->I741 = (EBLK  *)(-1);
    I1323->I742 = I1589;
    if (0 && rmaProfEvtProp) {
        vcs_simpSetEBlkEvtID(I1323);
    }
    if (I1589 < (U )vcs_clocks) {
        I1590 = ((U  *)&vcs_clocks)[1];
        sched_millenium(pQ, I1323, I1590 + 1, I1589);
    }
    else if ((peblkFutQ1Head != ((void *)0)) && (I696 == 1)) {
        I1323->I744 = (struct eblk *)peblkFutQ1Tail;
        peblkFutQ1Tail->I741 = I1323;
        peblkFutQ1Tail = I1323;
    }
    else if ((I1592 = pQ->I1231[I1591].I764)) {
        I1323->I744 = (struct eblk *)I1592->I762;
        I1592->I762->I741 = (RP )I1323;
        I1592->I762 = (RmaEblk  *)I1323;
    }
    else {
        sched_hsopt(pQ, I1323, I1589);
    }
}
#ifdef __cplusplus
extern "C" {
#endif
void SinitHsimPats(void);
#ifdef __cplusplus
}
#endif
