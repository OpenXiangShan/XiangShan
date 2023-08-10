sed 's/|/ /g' | awk --bignum '

func chnstr(chn) {
    if(chn == 1){
        return "A"
    } else if(chn == 2){
        return "B"
    } else if(chn == 4){
        return "C"
    }
    return "Unknown Channel"
}

func opstr(chn, op, msTask) {
    a_op[1] = "PutFullData"
    a_op[2] = "PutPartialData"
    a_op[3] = "ArithmeticData"
    a_op[4] = "LogicalData"
    a_op[5] = "Get"
    a_op[6] = "Hint"
    a_op[7] = "AcquireBlock"
    a_op[8] = "AcquirePerm"

    b_op[1] = "PutFullData"
    b_op[2] = "PutPartialData"
    b_op[3] = "ArithmeticData"
    b_op[4] = "LogicalData"
    b_op[5] = "Get"
    b_op[6] = "Hint"
    b_op[7] = "Probe"

    c_op[1] = "AccessAck"
    c_op[2] = "AccessAckData"
    c_op[3] = "HintAck"
    c_op[4] = "Invalid Opcode"
    c_op[5] = "ProbeAck"
    c_op[6] = "ProbeAckData"
    c_op[7] = "Release"
    c_op[8] = "ReleaseData"

    d_op[1] = "AccessAck"
    d_op[2] = "AccessAckData"
    d_op[3] = "HintAck"
    d_op[4] = "Invalid Opcode"
    d_op[5] = "Grant"
    d_op[6] = "GrantData"
    d_op[7] = "ReleaseAck"

    msa_op[2] = "AccessAckData"
    msa_op[3] = "HintAck"
    msa_op[5] = "Grant"
    msa_op[6] = "GrantData"
    msa_op[7] = "Release"
    msa_op[8] = "ReleaseData"

    ret = "Unknown OP"

    if(msTask == 0){
        switch(chn) {
            case 1:
                ret = a_op[op+1]
                break;
            case 2:
                ret = b_op[op+1]
                break;
            case 4:
                ret = c_op[op+1]
                break;
        }
    } else {
        switch(chn) {
            case 1:
                ret = msa_op[op+1]
                break;
            case 2:
                ret = c_op[op+1]
                break;
        }
    }
    return ret
}
func paramstr(op, param) {
    split("Grow NtoB_Grow NtoT_Grow BtoT", grow, "_")
    split("Cap toT_Cap toB_Cap toN", cap, "_")
    split("Shrink TtoB_Shrink TtoN_Shrink BtoN_Report TotT_Report BtoB_Report NtoN", report, "_")

    ret = "Reserved"
    if(op == 'AcquireBlock' || op == 'AcquirePerm'){
        ret = grow[param+1]
    } else if(op == 'Probe'){
        ret = cap[param+1]
    } else if(op == 'Release' || op == 'ReleaseData' || op == 'ProbeAck' || op == 'ProbeAckData'){
        ret = report[param+1]
    } else if(op == 'Grant' || op == 'GrantData'){
        ret = cap[param+1]
    }
    return ret
}
func taskstr(msTask) {
    if(msTask == 0){
        return "Chn "
    } else {
        return "Mshr"
    }
}
func fulladdr_tltest(tag, set, bank) {
    tagbits = 3;
    setbits = 7;
    bankbits = 0;
    return (tag * (2^(bankbits + setbits)) + (set * (2^bankbits)) + bank) * 64;
}
func fulladdr_xs(tag, set, bank) {
    tagbits = 19;
    setbits = 9;
    bankbits = 2;
    return (tag * (2^(bankbits + setbits)) + (set * (2^bankbits)) + bank) * 64;
}

# TODO: add param
{
    METAWWAY = $2;
    METAWVALID = $3;
    MSHRID = $4;
    ALLOCPTR = $5;
    ALLOCVALID = $6;
    DIRWAY= $7;
    DIRHIT = $8;
    SSET = $9;
    TAG = $10;
    OPCODE = $11;
    CHANNEL = $12;
    MSHRTASK = $13;
    STAMP = $14;
    SITE = $15;

    match(SITE, /[0-9]+$/)
    BANK = substr(SITE, RSTART, RLENGTH)
    ADDR = fulladdr_xs(TAG, SSET, BANK)

    $1 = STAMP;
    $2 = SITE;
    $3 = taskstr(MSHRTASK);
    $4 = chnstr(CHANNEL);
    $5 = sprintf("%14s |", opstr(CHANNEL, OPCODE, MSHRTASK));
    $6 = sprintf("%lx(%d)", TAG, TAG);
    $7 = sprintf("%lx(%d)\t", SSET, SSET);
    $8 = sprintf("%lx(%d)\t", ADDR, ADDR);

    $9 = sprintf("|DIR %d %d", DIRHIT, DIRWAY);
    $10 = sprintf("|ALLOC %d %2d", ALLOCVALID, ALLOCPTR);
    $11 = sprintf("|MSHRID %2d", MSHRID);
    $12 = sprintf("|METAW %d %d", METAWVALID, METAWWAY);

    $13 = "";
    $14 = "";
    $15 = "";
}

1                                   # print every line
'
