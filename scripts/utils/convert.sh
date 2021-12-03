sed 's/|/ /g' | awk --bignum '

func chnstr(chn) {
  split("A B C D E", channels, " ");
  return channels[chn + 1]
}

func opstr(chn, op) {

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

  ret = "Unknown OP"
  switch(chn) {
    case 0:
      ret = a_op[op+1]
      break;
    case 1:
      ret = b_op[op+1]
      break;
    case 2:
      ret = c_op[op+1]
      break;
    case 3:
      ret = d_op[op+1]
      break;
    case 4:
      ret = "GrantAck"
      break;
  }
  return ret
}

func paramstr(chn, param) {

  split("Grow NtoB_Grow NtoT_Grow BtoT", grow, "_")
	split("Cap toT_Cap toB_Cap toN", cap, "_")
	split("Shrink TtoB_Shrink TtoN_Shrink BtoN_Report TotT_Report BtoB_Report NtoN", report, "_")

  ret = "Reserved"
  switch(chn){
    case 0:
      ret = grow[param+1]
      break;
    case 1:
      ret = cap[param+1]
      break;
    case 2:
      ret = report[param+1]
      break;
    case 3:
      ret = cap[param+1]
      break;
  }
  return ret
}

{
  $1 = $NF;                         # timestamp
  $NF = "";                         # remove log id
  $5 = paramstr($3, $5)             # param
  $4 = opstr($3, $4)                # opcode
  $3 = chnstr($3)                   # channel
  for(i=8; i<=12; i++){
    if(i == 8){                     # col 8 is address
      $i = sprintf("%lx", $i);
    } else {                        # cols 9-12 are data
      $i = sprintf("%016lx", $i);
    }
  }
  $13 = sprintf("user: %lx", $13);
  $14 = sprintf("echo: %lx", $14);
}

1                                   # print every line
'
