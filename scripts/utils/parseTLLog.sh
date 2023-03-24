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
  echo = $2;
  user = $3;
  data_1 = $4;
  data_2 = $5;
  data_3 = $6;
  data_4 = $7;
  sink = $9;
  source = $10;

  $1 = $14;                         # timestamp
  $2 = $NF;                         # name
  $3 = chnstr($13)                  # channel
  $NF = "";                         # remove log id
  $6 = sink;
  $7 = source;
  $5 = paramstr($13, $11)           # param
  $4 = opstr($13, $12)              # opcode

  $8 = sprintf("%lx", $8)           # address
  $9  = sprintf("%016lx", data_1)
  $10 = sprintf("%016lx", data_2)
  $11 = sprintf("%016lx", data_3)
  $12 = sprintf("%016lx", data_4)

  $13 = sprintf("user: %lx", user);
  $14 = sprintf("echo: %lx", echo);
}

1                                   # print every line
'
