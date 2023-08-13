sed 's/|/ /g' | awk '

func get_state(code){

  ret = "Unknown State"
  switch(code){
    case 0:
      ret = "INVALID"
      break;
    case 1:
      ret = "BRANCH"
      break;
    case 2:
      ret = "TRUNK"
      break;
    case 3:
      ret = "TIP"
      break;
  }
  return ret
}

func get_self_dir(code){
  prefetch = 0
  num_clients = 1
  
  str = "clients: ["
  for(i = 0; i < num_clients; i++){
    client_state = and( rshift(code, i * 2 + prefetch), 0x03)
    str = sprintf("%s %s", str, get_state(client_state))
  }
  str = sprintf("%s ]", str)

  self_state_code = and( rshift(code, num_clients * 2 + prefetch), 0x03)
  self_dirty = and( rshift(code, 2 + num_clients * 2 + prefetch), 1)

  str = sprintf("%s self: [%s] dirty: %d", str, get_state(self_state_code), self_dirty)

  return str
}

func get_client_dir(dir){
  return dir
}

func get_dir(typeId, dir){
  if(typeId > 1){
    return get_client_dir(dir)
  } else {
    return get_self_dir(dir)
  }
}

func get_type(tpe){
  ret = "Unknown Type"
  switch(tpe){
    case 0:
      ret = "self_dir"
      break;
    case 1:
      ret = "self_tag"
      break;
    case 2:
      ret = "client_dir"
      break;
    case 3:
      ret = "client_tag"
      break;
  }
  return ret
}

{
  $1 = $NF;                         # timestamp
  $NF = "";                         # remove log id
  if ($7 == 0 || $7 == 2) {
    $3 = ""
    $5 = sprintf("dir: %s", get_dir($7, $5))
  } else {
    $3 = sprintf("tag: %lx", $3)
    $5 = ""
  }
  $4 = sprintf("set: %lx", $4)
  $6 = sprintf("way: %d", $6)
  $7 = get_type($7)                 # type id
}

1                                   # print every line
'
