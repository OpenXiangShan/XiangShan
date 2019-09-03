
################################################################
# This is a generated script based on design: noop
#
# Though there are limitations about the generated script,
# the main purpose of this utility is to make learning
# IP Integrator Tcl commands easier.
################################################################

namespace eval _tcl {
proc get_script_folder {} {
   set script_path [file normalize [info script]]
   set script_folder [file dirname $script_path]
   return $script_folder
}
}
variable script_folder
set script_folder [_tcl::get_script_folder]

################################################################
# Check if script is running in correct Vivado version.
################################################################
set scripts_vivado_version 2019.1
set current_vivado_version [version -short]

if { [string first $scripts_vivado_version $current_vivado_version] == -1 } {
   puts ""
   catch {common::send_msg_id "BD_TCL-109" "ERROR" "This script was generated using Vivado <$scripts_vivado_version> and is being run in <$current_vivado_version> of Vivado. Please run the script in Vivado <$scripts_vivado_version> then open the design in Vivado <$current_vivado_version>. Upgrade the design by running \"Tools => Report => Report IP Status...\", then run write_bd_tcl to create an updated script."}

   return 1
}

################################################################
# START
################################################################

# To test this script, run the following commands from Vivado Tcl console:
# source noop_script.tcl


# The design that will be created by this Tcl script contains the following 
# module references:
# NOOPSoC, AXI4Timer, AXI4VGA

# Please add the sources of those modules before sourcing this Tcl script.

# If there is no project opened, this script will create a
# project, but make sure you do not have an existing project
# <./myproj/project_1.xpr> in the current working folder.

set list_projs [get_projects -quiet]
if { $list_projs eq "" } {
   create_project project_1 myproj -part xczu2eg-sfva625-1-e
   set_property BOARD_PART interwiser:none:part0:2.0 [current_project]
}


# CHANGE DESIGN NAME HERE
variable design_name
set design_name noop

# If you do not already have an existing IP Integrator design open,
# you can create a design using the following command:
#    create_bd_design $design_name

# Creating design if needed
set errMsg ""
set nRet 0

set cur_design [current_bd_design -quiet]
set list_cells [get_bd_cells -quiet]

if { ${design_name} eq "" } {
   # USE CASES:
   #    1) Design_name not set

   set errMsg "Please set the variable <design_name> to a non-empty value."
   set nRet 1

} elseif { ${cur_design} ne "" && ${list_cells} eq "" } {
   # USE CASES:
   #    2): Current design opened AND is empty AND names same.
   #    3): Current design opened AND is empty AND names diff; design_name NOT in project.
   #    4): Current design opened AND is empty AND names diff; design_name exists in project.

   if { $cur_design ne $design_name } {
      common::send_msg_id "BD_TCL-001" "INFO" "Changing value of <design_name> from <$design_name> to <$cur_design> since current design is empty."
      set design_name [get_property NAME $cur_design]
   }
   common::send_msg_id "BD_TCL-002" "INFO" "Constructing design in IPI design <$cur_design>..."

} elseif { ${cur_design} ne "" && $list_cells ne "" && $cur_design eq $design_name } {
   # USE CASES:
   #    5) Current design opened AND has components AND same names.

   set errMsg "Design <$design_name> already exists in your project, please set the variable <design_name> to another value."
   set nRet 1
} elseif { [get_files -quiet ${design_name}.bd] ne "" } {
   # USE CASES: 
   #    6) Current opened design, has components, but diff names, design_name exists in project.
   #    7) No opened design, design_name exists in project.

   set errMsg "Design <$design_name> already exists in your project, please set the variable <design_name> to another value."
   set nRet 2

} else {
   # USE CASES:
   #    8) No opened design, design_name not in project.
   #    9) Current opened design, has components, but diff names, design_name not in project.

   common::send_msg_id "BD_TCL-003" "INFO" "Currently there is no design <$design_name> in project, so creating one..."

   create_bd_design $design_name

   common::send_msg_id "BD_TCL-004" "INFO" "Making design <$design_name> as current_bd_design."
   current_bd_design $design_name

}

common::send_msg_id "BD_TCL-005" "INFO" "Currently the variable <design_name> is equal to \"$design_name\"."

if { $nRet != 0 } {
   catch {common::send_msg_id "BD_TCL-114" "ERROR" $errMsg}
   return $nRet
}

set bCheckIPsPassed 1
##################################################################
# CHECK IPs
##################################################################
set bCheckIPs 1
if { $bCheckIPs == 1 } {
   set list_check_ips "\ 
xilinx.com:ip:util_vector_logic:2.0\
xilinx.com:ip:axi_uartlite:2.0\
"

   set list_ips_missing ""
   common::send_msg_id "BD_TCL-006" "INFO" "Checking if the following IPs exist in the project's IP catalog: $list_check_ips ."

   foreach ip_vlnv $list_check_ips {
      set ip_obj [get_ipdefs -all $ip_vlnv]
      if { $ip_obj eq "" } {
         lappend list_ips_missing $ip_vlnv
      }
   }

   if { $list_ips_missing ne "" } {
      catch {common::send_msg_id "BD_TCL-115" "ERROR" "The following IPs are not found in the IP Catalog:\n  $list_ips_missing\n\nResolution: Please add the repository containing the IP(s) to the project." }
      set bCheckIPsPassed 0
   }

}

##################################################################
# CHECK Modules
##################################################################
set bCheckModules 1
if { $bCheckModules == 1 } {
   set list_check_mods "\ 
NOOPSoC\
AXI4Timer\
AXI4VGA\
"

   set list_mods_missing ""
   common::send_msg_id "BD_TCL-006" "INFO" "Checking if the following modules exist in the project's sources: $list_check_mods ."

   foreach mod_vlnv $list_check_mods {
      if { [can_resolve_reference $mod_vlnv] == 0 } {
         lappend list_mods_missing $mod_vlnv
      }
   }

   if { $list_mods_missing ne "" } {
      catch {common::send_msg_id "BD_TCL-115" "ERROR" "The following module(s) are not found in the project: $list_mods_missing" }
      common::send_msg_id "BD_TCL-008" "INFO" "Please add source files for the missing module(s) above."
      set bCheckIPsPassed 0
   }
}

if { $bCheckIPsPassed != 1 } {
  common::send_msg_id "BD_TCL-1003" "WARNING" "Will not continue with creation of design due to the error(s) above."
  return 3
}

##################################################################
# DESIGN PROCs
##################################################################


# Hierarchical cell: hier_devices
proc create_hier_cell_hier_devices { parentCell nameHier } {

  variable script_folder

  if { $parentCell eq "" || $nameHier eq "" } {
     catch {common::send_msg_id "BD_TCL-102" "ERROR" "create_hier_cell_hier_devices() - Empty argument(s)!"}
     return
  }

  # Get object for parentCell
  set parentObj [get_bd_cells $parentCell]
  if { $parentObj == "" } {
     catch {common::send_msg_id "BD_TCL-100" "ERROR" "Unable to find parent cell <$parentCell>!"}
     return
  }

  # Make sure parentObj is hier blk
  set parentType [get_property TYPE $parentObj]
  if { $parentType ne "hier" } {
     catch {common::send_msg_id "BD_TCL-101" "ERROR" "Parent <$parentObj> has TYPE = <$parentType>. Expected to be <hier>."}
     return
  }

  # Save current instance; Restore later
  set oldCurInst [current_bd_instance .]

  # Set parent object as current
  current_bd_instance $parentObj

  # Create cell and set as current instance
  set hier_obj [create_bd_cell -type hier $nameHier]
  current_bd_instance $hier_obj

  # Create interface pins
  create_bd_intf_pin -mode Slave -vlnv xilinx.com:interface:aximm_rtl:1.0 S_AXI

  create_bd_intf_pin -mode Master -vlnv xilinx.com:interface:uart_rtl:1.0 uart


  # Create pins
  create_bd_pin -dir I -type clk clk50
  create_bd_pin -dir I -type clk coreclk
  create_bd_pin -dir I -from 0 -to 0 -type rst corerstn
  create_bd_pin -dir O -from 3 -to 0 io_b
  create_bd_pin -dir O -from 3 -to 0 io_g
  create_bd_pin -dir O io_hsync
  create_bd_pin -dir O -from 3 -to 0 io_r
  create_bd_pin -dir O io_vsync
  create_bd_pin -dir I -type rst rstn50

  # Create instance: AXI4Timer_0, and set properties
  set block_name AXI4Timer
  set block_cell_name AXI4Timer_0
  if { [catch {set AXI4Timer_0 [create_bd_cell -type module -reference $block_name $block_cell_name] } errmsg] } {
     catch {common::send_msg_id "BD_TCL-105" "ERROR" "Unable to add referenced block <$block_name>. Please add the files for ${block_name}'s definition into the project."}
     return 1
   } elseif { $AXI4Timer_0 eq "" } {
     catch {common::send_msg_id "BD_TCL-106" "ERROR" "Unable to referenced block <$block_name>. Please add the files for ${block_name}'s definition into the project."}
     return 1
   }
  
  # Create instance: AXI4VGA_0, and set properties
  set block_name AXI4VGA
  set block_cell_name AXI4VGA_0
  if { [catch {set AXI4VGA_0 [create_bd_cell -type module -reference $block_name $block_cell_name] } errmsg] } {
     catch {common::send_msg_id "BD_TCL-105" "ERROR" "Unable to add referenced block <$block_name>. Please add the files for ${block_name}'s definition into the project."}
     return 1
   } elseif { $AXI4VGA_0 eq "" } {
     catch {common::send_msg_id "BD_TCL-106" "ERROR" "Unable to referenced block <$block_name>. Please add the files for ${block_name}'s definition into the project."}
     return 1
   }
  
  # Create instance: axi_interconnect_0, and set properties
  set axi_interconnect_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_interconnect:2.1 axi_interconnect_0 ]
  set_property -dict [ list \
   CONFIG.NUM_MI {4} \
   CONFIG.NUM_SI {1} \
 ] $axi_interconnect_0

  # Create instance: axi_uartlite_0, and set properties
  set axi_uartlite_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_uartlite:2.0 axi_uartlite_0 ]
  set_property -dict [ list \
   CONFIG.C_BAUDRATE {115200} \
 ] $axi_uartlite_0

  # Create instance: util_vector_logic_0, and set properties
  set util_vector_logic_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:util_vector_logic:2.0 util_vector_logic_0 ]
  set_property -dict [ list \
   CONFIG.C_OPERATION {not} \
   CONFIG.C_SIZE {1} \
   CONFIG.LOGO_FILE {data/sym_notgate.png} \
 ] $util_vector_logic_0

  # Create interface connections
  connect_bd_intf_net -intf_net Conn1 [get_bd_intf_pins uart] [get_bd_intf_pins axi_uartlite_0/UART]
  connect_bd_intf_net -intf_net S_AXI_1 [get_bd_intf_pins S_AXI] [get_bd_intf_pins axi_interconnect_0/S00_AXI]
  connect_bd_intf_net -intf_net axi_interconnect_0_M00_AXI [get_bd_intf_pins axi_interconnect_0/M00_AXI] [get_bd_intf_pins axi_uartlite_0/S_AXI]
  connect_bd_intf_net -intf_net axi_interconnect_0_M01_AXI [get_bd_intf_pins AXI4Timer_0/io_in] [get_bd_intf_pins axi_interconnect_0/M01_AXI]
  connect_bd_intf_net -intf_net axi_interconnect_0_M02_AXI [get_bd_intf_pins AXI4VGA_0/io_in_ctrl] [get_bd_intf_pins axi_interconnect_0/M02_AXI]
  connect_bd_intf_net -intf_net axi_interconnect_0_M03_AXI [get_bd_intf_pins AXI4VGA_0/io_in_fb] [get_bd_intf_pins axi_interconnect_0/M03_AXI]

  # Create port connections
  connect_bd_net -net AXI4VGA_0_io_vga_b [get_bd_pins io_b] [get_bd_pins AXI4VGA_0/io_vga_b]
  connect_bd_net -net AXI4VGA_0_io_vga_g [get_bd_pins io_g] [get_bd_pins AXI4VGA_0/io_vga_g]
  connect_bd_net -net AXI4VGA_0_io_vga_hsync [get_bd_pins io_hsync] [get_bd_pins AXI4VGA_0/io_vga_hsync]
  connect_bd_net -net AXI4VGA_0_io_vga_r [get_bd_pins io_r] [get_bd_pins AXI4VGA_0/io_vga_r]
  connect_bd_net -net AXI4VGA_0_io_vga_vsync [get_bd_pins io_vsync] [get_bd_pins AXI4VGA_0/io_vga_vsync]
  connect_bd_net -net clk50_1 [get_bd_pins clk50] [get_bd_pins AXI4Timer_0/clock] [get_bd_pins AXI4VGA_0/clock] [get_bd_pins axi_interconnect_0/ACLK] [get_bd_pins axi_interconnect_0/M00_ACLK] [get_bd_pins axi_interconnect_0/M01_ACLK] [get_bd_pins axi_interconnect_0/M02_ACLK] [get_bd_pins axi_interconnect_0/M03_ACLK] [get_bd_pins axi_uartlite_0/s_axi_aclk]
  connect_bd_net -net coreclk_1 [get_bd_pins coreclk] [get_bd_pins axi_interconnect_0/S00_ACLK]
  connect_bd_net -net proc_sys_reset_0_interconnect_aresetn [get_bd_pins rstn50] [get_bd_pins axi_interconnect_0/ARESETN] [get_bd_pins axi_interconnect_0/M00_ARESETN] [get_bd_pins axi_interconnect_0/M01_ARESETN] [get_bd_pins axi_interconnect_0/M02_ARESETN] [get_bd_pins axi_interconnect_0/M03_ARESETN] [get_bd_pins axi_uartlite_0/s_axi_aresetn] [get_bd_pins util_vector_logic_0/Op1]
  connect_bd_net -net uncorerstn_1 [get_bd_pins corerstn] [get_bd_pins axi_interconnect_0/S00_ARESETN]
  connect_bd_net -net util_vector_logic_0_Res [get_bd_pins AXI4Timer_0/reset] [get_bd_pins AXI4VGA_0/reset] [get_bd_pins util_vector_logic_0/Res]

  # Restore current instance
  current_bd_instance $oldCurInst
}


# Procedure to create entire design; Provide argument to make
# procedure reusable. If parentCell is "", will use root.
proc create_root_design { parentCell } {

  variable script_folder
  variable design_name

  if { $parentCell eq "" } {
     set parentCell [get_bd_cells /]
  }

  # Get object for parentCell
  set parentObj [get_bd_cells $parentCell]
  if { $parentObj == "" } {
     catch {common::send_msg_id "BD_TCL-100" "ERROR" "Unable to find parent cell <$parentCell>!"}
     return
  }

  # Make sure parentObj is hier blk
  set parentType [get_property TYPE $parentObj]
  if { $parentType ne "hier" } {
     catch {common::send_msg_id "BD_TCL-101" "ERROR" "Parent <$parentObj> has TYPE = <$parentType>. Expected to be <hier>."}
     return
  }

  # Save current instance; Restore later
  set oldCurInst [current_bd_instance .]

  # Set parent object as current
  current_bd_instance $parentObj


  # Create interface ports
  set AXI_MEM [ create_bd_intf_port -mode Master -vlnv xilinx.com:interface:aximm_rtl:1.0 AXI_MEM ]
  set_property -dict [ list \
   CONFIG.ADDR_WIDTH {32} \
   CONFIG.CLK_DOMAIN {/clk_wiz_0_clk_out1} \
   CONFIG.DATA_WIDTH {32} \
   CONFIG.NUM_READ_OUTSTANDING {2} \
   CONFIG.NUM_WRITE_OUTSTANDING {2} \
   CONFIG.PHASE {0.0} \
   CONFIG.PROTOCOL {AXI4} \
   ] $AXI_MEM

  set uart [ create_bd_intf_port -mode Master -vlnv xilinx.com:interface:uart_rtl:1.0 uart ]


  # Create ports
  set VGA_b [ create_bd_port -dir O -from 3 -to 0 VGA_b ]
  set VGA_g [ create_bd_port -dir O -from 3 -to 0 VGA_g ]
  set VGA_hsync [ create_bd_port -dir O VGA_hsync ]
  set VGA_r [ create_bd_port -dir O -from 3 -to 0 VGA_r ]
  set VGA_vsync [ create_bd_port -dir O VGA_vsync ]
  set clk50 [ create_bd_port -dir I -type clk clk50 ]
  set_property -dict [ list \
   CONFIG.FREQ_HZ {50000000} \
 ] $clk50
  set coreclk [ create_bd_port -dir I -type clk coreclk ]
  set_property -dict [ list \
   CONFIG.FREQ_HZ {100000000} \
 ] $coreclk
  set corerstn [ create_bd_port -dir I -type data corerstn ]
  set rstn50 [ create_bd_port -dir I -type rst rstn50 ]
  set uncoreclk [ create_bd_port -dir I -type clk uncoreclk ]
  set_property -dict [ list \
   CONFIG.CLK_DOMAIN {/clk_wiz_0_clk_out1} \
   CONFIG.FREQ_HZ {100000000} \
   CONFIG.PHASE {0.0} \
 ] $uncoreclk
  set uncorerstn [ create_bd_port -dir I -type rst uncorerstn ]

  # Create instance: NOOPSoC_0, and set properties
  set block_name NOOPSoC
  set block_cell_name NOOPSoC_0
  if { [catch {set NOOPSoC_0 [create_bd_cell -type module -reference $block_name $block_cell_name] } errmsg] } {
     catch {common::send_msg_id "BD_TCL-105" "ERROR" "Unable to add referenced block <$block_name>. Please add the files for ${block_name}'s definition into the project."}
     return 1
   } elseif { $NOOPSoC_0 eq "" } {
     catch {common::send_msg_id "BD_TCL-106" "ERROR" "Unable to referenced block <$block_name>. Please add the files for ${block_name}'s definition into the project."}
     return 1
   }
  
  # Create instance: axi_interconnect_2, and set properties
  set axi_interconnect_2 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_interconnect:2.1 axi_interconnect_2 ]
  set_property -dict [ list \
   CONFIG.NUM_MI {1} \
   CONFIG.NUM_SI {1} \
 ] $axi_interconnect_2

  # Create instance: hier_devices
  create_hier_cell_hier_devices [current_bd_instance .] hier_devices

  # Create instance: util_vector_logic_0, and set properties
  set util_vector_logic_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:util_vector_logic:2.0 util_vector_logic_0 ]
  set_property -dict [ list \
   CONFIG.C_OPERATION {not} \
   CONFIG.C_SIZE {1} \
   CONFIG.LOGO_FILE {data/sym_notgate.png} \
 ] $util_vector_logic_0

  # Create interface connections
  connect_bd_intf_net -intf_net NOOPSoC_0_io_mmio [get_bd_intf_pins NOOPSoC_0/io_mmio] [get_bd_intf_pins hier_devices/S_AXI]
  connect_bd_intf_net -intf_net S00_AXI_1 [get_bd_intf_pins NOOPSoC_0/io_mem] [get_bd_intf_pins axi_interconnect_2/S00_AXI]
  connect_bd_intf_net -intf_net axi_clock_converter_0_M_AXI [get_bd_intf_ports AXI_MEM] [get_bd_intf_pins axi_interconnect_2/M00_AXI]
  connect_bd_intf_net -intf_net hier_devices_uart [get_bd_intf_ports uart] [get_bd_intf_pins hier_devices/uart]

  # Create port connections
  connect_bd_net -net c_shift_ram_0_Q [get_bd_ports corerstn] [get_bd_pins axi_interconnect_2/ARESETN] [get_bd_pins axi_interconnect_2/S00_ARESETN] [get_bd_pins hier_devices/corerstn] [get_bd_pins util_vector_logic_0/Op1]
  connect_bd_net -net clk50_1 [get_bd_ports clk50] [get_bd_pins hier_devices/clk50]
  connect_bd_net -net coreclk_1 [get_bd_ports coreclk] [get_bd_pins NOOPSoC_0/clock] [get_bd_pins axi_interconnect_2/ACLK] [get_bd_pins axi_interconnect_2/S00_ACLK] [get_bd_pins hier_devices/coreclk]
  connect_bd_net -net hier_devices_io_b [get_bd_ports VGA_b] [get_bd_pins hier_devices/io_b]
  connect_bd_net -net hier_devices_io_g [get_bd_ports VGA_g] [get_bd_pins hier_devices/io_g]
  connect_bd_net -net hier_devices_io_hsync [get_bd_ports VGA_hsync] [get_bd_pins hier_devices/io_hsync]
  connect_bd_net -net hier_devices_io_r [get_bd_ports VGA_r] [get_bd_pins hier_devices/io_r]
  connect_bd_net -net hier_devices_io_vsync [get_bd_ports VGA_vsync] [get_bd_pins hier_devices/io_vsync]
  connect_bd_net -net rstn50_1 [get_bd_ports rstn50] [get_bd_pins hier_devices/rstn50]
  connect_bd_net -net uncoreclk_1 [get_bd_ports uncoreclk] [get_bd_pins axi_interconnect_2/M00_ACLK]
  connect_bd_net -net uncorerstn_2 [get_bd_ports uncorerstn] [get_bd_pins axi_interconnect_2/M00_ARESETN]
  connect_bd_net -net util_vector_logic_0_Res [get_bd_pins NOOPSoC_0/reset] [get_bd_pins util_vector_logic_0/Res]

  # Create address segments
  create_bd_addr_seg -range 0x00010000 -offset 0x40700000 [get_bd_addr_spaces NOOPSoC_0/io_mmio] [get_bd_addr_segs hier_devices/AXI4Timer_0/io_in/reg0] SEG_AXI4Timer_0_reg0
  create_bd_addr_seg -range 0x00010000 -offset 0x40800000 [get_bd_addr_spaces NOOPSoC_0/io_mmio] [get_bd_addr_segs hier_devices/AXI4VGA_0/io_in_ctrl/reg0] SEG_AXI4VGA_0_reg0
  create_bd_addr_seg -range 0x00080000 -offset 0x40000000 [get_bd_addr_spaces NOOPSoC_0/io_mmio] [get_bd_addr_segs hier_devices/AXI4VGA_0/io_in_fb/reg0] SEG_AXI4VGA_0_reg01
  create_bd_addr_seg -range 0x10000000 -offset 0x80000000 [get_bd_addr_spaces NOOPSoC_0/io_mem] [get_bd_addr_segs AXI_MEM/Reg] SEG_AXI_MEM_Reg
  create_bd_addr_seg -range 0x00010000 -offset 0x40600000 [get_bd_addr_spaces NOOPSoC_0/io_mmio] [get_bd_addr_segs hier_devices/axi_uartlite_0/S_AXI/Reg] SEG_axi_uartlite_0_Reg


  # Restore current instance
  current_bd_instance $oldCurInst

  validate_bd_design
  save_bd_design
}
# End of create_root_design()


##################################################################
# MAIN FLOW
##################################################################

create_root_design ""


