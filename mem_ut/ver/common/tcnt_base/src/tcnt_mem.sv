/** @tcnt_mem.sv */

`ifndef TCNT_MEM__SV
`define TCNT_MEM__SV

class tcnt_mem #(int ADDR_WIDTH=32, int DATA_WIDTH=128) extends uvm_object;
    protected bit[DATA_WIDTH-1:0] mem_array[bit[ADDR_WIDTH-1:0]];
    protected longint full_bus_byte_num;
    typedef enum int{
        ZERO = 32'h0,
        ONE = 32'h1,
        INCR_8BIT = 32'h2,
        INCR_16BIT = 32'h3,
        INCR_32BIT = 32'h4,
        RANDOM = 32'hf
    } init_value_e;
    init_value_e init_value=ZERO;
    //2022-5-10,required by tianzhu core ver-team
    bit ignore_write_addr_align_chk;
    bit ignore_read_addr_align_chk;
    protected bit read_with_update=1'b0;
    //end

    `uvm_object_param_utils(tcnt_mem#(ADDR_WIDTH,DATA_WIDTH))


    /**
    * Constructor
    */
    function new(string name="tcnt_mem");
        super.new(name);

        full_bus_byte_num=DATA_WIDTH/8;
        `uvm_info(get_type_name(),$psprintf("ADDR_WIDTH=%0d, DATA_WIDTH=%0d",ADDR_WIDTH,DATA_WIDTH),UVM_DEBUG);
    endfunction:new


    /**
    * init_mem()
    * Init the mem_array with specified pattern
    */
    extern function void init_mem(input init_value_e init_value_t);


    /**
    * write_byte()
    * write a byte of data into the mem_array
    */
    //extern function void write_byte(input bit[ADDR_WIDTH-1:0] addr, input bit[7:0] data);
    extern virtual function void write_byte(input bit[ADDR_WIDTH-1:0] addr, input bit[7:0] data); //2022-9-5,required by TZ-Core Ver


    /**
    * write_mem()
    * write data[DATA_WIDTH-1:0] into the mem_array with a align addr
    */
    extern function void write_mem(input bit[ADDR_WIDTH-1:0] addr, input bit[DATA_WIDTH-1:0] data);


    /**
    * read_byte()
    * read a byte of data from the mem_array
    */
    extern function void read_byte(input bit[ADDR_WIDTH-1:0] addr, output bit[7:0] data);


    /**
    * read_mem()
    * read data[DATA_WIDTH-1:0] from the mem_array with a align addr
    */
    extern function void read_mem(input bit[ADDR_WIDTH-1:0] addr, output bit[DATA_WIDTH-1:0] data);


    /**
    * display_mem()
    * display mem_array contents with specified addr range
    */
    extern function void display_mem(input bit[ADDR_WIDTH-1:0] addr, int length);

    /**
    * delete_mem()
    * delete mem_array contents
    */
    extern function void delete_mem();

    /**
    * rand_unwritten_region();
    */
    extern virtual function bit[DATA_WIDTH-1:0] rand_unwritten_region(input bit[ADDR_WIDTH-1:0] addr);
endclass


/**
* init_mem()
* Init the mem_array with specified pattern
*/
function void tcnt_mem::init_mem(input init_value_e init_value_t);
    this.init_value=init_value_t;
    this.read_with_update=1'b1;
    `uvm_info(get_type_name(),$psprintf("init_value=%0s",this.init_value.name()),UVM_LOW);
endfunction:init_mem


/**
* write_byte()
* write a byte of data into the mem_array
*/
function void tcnt_mem::write_byte(input bit[ADDR_WIDTH-1:0] addr, input bit[7:0] data);
    bit[ADDR_WIDTH-1:0] addr_offset;
    bit[ADDR_WIDTH-1:0] addr_base;

    addr_offset=addr%this.full_bus_byte_num;
    addr_base=addr-addr_offset;

    `uvm_info("before write_byte",$psprintf("addr='h%0h,addr_base='h%0h,addr_offset='h%0h,wdata=8'h%0h,mem_array[%0h]=%0h",addr,addr_base,addr_offset,data,addr_base,mem_array[addr_base]),UVM_DEBUG);

    this.mem_array[addr_base][addr_offset*8+:8]=data;

    `uvm_info("after write_byte",$psprintf("addr='h%0h,addr_base='h%0h,addr_offset='h%0h,wdata=8'h%0h,mem_array[%0h]=%0h",addr,addr_base,addr_offset,data,addr_base,mem_array[addr_base]),UVM_DEBUG);
endfunction:write_byte


/**
* write_mem()
* write data[DATA_WIDTH-1:0] into the mem_array with a align addr
*/
function void tcnt_mem::write_mem(input bit[ADDR_WIDTH-1:0] addr, input bit[DATA_WIDTH-1:0] data);

    if(this.ignore_write_addr_align_chk==1'b0)begin
        if(addr%full_bus_byte_num!=0)begin
            `uvm_error("write_mem()",$psprintf("Please input a align addr, addr=%0h",addr));
        end
        else begin
            `uvm_info("before write_mem",$psprintf("addr='h%0h,wdata='h%0h,mem_array[%0h]=%0h",addr,data,addr,mem_array[addr]),UVM_DEBUG);

            this.mem_array[addr][DATA_WIDTH-1:0]=data;

            `uvm_info("after write_mem",$psprintf("addr='h%0h,wdata='h%0h,mem_array[%0h]=%0h",addr,data,addr,mem_array[addr]),UVM_DEBUG);
        end
    end
    else begin
        `uvm_info("before write_mem",$psprintf("addr='h%0h,wdata='h%0h,mem_array[%0h]=%0h",addr,data,addr,mem_array[addr]),UVM_DEBUG);

        this.mem_array[addr][DATA_WIDTH-1:0]=data;

        `uvm_info("after write_mem",$psprintf("addr='h%0h,wdata='h%0h,mem_array[%0h]=%0h",addr,data,addr,mem_array[addr]),UVM_DEBUG);
    end
endfunction:write_mem


/**
* read_byte()
* read a byte of data from the mem_array
*/
function void tcnt_mem::read_byte(input bit[ADDR_WIDTH-1:0] addr, output bit[7:0] data);
    bit[ADDR_WIDTH-1:0] addr_offset;
    bit[ADDR_WIDTH-1:0] addr_base;
    bit[DATA_WIDTH-1:0] data_t;

    addr_offset=addr%this.full_bus_byte_num;
    addr_base=addr-addr_offset;
    
    if(this.mem_array.exists(addr_base))begin
        data=this.mem_array[addr_base][addr_offset*8+:8];

        `uvm_info("read_byte()",$psprintf("addr='h%0h,addr_base='h%0h,addr_offset='h%0h,rdata=8'h%0h,mem_array[%0h]=%0h",addr,addr_base,addr_offset,data,addr_base,mem_array[addr_base]),UVM_DEBUG);
    end
    else begin
        //data_t=this.rand_unwritten_region(addr);
        data_t=this.rand_unwritten_region(addr_base); //2022-8-26
        data=data_t[addr_offset*8+:8];

        `uvm_info("read_byte()",$psprintf("addr='h%0h,addr_base='h%0h,addr_offset='h%0h,rdata=8'h%0h,mem_array[%0h]=%0h",addr,addr_base,addr_offset,data,addr_base,data_t),UVM_DEBUG);
    end
endfunction:read_byte


/**
* read_mem()
* read data[DATA_WIDTH-1:0] from the mem_array with a align addr
*/
function void tcnt_mem::read_mem(input bit[ADDR_WIDTH-1:0] addr, output bit[DATA_WIDTH-1:0] data);
    bit[DATA_WIDTH-1:0] data_t;

    if(this.ignore_read_addr_align_chk==1'b0)begin
        if(addr%full_bus_byte_num!=0)begin
            `uvm_error("read_mem()",$psprintf("Please input a align addr, addr=%0h",addr));
            data=0;
        end
        else begin
            if(this.mem_array.exists(addr))begin
                data=this.mem_array[addr];

                `uvm_info("read_mem()",$psprintf("addr='h%0h,mem_array[%0h]=%0h",addr,addr,mem_array[addr]),UVM_DEBUG);
            end
            else begin
                data=this.rand_unwritten_region(addr);

                `uvm_info("read_mem()",$psprintf("addr='h%0h,mem_array[%0h]=%0h",addr,addr,data),UVM_DEBUG);
            end
        end
    end
    else begin
        if(this.mem_array.exists(addr))begin
            data=this.mem_array[addr];

            `uvm_info("read_mem()",$psprintf("addr='h%0h,mem_array[%0h]=%0h",addr,addr,mem_array[addr]),UVM_DEBUG);
        end
        else begin
            data=this.rand_unwritten_region(addr);

            `uvm_info("read_mem()",$psprintf("addr='h%0h,mem_array[%0h]=%0h",addr,addr,data_t),UVM_DEBUG);
        end
    end
endfunction:read_mem


/**
* display_mem()
* display slave memory contents with specified addr range
*/
function void tcnt_mem::display_mem(input bit[ADDR_WIDTH-1:0] addr, int length);
    bit[ADDR_WIDTH-1:0] addr_index;
    bit[DATA_WIDTH-1:0] data_t;

    `uvm_info("display_mem()",$psprintf("begin to display slave memory contents, start addr is %0h, end addr is %0h ...",addr, addr+(length-1)*this.full_bus_byte_num),UVM_NONE);

    for(int j=0;j<length;j++)begin
        addr_index=addr+j*this.full_bus_byte_num;
        
        if(this.mem_array.exists(addr_index))begin
            `uvm_info("display_mem()",$psprintf("addr=%0h, data=%0h",addr_index,this.mem_array[addr_index]),UVM_NONE);
        end
        else begin
            data_t=this.rand_unwritten_region(addr_index);

            `uvm_info("display_mem()",$psprintf("addr=%0h, data=%0h",addr_index, data_t),UVM_NONE);
        end
    end

    `uvm_info("display_mem()","displayed all slave memory contents",UVM_NONE);
endfunction:display_mem

/**
* delete_mem()
* delete mem_array contents
*/
function void tcnt_mem::delete_mem();
    this.mem_array.delete();
    this.init_value=ZERO;
    this.read_with_update=1'b0;
endfunction:delete_mem

/**
* rand_unwritten_region();
*/
function bit[tcnt_mem::DATA_WIDTH-1:0] tcnt_mem::rand_unwritten_region(input bit[ADDR_WIDTH-1:0] addr);
    bit[DATA_WIDTH-1:0] data_t;
    int unsigned incr_rpt;

    case(this.init_value)
        ZERO:begin
            data_t=0;
        end
        ONE:begin
            data_t={DATA_WIDTH{1'b1}};
        end
        INCR_8BIT:begin
            incr_rpt=DATA_WIDTH/8;
            for(int i=0;i<incr_rpt;i++)begin
                if(i==0)begin
                    data_t[i*8+:8]=8'h0;
                end
                else begin
                    data_t[i*8+:8]=data_t[(i-1)*8+:8]+8'h1;
                end
            end
        end
        INCR_16BIT:begin
            if(DATA_WIDTH<16)begin
                `uvm_error("read_mem()","DATA_WIDTH less than 16bit");
            end
            else begin
                incr_rpt=DATA_WIDTH/16;
                for(int i=0;i<incr_rpt;i++)begin
                    if(i==0)begin
                        data_t[i*16+:16]=16'h0;
                    end
                    else begin
                        data_t[i*16+:16]=data_t[(i-1)*16+:16]+16'h1;
                    end
                end
            end
        end
        INCR_32BIT:begin
            if(DATA_WIDTH<32)begin
                `uvm_error("read_mem()","DATA_WIDTH less than 32bit");
            end
            else begin
                incr_rpt=DATA_WIDTH/32;
                for(int i=0;i<incr_rpt;i++)begin
                    if(i==0)begin
                        data_t[i*32+:32]=32'h0;
                    end
                    else begin
                        data_t[i*32+:32]=data_t[(i-1)*32+:32]+32'h1;
                    end
                end
            end
        end
        RANDOM:begin
            void'(std::randomize(data_t));

            //required by tianchu core ver team, 2022-6-13
            if(this.read_with_update==1'b1)begin
                this.mem_array[addr][DATA_WIDTH-1:0]=data_t;
            end
        end
    endcase
    return data_t;
endfunction:rand_unwritten_region
`endif
