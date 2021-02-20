#ifndef __JTAG_COMMON_H__
#define __JTAG_COMMON_H__

#define JTAG_SERVER_SUCCESS  0
#define JTAG_SERVER_ERROR  1
#define JTAG_SERVER_TRY_LATER  2
#define JTAG_SERVER_CLIENT_DISCONNECTED  3

#define	XFERT_MAX_SIZE	512

// jtag_vpi packet structure
struct jtag_cmd {
	uint32_t cmd;
	unsigned char buffer_out[XFERT_MAX_SIZE];
	unsigned char buffer_in[XFERT_MAX_SIZE];
	uint32_t length;
	uint32_t nb_bits;
};

typedef void (*print_func_ptr_t)(char *);

void jtag_server_set_print_func(print_func_ptr_t f);
int jtag_server_create(int port, int loopback_only);
int jtag_server_wait_for_client(void);
int check_for_command(struct jtag_cmd *vpi);
int send_result_to_server(struct jtag_cmd *vpi);
void jtag_server_finish(void);

#endif
