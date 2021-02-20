#ifndef __JTAG_SERVER_H__
#define __JTAG_SERVER_H__

#include "jtag_common.h"

class VerilatorJtagServer {
public:
	enum {
		SUCCESS,
		ERROR,
		CLIENT_DISCONNECTED
	};

	VerilatorJtagServer(uint64_t period);
	~VerilatorJtagServer();

	int doJTAG(uint64_t t, uint8_t *tms, uint8_t *tdi, uint8_t *tck, uint8_t tdo);
	int init_jtag_server(int port, bool loopback_only);
	bool stop_simu;

private:
	int gen_clk(uint64_t t, int nb_period, uint8_t *tck, uint8_t tdo, uint8_t *captured_tdo, int restart, int get_tdo);
	void gen_clk_restart(void);
	int reset_tap(uint64_t t, uint8_t *tms, uint8_t *tck);
	int goto_run_test_idle(uint64_t t, uint8_t *tms, uint8_t *tck);
	int do_tms_seq(uint64_t t, int length, int nb_bits, unsigned char *buffer, uint8_t *tms, uint8_t *tck);
	int do_scan_chain(uint64_t t, int length, int nb_bits, unsigned char *buffer_out,
			  unsigned char *buffer_in, uint8_t *tms, uint8_t *tck, uint8_t *tdi, uint8_t tdo, int flip_tms);

	struct jtag_cmd packet;

	int jtag_state;
	int cmd_in_progress;
	int tms_flip;
	uint64_t tck_period;
};

#endif
