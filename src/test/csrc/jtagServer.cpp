/*
 * SPDX-FileCopyrightText: 2020 Jan Matyas
 * SPDX-FileCopyrightText: 2019 Olof Kindgren
 * SPDX-FileCopyrightText: 2014 Franck Jullien <franck.jullien@gmail.com>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <argp.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <fcntl.h>

#include "jtagServer.h"
#include "jtag_common.h"

#define DONE                    0
#define IN_PROGRESS             1

#define CMD_RESET               0
#define CMD_TMS_SEQ             1
#define CMD_SCAN_CHAIN          2
#define CMD_SCAN_CHAIN_FLIP_TMS 3
#define CMD_STOP_SIMU           4

#define CHECK_CMD               0
#define TAP_RESET               1
#define GOTO_IDLE               2
#define DO_TMS_SEQ              3
#define SCAN_CHAIN              4
#define FINISHED                5

int VerilatorJtagServer::gen_clk(uint64_t t, int nb_period, uint8_t *tck, uint8_t tdo,
				 uint8_t *captured_tdo, int restart, int get_tdo)
{
	static bool gen_clk_begin = 1;
	static bool tck_low = 1;
	static uint64_t time;
	static int cur_period = 0;
	static int capture_done = 0;

	if (restart) {
		cur_period = 0;
		gen_clk_begin = 1;
		tck_low = 1;
		return DONE;
	}

	if (gen_clk_begin) {
		gen_clk_begin = 0;
		time = t;
	}

	if (cur_period < (nb_period * tck_period)) {
		if (tck_low) {
			capture_done = 0;
			*tck = 0;
			if ((t - time) > tck_period) {
				time = t;
				tck_low = 0;
			}
			return IN_PROGRESS;
		} else {
			*tck = 1;
			if (get_tdo && !capture_done) {
				capture_done = 1;
				*captured_tdo = tdo;
			}
			if ((t - time) > tck_period) {
				time = t;
				tck_low = 1;
			}
			cur_period++;
			return IN_PROGRESS;
		}
	}

	capture_done = 0;
	gen_clk_begin = 1;
	return DONE;
}

void VerilatorJtagServer::gen_clk_restart()
{
	gen_clk(0, 0, NULL, 0, NULL, 1, 0);
}

int VerilatorJtagServer::reset_tap(uint64_t t, uint8_t *tms, uint8_t *tck)
{
	*tms = 1;
	if (gen_clk(t, 5, tck, -1, NULL, 0, 0) == DONE) {
		gen_clk_restart();
		*tms = 0;
		return DONE;
	}

	return IN_PROGRESS;
}

int VerilatorJtagServer::goto_run_test_idle(uint64_t t, uint8_t *tms, uint8_t *tck)
{
	*tms = 0;
	if (gen_clk(t, 1, tck, -1, NULL, 0, 0) == DONE) {
		gen_clk_restart();
		return DONE;
	}

	return IN_PROGRESS;
}

int VerilatorJtagServer::do_tms_seq(uint64_t t, int length, int nb_bits,
				    uint8_t *buffer, uint8_t *tms, uint8_t *tck)
{
	static int i = 0;
	static int j = 0;
	static unsigned char data;
	static int start = 1;
	int nb_bits_rem;
	int nb_bits_in_this_byte;

	// Number of bits to send in the last byte
	nb_bits_rem = nb_bits % 8;

	while (i < length) {
		// If we are in the last byte, we have to send only
		// nb_bits_rem bits. If not, we send the whole byte.
		nb_bits_in_this_byte = (i == (length - 1)) ? nb_bits_rem : 8;

		if (start) {
			data = buffer[i];
			start = 0;
		}

		while (j < nb_bits_in_this_byte) {
			*tms = 0;
			if ((data & 1) == 1)
				*tms = 1;
			if (gen_clk(t, 1, tck, 0, NULL, 0, 0) == DONE) {
				j++;
				data = data >> 1;
				gen_clk_restart();
			}
			return IN_PROGRESS;
		}

		j = 0;
		i++;
		start = 1;
		return IN_PROGRESS;
	}

	*tms = 0;
	i = 0;
	j = 0;
	start = 1;

	return DONE;
}

int VerilatorJtagServer::do_scan_chain(uint64_t t, int length, int nb_bits,
	unsigned char *buffer_out, unsigned char *buffer_in, uint8_t *tms,
	uint8_t *tck, uint8_t *tdi, uint8_t tdo, int flip_tms)
{
	static int i = 0;
	static int j = 0;
	static int index = 0;
	static int start = 1;
	static unsigned char data;
	static uint8_t captured_tdo;
	int nb_bits_rem;
	int nb_bits_in_this_byte;

	// Number of bits to send in the last byte
	nb_bits_rem = nb_bits % 8;

	while (i < length) {
		// If we are in the last byte, we have to send only
		// nb_bits_rem bits if it's not zero.
		// If not, we send the whole byte.
		nb_bits_in_this_byte = (i == (length - 1)) ? ((nb_bits_rem == 0) ? 8 : nb_bits_rem) : 8;

		if (start) {
			buffer_in[i] = 0;
			if (buffer_out)
				data = buffer_out[i];
			else
				data = 0;
			start = 0;
		}

		while (j < nb_bits_in_this_byte) {
			*tdi = 0;
			if ((data & 1) == 1)
				*tdi = 1;

			// On the last bit, set TMS to '1'
			if (((j == (nb_bits_in_this_byte - 1)) && (i == (length - 1))) && (flip_tms == 1))
				*tms = 1;

			if (gen_clk(t, 1, tck, tdo, &captured_tdo, 0, 1) == DONE) {
				if (captured_tdo)
					buffer_in[i] |= (1 << j);
				else
					buffer_in[i] &= ~(1 << j);
				j++;
				data = data >> 1;
				gen_clk_restart();
			}
			return IN_PROGRESS;
		}

		j = 0;
		i++;
		start = 1;
		return IN_PROGRESS;
	}

	*tdi = 0;
	*tms = 0;
	i = 0;
	j = 0;
	start = 1;

	return DONE;
}

namespace 
{
// Function to print messages from within jtag_common.c
void print_func(char *msg)
{
	// In Verilator, plain printf() suffices
	printf("%s", msg);
	fflush(stdout);
}
}

int VerilatorJtagServer::init_jtag_server(int port, bool loopback_only)
{
	::jtag_server_set_print_func(print_func);
	if (::jtag_server_create(port, loopback_only) != JTAG_SERVER_SUCCESS) {
		printf("Error: Could not create jtag_vpi server.\n");
		return ERROR;
	}
	if (::jtag_server_wait_for_client() != JTAG_SERVER_SUCCESS) {
		printf("Error: Could not accept incoming client connection.\n");
		return ERROR;
	}
	return SUCCESS;
}


int VerilatorJtagServer::doJTAG(
	uint64_t t, uint8_t *tms, uint8_t *tdi, uint8_t *tck, uint8_t tdo)
{
	int ret;
	switch (jtag_state) {

	case CHECK_CMD:
		ret = check_for_command(&packet);
		if (ret == JTAG_SERVER_SUCCESS) {
			// New command from OpenOCD arrived
			switch (packet.cmd) {
			case CMD_RESET:
				cmd_in_progress = 1;
				jtag_state = TAP_RESET;
				break;
			case CMD_TMS_SEQ:
				cmd_in_progress = 1;
				jtag_state = DO_TMS_SEQ;
				break;
			case CMD_SCAN_CHAIN:
				cmd_in_progress = 1;
				tms_flip = 0;
				jtag_state = SCAN_CHAIN;
				break;
			case CMD_SCAN_CHAIN_FLIP_TMS:
				cmd_in_progress = 1;
				tms_flip = 1;
				jtag_state = SCAN_CHAIN;
				break;
			case CMD_STOP_SIMU:
				stop_simu = true;
				break;
			default:
				break;
			}
		}
		else if (ret == JTAG_SERVER_TRY_LATER) {
			// No command from OpenOCD at the moment.
			// Nothing to do.
		}
		else if (ret == JTAG_SERVER_CLIENT_DISCONNECTED) {
			return CLIENT_DISCONNECTED;
		}
		else {
			printf("Error when trying receive data from jtag_vpi client.\n");
			return ERROR;
		}
		break;

	case TAP_RESET:
		if (reset_tap(t, tms, tck) == DONE) {
			jtag_state = GOTO_IDLE;
		}
		break;

	case GOTO_IDLE:
		if (goto_run_test_idle(t, tms, tck) == DONE) {
			cmd_in_progress = 0;
			jtag_state = CHECK_CMD;
		}
		break;

	case DO_TMS_SEQ:
		if (do_tms_seq(t, packet.length, packet.nb_bits, packet.buffer_out, tms, tck) == DONE) {
			cmd_in_progress = 0;
			jtag_state = CHECK_CMD;
		}
		break;

	case SCAN_CHAIN:
		*tms = 0;
		if (do_scan_chain(t, packet.length, packet.nb_bits, packet.buffer_out,
				  packet.buffer_in, tms, tck, tdi, tdo, tms_flip) == DONE) {
			cmd_in_progress = 0;
			jtag_state = CHECK_CMD;
			if (send_result_to_server(&packet) != JTAG_SERVER_SUCCESS) {
				printf("Error when sending data to jtag_vpi client. \n");
				return ERROR;
			}
		}
		break;

	case FINISHED:
		break;
	}

	return SUCCESS;
}

VerilatorJtagServer::VerilatorJtagServer(uint64_t period) {
	tck_period = period;
	jtag_state = CHECK_CMD;
	cmd_in_progress = 0;
	stop_simu = false;
}

VerilatorJtagServer::~VerilatorJtagServer() {
}
