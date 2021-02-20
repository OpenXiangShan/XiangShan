/*
 * SPDX-FileCopyrightText: 2020 Jan Matyas
 * SPDX-FileCopyrightText: 2019 Olof Kindgren
 * SPDX-FileCopyrightText: 2012 Franck Jullien <franck.jullien@gmail.com>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <netinet/tcp.h>

#include "jtag_common.h"

// TODO: Networking on win32 not yet supported (winsock2.h et al.)

// Default TCP port where to listen for incoming OpenOCD connections
#ifndef JTAG_VPI_DEFAULT_SERVER_PORT
#define JTAG_VPI_DEFAULT_SERVER_PORT  5555
#endif

// Security: By default, do not listen on external interfaces (only on 127.0.0.1)
#ifndef JTAG_VPI_IS_LOOPBACK_ONLY
#define JTAG_VPI_IS_LOOPBACK_ONLY  1
#endif

// Socket descriptors
static int listenfd = -1;  // Server socket (listening)
static int connfd = -1;    // Connected client

// Buffer for partially received packets
char pkt_buf[ sizeof(struct jtag_cmd) ];
unsigned pkt_buf_bytes = 0;

// Function for printing of messages.
// This is needed since the printing mechanism differs based on the simulator used.
print_func_ptr_t print_function = NULL;

// Macro to print a formatted message via the user-defined function
#define PRINT_BUF_SIZE 256
char print_buf[PRINT_BUF_SIZE];

#define PRINT_MSG( ...) \
	do { \
		if (print_function) { \
			snprintf( print_buf, PRINT_BUF_SIZE, __VA_ARGS__); \
			print_buf[PRINT_BUF_SIZE - 1] = '\0'; \
			print_function(print_buf); \
		} \
	} while (0)
    

static int is_host_little_endian(void);
static uint32_t from_little_endian_u32(uint32_t val);
static uint32_t to_little_endian_u32(uint32_t val);

/**
 * Set user-provided print function that will be used to display messages.
 */
void jtag_server_set_print_func( print_func_ptr_t f )
{
	print_function = f;
}

/**
 * Create jtag_vpi server socket and start listening.
 */
int jtag_server_create(int port, int loopback_only)
{
	struct sockaddr_in serv_addr;
	int ret;
	int fd;
	int opt_val;

	PRINT_MSG("Starting jtag_vpi server: interface %s, port %d/tcp ...\n", 
		loopback_only ? "127.0.0.1 (loopback)" : "0.0.0.0 (any)", port);

	// Create server socket
	fd = socket(AF_INET, SOCK_STREAM, 0);
	if (fd == -1) {
		PRINT_MSG("Failed to create jtag_vpi server socket: errno=%d, %s\n", 
			errno, strerror(errno));
		return JTAG_SERVER_ERROR;
	}
	listenfd = fd;

	// Allow immediate reuse of the server port number (skip TIME_WAIT state)
	opt_val = 1;
	ret = setsockopt(listenfd, SOL_SOCKET, SO_REUSEADDR, (const char*) &opt_val, sizeof(opt_val)); 
	if (ret == -1)
	{
		PRINT_MSG("Failed to disable TIME_WAIT on jtag_vpi socket: errno=%d, %s\n",
			errno, strerror(errno));
		return JTAG_SERVER_ERROR;
	}

	// Bind the socket to a local port
	memset(&serv_addr, '0', sizeof(serv_addr));
	serv_addr.sin_family = AF_INET;
	serv_addr.sin_addr.s_addr = htonl(loopback_only ? INADDR_LOOPBACK : INADDR_ANY);
	serv_addr.sin_port = htons(port);

	ret = bind(listenfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr));
	if (ret == -1) {
		PRINT_MSG("Failed to bind jtag_vpi server socket: errno=%d, %s\n", 
			errno, strerror(errno));
		return JTAG_SERVER_ERROR;
	}

	// Start listening for incoming connections
	ret = listen(listenfd, 1);
	if (ret == -1) {
		PRINT_MSG("Could not start listening for jtag_vpi clients: errno=%d, %s\n", 
			errno, strerror(errno));
		return JTAG_SERVER_ERROR;
	}

	PRINT_MSG("jtag_vpi server created.\n");
	PRINT_MSG("Waiting for client connection...\n");

	return JTAG_SERVER_SUCCESS;
}

/**
 * Wait (block) until a jtag_vpi client (OpenOCD) connects.
 */
int jtag_server_wait_for_client(void)
{
	int fd;
	int flags;
	int ret;

	// Accept a client (block)
	fd = accept(listenfd, (struct sockaddr*)NULL, NULL);
	if (fd == -1) {
		// Error accepting the client
		PRINT_MSG("Could not accept client connection: errno=%d, %s\n", 
			errno, strerror(errno));
		return JTAG_SERVER_ERROR;
	}

	connfd = fd;
	PRINT_MSG("Client connection accepted.\n");

	// Set the client socket to non-blocking mode
	flags = fcntl(connfd, F_GETFL, 0);
	ret = fcntl(connfd, F_SETFL, flags | O_NONBLOCK);
	if (ret == -1) {
		PRINT_MSG("Failed to set client socket to non-blocking mode: errno=%d, %s\n", 
			errno, strerror(errno));
		return JTAG_SERVER_ERROR;
	}

	// Set TCP_NODELAY for the client socket. This prevents delaying of outgoing data
	// (Nagle's algorithm disabled) which improves the performance of jtag_vpi.
	flags = 1;
	ret = setsockopt(connfd, IPPROTO_TCP, TCP_NODELAY, (void *)&flags, sizeof(flags));
	if (ret == -1) {
		PRINT_MSG("Failed to set TCP_NODELAY: errno=%d, %s\n", 
			errno, strerror(errno));
		return JTAG_SERVER_ERROR;
	}

	return JTAG_SERVER_SUCCESS;
}

/**
 * Check for an incoming jtag_vpi command from OpenOCD
 * (non-blocking check).
 */
int check_for_command(struct jtag_cmd *packet)
{
	int nb;
	int ret;
	if (connfd == -1) {
		// jtag_vpi server does not run, so start it now
		ret = jtag_server_create(JTAG_VPI_DEFAULT_SERVER_PORT, 
			JTAG_VPI_IS_LOOPBACK_ONLY);
		if (ret != JTAG_SERVER_SUCCESS) {
			return ret;	
		}
		ret = jtag_server_wait_for_client();
		if (ret != JTAG_SERVER_SUCCESS) {
			return ret;
		}
	}
	// See if there is incoming data from OpenOCD
	unsigned bytes_to_receive = sizeof(struct jtag_cmd) - pkt_buf_bytes;
	nb = read(connfd, pkt_buf + pkt_buf_bytes, bytes_to_receive);
	if (nb < 0) {
		if (errno == EAGAIN || errno == EWOULDBLOCK) {
			// No data received at this time
			return JTAG_SERVER_TRY_LATER;
		}
		else {
			// An error when working with the socket
			PRINT_MSG("Failed to receive data from OpenOCD: errno=%d, %s\n", 
				errno, strerror(errno));
			return JTAG_SERVER_ERROR;
		}
	}
	else if (nb == 0) {
		// Connection closed by the other side (OpenOCD disconnected)
		PRINT_MSG("jtag_vpi client has disconnected.\n");

		// Close at our end as well.
		jtag_server_finish();

		return JTAG_SERVER_CLIENT_DISCONNECTED;
	}
	else {
		// Some data arrived (nb > 0)
		pkt_buf_bytes += nb;
		if (pkt_buf_bytes < sizeof(struct jtag_cmd)) {
			// Not yet a whole packet, wait for the rest
			return JTAG_SERVER_TRY_LATER;
		}

		// We have a full jtag_vpi packet
		pkt_buf_bytes = 0;
		memcpy(packet, pkt_buf, sizeof(struct jtag_cmd));

		// Handle endianness of received data
		packet->cmd = from_little_endian_u32(packet->cmd);
		packet->length = from_little_endian_u32(packet->length);
		packet->nb_bits = from_little_endian_u32(packet->nb_bits);

		return JTAG_SERVER_SUCCESS;
	}
}

/**
 * Send response packet back to OpenOCD.
 */
int send_result_to_server(struct jtag_cmd *packet)
{
	ssize_t n;

	// Handle endianness of the data being sent
	packet->cmd = to_little_endian_u32(packet->cmd);
	packet->length = to_little_endian_u32(packet->length);
	packet->nb_bits = to_little_endian_u32(packet->nb_bits);

	// Send the packet to OpenOCD
	n = write(connfd, packet, sizeof(struct jtag_cmd));
	if (n < (ssize_t)sizeof(struct jtag_cmd)) {
		// Failed to write to socket. Cannot recover from this.
		PRINT_MSG("Failed to send data to OpenOCD: errno=%d, %s\n", errno, strerror(errno));
		return JTAG_SERVER_ERROR;
	}
	return JTAG_SERVER_SUCCESS;
}

/**
 * Close server sockets
 */
void jtag_server_finish(void)
{
	if (connfd != -1) {
		close(connfd);
		connfd = -1;
	}
	if (listenfd != -1) {
		close(listenfd);
		listenfd = -1;
	}
}

/**
 * Helper function. Determine if this machine is little endian.
 */
static int is_host_little_endian(void)
{
	return (htonl(25) != 25);
}

/**
 * Helper function. Convert u32 from little endian to host endianness.
 */
static uint32_t from_little_endian_u32(uint32_t val)
{
	return is_host_little_endian() ? val : htonl(val);
}

/**
 * Helper function. Convert u32 from host endianness to little endian.
 */
static uint32_t to_little_endian_u32(uint32_t val)
{
	return from_little_endian_u32(val);
}
