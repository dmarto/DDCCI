/*
 * Copyright (c) 2018, Martin Dimov <martin <at> dmarto <dot> com>
 *    Licensed under the BSD-2-Clause "Simplified BSD License".
 */

#include <sys/ioctl.h>
#include <linux/i2c.h>
#include <linux/i2c-dev.h>

#include "errno.h"
#include "stdio.h"

//	struct i2c_msg {
//		__u16 addr;	/* slave address			*/
//		__u16 flags;
//	#define I2C_M_RD		0x0001	/* read data, from slave to master */
//						/* I2C_M_RD is guaranteed to be 0x0001! */
//	#define I2C_M_TEN		0x0010	/* this is a ten bit chip address */
//	#define I2C_M_RECV_LEN		0x0400	/* length will be first received byte */
//	#define I2C_M_NO_RD_ACK		0x0800	/* if I2C_FUNC_PROTOCOL_MANGLING */
//	#define I2C_M_IGNORE_NAK	0x1000	/* if I2C_FUNC_PROTOCOL_MANGLING */
//	#define I2C_M_REV_DIR_ADDR	0x2000	/* if I2C_FUNC_PROTOCOL_MANGLING */
//	#define I2C_M_NOSTART		0x4000	/* if I2C_FUNC_NOSTART */
//	#define I2C_M_STOP		0x8000	/* if I2C_FUNC_PROTOCOL_MANGLING */
//		__u16 len;		/* msg length				*/
//		__u8 *buf;		/* pointer to msg data			*/
//	};

int i2c(
	int fd,
	unsigned int addr,
	unsigned int isWR,
	unsigned char *buf,
	unsigned int len
) {

	struct i2c_msg msg;
	struct i2c_rdwr_ioctl_data msgset;

	msg.addr    = addr;
	msg.flags   = isWR == 1 ? 0 : I2C_M_RD;
	msg.buf     = buf;
	msg.len     = len;

	msgset.nmsgs = 1;
	msgset.msgs = &msg;

	int e = ioctl(fd, I2C_RDWR, &msgset);
	if (e < 0) {
		fprintf(stderr, "ioctl: r/%d errno/%d", e, errno);
		return -1;
	}
}

int i2c_read(
	int fd,
	unsigned int addr,
	unsigned char *buf,
	unsigned int len
) {
	return i2c(fd, addr, 0, buf, len);
}

int i2c_write(
	int fd,
	unsigned int addr,
	unsigned char *buf,
	unsigned int len
) {
	return i2c(fd, addr, 1, buf, len);
}
