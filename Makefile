# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.5 2002/02/12 10:52:47 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS  = include

ALL_DIRS = Network
PACKAGE      = network

SRC_CC_OPTS  += -Iinclude -I$(GHC_INCLUDE_DIR)

#
# Only bother with cbits/initWinSock.c when it's really needed.
# 
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
C_SRCS=cbits/initWinSock.c
endif

include $(TOP)/mk/target.mk

