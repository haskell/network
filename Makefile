# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.7 2002/02/14 14:09:47 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS		= include

ALL_DIRS	= Network
PACKAGE		= network

SRC_HC_OPTS	+= -Iinclude
SRC_CC_OPTS     += -Iinclude -I$(GHC_INCLUDE_DIR)
SRC_HSC2HS_OPTS += -Iinclude

# Only bother with cbits/initWinSock.c when it's really needed.
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
C_SRCS=cbits/initWinSock.c
endif

include $(TOP)/mk/target.mk

