# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.9 2002/05/01 05:42:42 sof Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS		= include

ALL_DIRS	= Network
PACKAGE		= network

SRC_HC_OPTS	+= -Iinclude
SRC_CC_OPTS     += -Iinclude -I. -I$(GHC_INCLUDE_DIR)
SRC_HSC2HS_OPTS += -Iinclude

# Only bother with cbits/initWinSock.c when it's really needed.
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
C_SRCS=cbits/initWinSock.c
STUBOBJS=cbits/initWinSock.o
else
C_SRCS=cbits/ancilData.c
STUBOBJS+=cbits/ancilData.o
endif

include $(TOP)/mk/target.mk

