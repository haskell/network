# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.10 2002/05/01 17:52:53 sof Exp $

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
EXTRA_SRCS=cbits/initWinSock.c
else
EXTRA_SRCS=cbits/ancilData.c
endif

include $(TOP)/mk/target.mk

