# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.16 2003/01/13 14:12:23 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS		= include

ALL_DIRS	= Network
PACKAGE		= network
PACKAGE_DEPS	= base

SRC_HC_OPTS	+= -Iinclude -\#include HsNet.h
SRC_CC_OPTS     += -Iinclude -I. -I$(GHC_INCLUDE_DIR)
SRC_HSC2HS_OPTS += -Iinclude

Network/Socket_HC_OPTS += -cpp

EXTRA_SRCS += cbits/HsNet.c

# Only bother with cbits/initWinSock.c when it's really needed.
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
EXTRA_SRCS += cbits/initWinSock.c
Network/Socket_HC_OPTS += -DCALLCONV=stdcall
else
EXTRA_SRCS += cbits/ancilData.c
Network/Socket_HC_OPTS += -DCALLCONV=ccall
endif

SRC_HADDOCK_OPTS += -t "Haskell Core Libraries (network package)" \
	-p prologue.txt

include $(TOP)/mk/target.mk

