# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.12 2002/06/24 14:40:03 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS		= include

ALL_DIRS	= Network
PACKAGE		= network
PACKAGE_DEPS	= base

SRC_HC_OPTS	+= -Iinclude
SRC_CC_OPTS     += -Iinclude -I. -I$(GHC_INCLUDE_DIR)
SRC_HSC2HS_OPTS += -Iinclude

# Only bother with cbits/initWinSock.c when it's really needed.
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
EXTRA_SRCS=cbits/initWinSock.c
else
EXTRA_SRCS=cbits/ancilData.c
endif

SRC_HADDOCK_OPTS += -t "Haskell Core Libraries (network package)"

include $(TOP)/mk/target.mk

