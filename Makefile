# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 2002/02/13 12:04:00 simonmar Exp $

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

# URI relies on Text.Regex at the moment
ifneq "$(HavePosixRegex)" "YES"
EXCLUDED_SRCS += Network/URI.hs
endif

include $(TOP)/mk/target.mk

