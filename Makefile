# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2002/02/07 11:13:31 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS  = include

ALL_DIRS = Network
PKG      = network

SRC_CC_OPTS  += -Iinclude -I$(GHC_INCLUDE_DIR)

include $(TOP)/mk/target.mk
