# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2002/02/06 15:40:42 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS  = include

ALL_DIRS = Network
PKG      = net

SRC_CC_OPTS  += -Iinclude -I$(GHC_INCLUDE_DIR)

include $(TOP)/mk/target.mk
