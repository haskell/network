# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.1 2001/08/01 13:33:26 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS  = include

ALL_DIRS = Network
PKG      = net

include $(TOP)/mk/target.mk
