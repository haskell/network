# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2005/03/02 16:39:58 ross Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

H_FILES = $(wildcard *.h)

includedir = $(libdir)/include

# Typeable.h doesn't need to be installed, and in fact it'll clobber the
# Typeable.h from package base (which arguably doesn't need to be installed
# either, but apparently some people use it...).  See bug #1106.  The Cabal
# description for network already does the right thing here.
INSTALL_INCLUDES = $(filter-out Typeable.h,$(H_FILES))

DIST_CLEAN_FILES += HsNetworkConfig.h

include $(TOP)/mk/target.mk
