## Unreleased version

 * Added a Read instance for PortNumber. [#145](https://github.com/haskell/network/pull/145)

 * We only set the IPV6_V6ONLY flag to 0 for stream and datagram socket types,
   as opposed to all of them. This makes it possible to use ICMPv6.

 * Socket errors no longer cause segfaults or hangs on Windows.

 * Various documentation improvements.

 * Various internal improvements.

## Version 2.6.2.1

 * Regenerate configure and HsNetworkConfig.h.in.

 * Better detection of CAN sockets.

## Version 2.6.2.0

 * Add support for TCP_USER_TIMEOUT.

 * Don't conditionally export the SockAddr constructors.

 * Add isSupportSockAddr to allow checking for supported address types
   at runtime.
