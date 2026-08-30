#include <netinet/in.h>

static uint32_t my_htonl(uint32_t x) { return htonl(x); }
static uint16_t my_htons(uint16_t x) { return htons(x); }
static uint32_t my_ntohl(uint32_t x) { return ntohl(x); }
static uint32_t my_ntohs(uint16_t x) { return ntohs(x); }

#undef htonl
#undef htons
#undef ntohl
#undef ntohs

uint32_t htonl(uint32_t x) { return my_htonl(x); }
uint32_t htons(uint16_t x) { return my_htons(x); }
uint32_t ntohl(uint32_t x) { return my_ntohl(x); }
uint32_t ntohs(uint16_t x) { return my_ntohs(x); }
