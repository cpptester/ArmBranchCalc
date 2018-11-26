#ifndef __BITOPS_H__
#define __BITOPS_H__

#include <stdint.h>

// 32-bit版 startで指定したビットからlengthビットを抽出する
static __inline uint32_t extract32(uint32_t value, int start, int length)
{
    return (value >> start) & (~0U >> (32 - length));
}

// 64-bit版 startで指定したビットからlengthビットを抽出する
static __inline uint64_t extract64(uint64_t value, int start, int length)
{
    return (value >> start) & (~0ULL >> (64 - length));
}

#endif
