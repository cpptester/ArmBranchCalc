#ifndef __ARM_INSTR_H__
#define __ARM_INSTR_H__

#include <stdint.h>

typedef uint32_t    ADRX;

struct arm_instr
{
    uint8_t link;		/* link付きブランチ命令か */
    uint8_t indirect;		/* 間接ブランチか */
    uint8_t change_state;	/* 命令ステートが変更される ARM<->Thumb */
    uint8_t conditional;
    uint8_t thumb_it_conditions;
    uint32_t context[16];
};

ADRX smon_next_branch_addr(ADRX addr, uint32_t opcode, int is_thumb, struct arm_instr *insn);

#endif // __ARM_INSTR_H__
