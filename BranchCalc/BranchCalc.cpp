// BranchCalc.cpp : コンソール アプリケーションのエントリ ポイントを定義します。
//

#include "stdafx.h"
#include <stdint.h>
#include "arm_instr.h"
#include "bitops.h"

#define _TEST

#ifdef _TEST
static uint32_t read_mem(uint32_t addr)
{
    if (addr > 0x20880030 && addr <= 0x20880040) {
	return addr - 0x30;
    }
    if (addr == 0x20880050) {
	return 0x20880000;
    }
    else if (addr == 0x20880058) {
	return 0x20880004;
    }
    return 0;
}
#else
static uint32_t read_mem(uint32_t addr)
{
    return peekl_usr(addr);
}
#endif

static int inst_ARM_branch_destination(uint32_t addr, uint32_t inst, uint32_t *pnpc)
{
    uint32_t npc;
    int is_direct_branch = 1;
    if ((inst & 0x0e000000) == 0x0a000000) {
	/*
	B:   cccc:1010:imm24
	BL:  cccc:1011:imm24
	BLX: 1111:101H:imm24
	*/
	npc = addr + 8 + ((int32_t)((inst & 0xffffff) << 8) >> 6);
	if ((inst & 0xf0000000) == 0xf0000000) {
	    npc |= 1;  /* indicate ISA is now Thumb */
	    npc |= ((inst >> 23) & 2);   /* apply the H bit */
	}
    }
    else {
	is_direct_branch = 0;
    }
    if (is_direct_branch && pnpc != NULL) {
	*pnpc = npc;
    }
    return is_direct_branch;
}

static int inst_ARM_is_branch_and_link(uint32_t inst)
{
    int is_branch = 1;
    if ((inst & 0xf0000000) == 0xf0000000) {
	if ((inst & 0xfe000000) == 0xfa000000) {
	    /* BLX (imm) */
	}
	else {
	    is_branch = 0;
	}
    }
    else if ((inst & 0x0f000000) == 0x0b000000) {
	/* BL */
    }
    else if ((inst & 0x0ff000f0) == 0x01200030) {
	/* BLX (reg) */
    }
    else {
	is_branch = 0;
    }
    return is_branch;
}

static int inst_ARM_is_direct_branch(uint32_t inst)
{
    int is_direct_branch = 1;
    if ((inst & 0xf0000000) == 0xf0000000) {
	/* NV space */
	if ((inst & 0xfe000000) == 0xfa000000) {
	    /* BLX (imm) */
	}
	else {
	    is_direct_branch = 0;
	}
    }
    else if ((inst & 0x0e000000) == 0x0a000000) {
	/* B, BL */
    }
    else {
	is_direct_branch = 0;
    }
    return is_direct_branch;
}

static uint32_t decode_imm_shift(uint32_t type, uint32_t value, uint32_t imm)
{
    if (type == 0) {
	/* lsl */
	return value << imm;
    }
    return value >> imm;
}

static int inst_ARM_is_indirect_branch(uint32_t inst, uint32_t *npc, struct arm_instr *insn)
{
    int is_indirect_branch = 1;
    uint32_t addr;
    int Rn, Rm, Rs, imm;
    if ((inst & 0xf0000000) == 0xf0000000) {
	/* NV space */
	if ((inst & 0xfe500000) == 0xf8100000) {
	    /* RFE */
	    Rn = extract32(inst, 16, 4);
	    *npc = insn->context[Rn];
	    if (inst & 0x00800000) {
		/* increment = 1 */
		if (inst & 0x1000000) {
		    /* Pre */
		    *npc += 4;
		}
	    }
	    else {
		if (inst & 0x1000000) {
		    *npc -= 8;
		}
		else {
		    *npc -= 4;
		}
	    }
	}
	else {
	    is_indirect_branch = 0;
	}
    }
    else if ((inst & 0x0fe0f010) == 0x0080f000) {
	uint32_t type;
	/* ADD pc, reg, reg{, <shift>} */
	Rn = extract32(inst, 16, 4);
	Rm = extract32(inst, 0, 4);
	imm = extract32(inst, 7, 5);
	type = extract32(inst, 5, 2);
	*npc = insn->context[Rn] + decode_imm_shift(type, insn->context[Rm], imm);
    }
    else if ((inst & 0x0ff000d0) == 0x01200010) {
	/* BLX (register) */
	if ((inst & 0x000fff30) == 0x000fff30) {
	    insn->link = 1;
	}
	/* BX (register) */
	Rn = extract32(inst, 0, 4);
	*npc = insn->context[Rn];
    }
    else if ((inst & 0x0ff000f0) == 0x01200020) {
	/* BXJ: in v8 this behaves like BX */
    }
    else if ((inst & 0x0e108000) == 0x08108000) {
	/* POP {...,pc} or LDMxx {...,pc} */
	if ((inst & 0x0FFFA000) == 0x08BD8000) {
	    /* LDMIA SP!,{...,pc} */
	}
	addr = insn->context[13];
	if (inst & 0x800000) {
	    /* Inc. */
	    int i, cnt = -1;
	    uint32_t mask = 1;
	    for (i = 0; i < 16; ++i) {
		if (inst & mask)
		    ++cnt;
		mask <<= 1;
	    }
	    if (inst & 0x1000000) {	/* Pre. */
		++cnt;
	    }
	    *npc = read_mem(addr + cnt * 4);
	}
	else {
	    /* Dec. */
	    if (inst & 0x1000000) {	/* Pre. */
		addr -= 4;
	    }
	    *npc = read_mem(addr);
	}
    }
    else if ((inst & 0x0e50f000) == 0x0410f000) {
	imm = extract32(inst, 0, 12);
	Rn = extract32(inst, 16, 4);
	/* LDR PC,imm... inc. POP {PC} */

	if (inst & (1 << 24)) {
	    if (inst & (1 << 23)) {
		*npc = read_mem(insn->context[Rn] + imm);
	    }
	    else {
		*npc = read_mem(insn->context[Rn] - imm);
	    }
	}
	else {
	    *npc = read_mem(insn->context[Rn]);
	}
    }
    else if ((inst & 0x0e50f010) == 0x0610f000) {
	/* LDR PC,reg */
	Rn = extract32(inst, 0, 4);
	*npc = read_mem(insn->context[Rn]);
    }
    else if ((inst & 0x0fe0f000) == 0x01a0f000) {
	/* MOV PC,rx */
	Rn = extract32(inst, 0, 4);
	*npc = insn->context[Rn];
    }
    else if ((inst & 0x0f900080) == 0x01000000) {
	/* "Miscellaneous instructions" - in DP space */
	is_indirect_branch = 0;
    }
    else if ((inst & 0x0f9000f0) == 0x01800090) {
	/* Some extended loads and stores */
	is_indirect_branch = 0;
    }
    else if ((inst & 0x0fb0f000) == 0x0320f000) {
	/* MSR #imm */
	is_indirect_branch = 0;
    }
    else if ((inst & 0x0e10f000) == 0x0210f000) {
	/* subs pc, lr, imm */
	imm = extract32(inst, 0, 12);
	*npc = insn->context[14] - imm;
    }
    else if ((inst & 0x0e00f000) == 0x0200f000) {
	/* DP PC,imm shift */
	if ((inst & 0x0f90f000) == 0x0310f000) {
	    /* TST/CMP */
	    is_indirect_branch = 0;
	}
    }
    else if ((inst & 0x0e00f000) == 0x0000f000) {
	/* DP PC,reg */
    }
    else {
	is_indirect_branch = 0;
    }
    return is_indirect_branch;
}

/*
For Thumb2, test if a halfword is the first half of a 32-bit instruction,
as opposed to a complete 16-bit instruction.
*/
static int is_wide_thumb(uint16_t insthw)
{
    return (insthw & 0xF800) >= 0xE800;
}

static int inst_Thumb_branch_destination(uint32_t addr, uint32_t inst, uint32_t *pnpc)
{
    uint32_t npc;
    int is_direct_branch = 1;
    if ((inst & 0xf0000000) == 0xd0000000 && (inst & 0x0e000000) != 0x0e000000) {
	/* B<c> (encoding T1) */
	npc = addr + 4 + ((int32_t)((inst & 0x00ff0000) << 8) >> 23);
	npc |= 1;
    }
    else if ((inst & 0xf8000000) == 0xe0000000) {
	/* B (encoding T2) */
	npc = addr + 4 + ((int32_t)((inst & 0x07ff0000) << 5) >> 20);
	npc |= 1;
    }
    else if ((inst & 0xf800d000) == 0xf0008000 && (inst & 0x03800000) != 0x03800000) {
	/* B (encoding T3) */
	npc = addr + 4 + ((int32_t)(((inst & 0x04000000) << 5) |
	    ((inst & 0x0800) << 19) |
	    ((inst & 0x2000) << 16) |
	    ((inst & 0x003f0000) << 7) |
	    ((inst & 0x000007ff) << 12)) >> 11);
	npc |= 1;
    }
    else if ((inst & 0xf8009000) == 0xf0009000) {
	/* B (encoding T4); BL (encoding T1) */
	uint32_t S = ((inst & 0x04000000) >> 26) - 1;  /* ffffffff or 0 according to S bit */
	npc = addr + 4 + ((int32_t)(((inst & 0x04000000) << 5) |
	    (((inst^S) & 0x2000) << 17) |
	    (((inst^S) & 0x0800) << 18) |
	    ((inst & 0x03ff0000) << 3) |
	    ((inst & 0x000007ff) << 8)) >> 7);
	npc |= 1;
    }
    else if ((inst & 0xf800d001) == 0xf000c000) {
	/* BLX (encoding T2) */
	uint32_t S = ((inst & 0x04000000) >> 26) - 1;  /* ffffffff or 0 according to S bit */
	addr &= 0xfffffffc;   /* Align(PC,4) */
	npc = addr + 4 + ((int32_t)(((inst & 0x04000000) << 5) |
	    (((inst^S) & 0x2000) << 17) |
	    (((inst^S) & 0x0800) << 18) |
	    ((inst & 0x03ff0000) << 3) |
	    ((inst & 0x000007fe) << 8)) >> 7);
	/* don't set the Thumb bit, as we're transferring to ARM */
    }
    else if ((inst & 0xf5000000) == 0xb1000000) {
	/* CB(NZ) */
	/* Note that it's zero-extended - always a forward branch */
	npc = addr + 4 + ((((inst & 0x02000000) << 6) |
	    ((inst & 0x00f80000) << 7)) >> 25);
	npc |= 1;
    }
    else {
	is_direct_branch = 0;
    }
    if (is_direct_branch && pnpc != NULL) {
	*pnpc = npc;
    }
    return is_direct_branch;
}

static int inst_Thumb_is_direct_branch_link(uint32_t inst, struct arm_instr *insn)
{
    int is_direct_branch = 1;

    if ((inst & 0xf0000000) == 0xd0000000 && (inst & 0x0e000000) != 0x0e000000) {
	/* B<c> (encoding T1) */
	insn->conditional = 1;
    }
    else if ((inst & 0xf8000000) == 0xe0000000) {
	/* B (encoding T2) */
    }
    else if ((inst & 0xf800d000) == 0xf0008000 && (inst & 0x03800000) != 0x03800000) {
	/* B (encoding T3) */
	insn->conditional = 1;
    }
    else if ((inst & 0xf8009000) == 0xf0009000) {
	/* B (encoding T4); BL (encoding T1) */
	if (inst & 0x00004000) {
	    insn->link = 1;
	}
    }
    else if ((inst & 0xf800d001) == 0xf000c000) {
	/* BLX (imm) (encoding T2) */
	insn->link = 1;
    }
    else if ((inst & 0xf5000000) == 0xb1000000) {
	/* CB(NZ) */
	insn->conditional = 1;
    }
    else {
	is_direct_branch = 0;
    }
    return is_direct_branch;
}

static int inst_Thumb_is_indirect_branch_link(uint32_t inst, uint32_t addr, uint32_t *npc, struct arm_instr *insn)
{
    /* See e.g. PFT Table 2-3 and Table 2-5 */
    int is_branch = 1;

    if ((inst & 0xff000000) == 0x47000000) {
	/* BX, BLX (reg) */
	if (inst & 0x00800000) {
	    insn->link = 1;
	}
	else if ((inst & 0x00780000) == 0x00700000) {
	    /* BX LR */
	}
    }
    else if ((inst & 0xfff0d000) == 0xf3c08000) {
	/* BXJ: in v8 this behaves like BX */
    }
    else if ((inst & 0xff000000) == 0xbd000000) {
	/* POP {pc} */
    }
    else if ((inst & 0xfd870000) == 0x44870000) {
	/* MOV PC,reg or ADD PC,reg */
	if ((inst & 0xffff0000) == 0x46f70000) {
	    /* MOV PC,LR */
	}
    }
    else if ((inst & 0xfff0ffe0) == 0xe8d0f000) {
	/* TBB/TBH */
    }
    else if ((inst & 0xffd00000) == 0xe8100000) {
	/* RFE (T1) */
    }
    else if ((inst & 0xffd00000) == 0xe9900000) {
	/* RFE (T2) */
    }
    else if ((inst & 0xfff0d000) == 0xf3d08000) {
	/* SUBS PC,LR,#imm inc.ERET */
    }
    else if ((inst & 0xfff0f000) == 0xf8d0f000) {
	/* LDR PC,imm (T3) */
    }
    else if ((inst & 0xff7ff000) == 0xf85ff000) {
	/* LDR PC,literal (T2) */
    }
    else if ((inst & 0xfff0f800) == 0xf850f800) {
	/* LDR PC,imm (T4) */
	if ((inst & 0x000f0f00) == 0x000d0b00) {
	    /* LDR PC, [SP], #imm*/
	}
    }
    else if ((inst & 0xfff0ffc0) == 0xf850f000) {
	/* LDR PC,reg (T2) */
    }
    else if ((inst & 0xfe508000) == 0xe8108000) {
	/* LDM PC */
	if ((inst & 0x0FFF0000) == 0x08BD0000) { /* LDMIA [SP]!, */
						 /* POP {...,pc} */
	}
    }
    else {
	is_branch = 0;
    }
    return is_branch;
}

static uint8_t inst_ARM_is_conditional(uint32_t inst)
{
    return (inst & 0xe0000000) != 0xe0000000;
}

static uint8_t inst_Thumb_is_conditional(uint32_t inst)
{
    if ((inst & 0xf0000000) == 0xd0000000 && (inst & 0x0e000000) != 0x0e000000) {
	/* B<c> (encoding T1) */
	return 1;
    }
    else if ((inst & 0xf800d000) == 0xf0008000 && (inst & 0x03800000) != 0x03800000) {
	/* B<c> (encoding T3) */
	return 1;
    }
    else if ((inst & 0xf5000000) == 0xb1000000) {
	/* CB(N)Z */
	return 1;
    }
    return 0;
}

static uint8_t inst_Thumb_is_IT(uint32_t inst)
{
    if ((inst & 0xff000000) == 0xbf000000 &&
	(inst & 0x000f0000) != 0x00000000) {
	if (inst & 0x00010000) {
	    return 4;
	}
	else if (inst & 0x00020000) {
	    return 3;
	}
	else if (inst & 0x00040000) {
	    return 2;
	}
	else {
	    return 1;
	}
    }
    else {
	return 0;
    }
}

uint32_t calc_next_branch_addr(uint32_t addr, uint32_t opcode, int is_thumb, struct arm_instr *insn)
{
    uint32_t branchAddr = addr;
    uint32_t instr_size = 4;

    insn->link = 0;
    insn->indirect = 0;
    insn->change_state = 0;
    insn->conditional = 0;
    insn->thumb_it_conditions = 0;

    if (is_thumb) {
	if (inst_Thumb_is_direct_branch_link(opcode, insn)) {
	    inst_Thumb_branch_destination(addr, opcode, &branchAddr);
	    if ((branchAddr & 0x1) == 0) {
		/* ターゲットアドレスがARM命令 */
		insn->change_state = 1;
	    }
	    else {
		branchAddr &= ~0x1;
	    }
	}
	else if (inst_Thumb_is_indirect_branch_link(opcode, addr, &branchAddr, insn)) {
	    if ((branchAddr & 0x1) == 0) {
		/* ターゲットアドレスがARM命令 */
		insn->change_state = 1;
	    }
	    else {
		branchAddr &= ~0x1;
	    }
	    insn->indirect = 1;
	}
	else {
	    branchAddr += is_wide_thumb((uint16_t)(opcode >> 16)) ? 4 : 2;
	}
	insn->conditional = inst_Thumb_is_conditional(opcode);
	insn->thumb_it_conditions = inst_Thumb_is_IT(opcode);
    }
    else {
	if (inst_ARM_is_indirect_branch(opcode, &branchAddr, insn)) {
	    insn->indirect = 1;
	    insn->link = inst_ARM_is_branch_and_link(opcode);
	    if (branchAddr & 1) {
		/* ターゲットアドレスがThumb命令 */
		branchAddr &= ~0x1;
		insn->change_state = 1;
	    }
	}
	else if (inst_ARM_is_direct_branch(opcode)) {
	    inst_ARM_branch_destination(addr, opcode, &branchAddr);
	    if (branchAddr & 1) {
		/* ターゲットアドレスがThumb命令 */
		branchAddr &= ~0x1;
		insn->change_state = 1;
	    }
	    insn->link = inst_ARM_is_branch_and_link(opcode);
	}
	else {
	    branchAddr += instr_size;
	}
	insn->conditional = inst_ARM_is_conditional(opcode);
    }
    return branchAddr;
}

#define CHECK_ARM(I, code, validaddr, pc) \
    I.context[15] = pc; \
    addr = calc_next_branch_addr(pc, code, 0, &instr); \
    if (addr != validaddr) {   \
        printf("Wrong address calculation, 0x%08x. valid addr is 0x%08x", addr, validaddr); \
    }

int main()
{
    uint32_t addr;
    struct arm_instr instr;
    instr.context[0] = 0x20880000;
    instr.context[1] = 0x8;
    instr.context[3] = 0x20880050;
    instr.context[4] = 0x20880050;
    instr.context[10] = 0x20880008;
    instr.context[13] = 0x20880030;
    instr.context[14] = 0x2088000C;
    instr.context[15] = 0x20880000;

    CHECK_ARM(instr, 0xE080F001, 0x20880008, 0);    /* add pc, r0, r1 */
    CHECK_ARM(instr, 0xE080F101, 0x20880020, 0);    /* add pc, r0, r1, lsl #1 */
    CHECK_ARM(instr, 0xE12FFF1A, 0x20880008, 0);    /* bx r10 */
    CHECK_ARM(instr, 0xE12FFF1E, 0x2088000C, 0);    /* bx lr */
    CHECK_ARM(instr, 0xE25EF004, 0x20880008, 0);    /* subs pc, lr, #4 */
    CHECK_ARM(instr, 0xE593F000, 0x20880000, 0);    /* ldr pc, [r3] */
    CHECK_ARM(instr, 0xE494F008, 0x20880000, 0);    /* ldr pc, [r4], #8 */
    CHECK_ARM(instr, 0xE594F008, 0x20880004, 0);    /* ldr pc, [r4, #8] */
    CHECK_ARM(instr, 0xE89D800F, 0x20880010, 0);    /* ldmia sp, {r0-r3, pc} */

						    //CHECK(instr, 0xEB000F51, 0x20880008)    /* add pc, r0, r1, lsr #1 */

    return 0;
}

