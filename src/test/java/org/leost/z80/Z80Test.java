package org.leost.z80;

import org.junit.Test;

import static org.junit.Assert.*;



public class Z80Test {

    @Test
    public void testLd() {

        // LD A, (IX + d)

        Z80 cpu = new Z80();

        cpu.reg_PC = 0x0000;
        cpu.reg_IX = 0x1100;
        cpu.loadRam(0x0000, 0xFD, 0x7E, 0x08);
        cpu.loadRam(0x1108, 0xCC);

        cpu.executeOp();

        assertEquals(0xCC, cpu.reg_A);

        cpu.reg_PC = 0x0000;
        cpu.reg_IX = 0x1100;
        cpu.loadRam(0x0000, 0xFD, 0x7E, -0x08);
        cpu.loadRam(0x10F8, 0xDD);

        cpu.executeOp();

        assertEquals(0xDD, cpu.reg_A);
    }
}
