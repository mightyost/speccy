package org.leost.z80;

import org.junit.Test;
import org.leost.z80.test.TestMemory;
import org.leost.z80.test.TestUla;

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

    @Test
    public void testRlca() {

        Z80 cpu = new Z80();
        cpu.init(new TestMemory(), new TestUla());

        cpu.reg_PC = 0x0000;
        cpu.loadRam(0x0000, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07);

        cpu.reg_F.C = cpu.reg_F.H = cpu.reg_F.N = cpu.reg_F.P = cpu.reg_F.S = cpu.reg_F.X = cpu.reg_F.Y = cpu.reg_F.Z = true;
        cpu.reg_A = 0b10101010; // 0xAA

        cpu.executeOp();

        assertEquals(0b01010101, cpu.reg_A);
        assertEquals(true, cpu.reg_F.C);
        assertEquals(true, cpu.reg_F.S);
        assertEquals(true, cpu.reg_F.Z);
        assertEquals(false, cpu.reg_F.H);
        assertEquals(true, cpu.reg_F.P);
        assertEquals(false, cpu.reg_F.N);

        cpu.executeOp();

        assertEquals(0b10101010, cpu.reg_A);
        assertEquals(false, cpu.reg_F.C);

        cpu.reg_A = 0b11111111; // 0xFF

        cpu.executeOp();

        assertEquals(0b11111111, cpu.reg_A);
        assertEquals(true, cpu.reg_F.C);

        cpu.executeOp();

        assertEquals(0b11111111, cpu.reg_A);
        assertEquals(true, cpu.reg_F.C);
    }

    @Test
    public void testRla() {

        Z80 cpu = new Z80();
        cpu.init(new TestMemory(), new TestUla());

        cpu.reg_PC = 0x0000;
        cpu.loadRam(0x0000, 0x17, 0x17, 0x17, 0x17, 0x17, 0x17, 0x17, 0x17);

        cpu.reg_F.C = cpu.reg_F.H = cpu.reg_F.N = cpu.reg_F.P = cpu.reg_F.S = cpu.reg_F.X = cpu.reg_F.Y = cpu.reg_F.Z = true;
        cpu.reg_A = 0b10101010; // 0xAA

        cpu.executeOp();

        assertEquals(0b01010101, cpu.reg_A);
        assertEquals(true, cpu.reg_F.C);
        assertEquals(true, cpu.reg_F.S);
        assertEquals(true, cpu.reg_F.Z);
        assertEquals(false, cpu.reg_F.H);
        assertEquals(true, cpu.reg_F.P);
        assertEquals(false, cpu.reg_F.N);

        cpu.executeOp();

        assertEquals(0b10101011, cpu.reg_A);
        assertEquals(false, cpu.reg_F.C);

        cpu.reg_A = 0b11111111; // 0xFF

        cpu.executeOp();

        assertEquals(0b11111110, cpu.reg_A);
        assertEquals(true, cpu.reg_F.C);

        cpu.executeOp();

        assertEquals(0b11111101, cpu.reg_A);
        assertEquals(true, cpu.reg_F.C);
    }

    @Test
    public void djnz() {

        Z80 cpu = new Z80();
        cpu.init(new TestMemory(), new TestUla());

        cpu.reg_B = 0x01;
        cpu.reg_PC = 0x0000;
        cpu.loadRam(0x0000, 0x10, 0x10);

        cpu.executeOp();

        assertEquals(0x00, cpu.reg_B);
        assertEquals(0x0002, cpu.reg_PC);

        cpu.reg_B = 0x02;
        cpu.reg_PC = 0x0000;
        cpu.loadRam(0x0000, 0x10, 0x10);

        cpu.executeOp();

        assertEquals(0x01, cpu.reg_B);
        assertEquals(0x0012, cpu.reg_PC);

        cpu.reg_B = 0x02;
        cpu.reg_PC = 0x1010;
        cpu.loadRam(0x1010, 0x10, 0xF0);

        cpu.executeOp();

        assertEquals(0x01, cpu.reg_B);
        assertEquals(0x1002, cpu.reg_PC);
    }

    @Test
    public void jr() {

        Z80 cpu = new Z80();
        cpu.init(new TestMemory(), new TestUla());

        cpu.reg_PC = 0x0000;
        cpu.loadRam(0x0000, 0x18, 0x10);

        cpu.executeOp();

        assertEquals(0x0012, cpu.reg_PC);

        cpu.reg_PC = 0x1010;
        cpu.loadRam(0x1010, 0x18, 0xF0);

        cpu.executeOp();

        assertEquals(0x1002, cpu.reg_PC);
    }

}
