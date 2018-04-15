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
    public void testRra() {

        Z80 cpu = new Z80();
        cpu.init(new TestMemory(), new TestUla());

        cpu.reg_PC = 0x0000;
        cpu.loadRam(0x0000, 0x1F, 0x1F, 0x1F, 0x1F, 0x1F);

        cpu.reg_F.C = cpu.reg_F.H = cpu.reg_F.N = cpu.reg_F.P = cpu.reg_F.S = cpu.reg_F.X = cpu.reg_F.Y = cpu.reg_F.Z = true;
        cpu.reg_A = 0b10101010; // 0xAA

        cpu.executeOp();

        assertEquals(0b11010101, cpu.reg_A);
        assertEquals(false, cpu.reg_F.C);
        assertEquals(true, cpu.reg_F.S);
        assertEquals(true, cpu.reg_F.Z);
        assertEquals(false, cpu.reg_F.H);
        assertEquals(true, cpu.reg_F.P);
        assertEquals(false, cpu.reg_F.N);

        cpu.executeOp();

        assertEquals(0b01101010, cpu.reg_A);
        assertEquals(true, cpu.reg_F.C);

        cpu.executeOp();

        assertEquals(0b10110101, cpu.reg_A);
        assertEquals(false, cpu.reg_F.C);

        cpu.reg_A = 0b11111111; // 0xFF

        cpu.executeOp();

        assertEquals(0b01111111, cpu.reg_A);
        assertEquals(true, cpu.reg_F.C);

        cpu.executeOp();

        assertEquals(0b10111111, cpu.reg_A);
        assertEquals(true, cpu.reg_F.C);
    }

    @Test
    public void testDaa() {

        /*
        Depending on the NF flag, the ‘diff’ from this table must be added (NF is reset) or subtracted (NF is set) to A.

          CF | high   | HF | low    | diff
             | nibble |    | nibble |
        +----+--------+----+--------+--------
           0    0-9      0     0-9      00
           0    0-9      1     0-9      06
           0    0-8      *     a-f      06
           0    a-f      0     0-9      60
           1      *      0     0-9      60
           1      *      1     0-9      66
           1      *      *     a-f      66
           0    9-f      *     a-f      66
           0    a-f      1     0-9      66


        The CF flag is affected as follows:

          CF   high     low      CF’
               nibble   nibble
        +----+--------+--------+----+
          0     0-9      0-9     0
          0     0-8      a-f     0
          0     9-f      a-f     1
          0     a-f      0-9     1
          1       *        *     1

        The NF flags is affected as follows:

          NF   HF   low     HF’
                    nibble
        +----+----+--------+----+
           0    *    0-9     0
           0    *    a-f     1
           1    0      *     0
           1    1    6-f     0
           1    1    0-5     1

        SF, YF, XF are copies of bit 7,5,3 of the result respectively; ZF is set according to the result and NF is always
        unchanged.
         */
        Z80 cpu = new Z80();
        cpu.init(new TestMemory(), new TestUla());

        cpu.reg_PC = 0x0000;
        cpu.loadRam(0x0000, 0x27);

        cpu.reg_F.C = cpu.reg_F.H = cpu.reg_F.N = cpu.reg_F.P = cpu.reg_F.S = cpu.reg_F.X = cpu.reg_F.Y = cpu.reg_F.Z = true;

        for (int n = 0; n < 2; n++) {                       // N flag
            for (int c = 0; c < 2; c++) {                   // C flag
                for (int h = 0; h < 2; h++) {               // H flag
                    for (int a = 0; a <= 0xff; a++) {       // A reg
                        cpu.reg_F.N = n == 1;
                        cpu.reg_F.C = c == 1;
                        cpu.reg_F.H = h == 1;
                        cpu.reg_A = a;

                        int high = (a & 0xF0) >> 4;
                        int low  = (a & 0x0F);

                        int expCorr = 0;
                        if (c == 0 && h == 0 && high <= 9 && low <= 9) {
                            expCorr = 0;
                        } else
                        if (c == 0 && h == 1 && high <= 9 && low <= 9 ||
                            c == 0           && high <= 8 && low >= 10) {
                            expCorr = 0x06;
                        } else
                        if (c == 0 && h == 0 && high >= 10 && low <= 9 ||
                            c == 1 && h == 0               && low <= 9) {
                            expCorr = 0x60;
                        } else
                        if (c == 1 && h == 1               && low <= 9 ||
                            c == 1                         && low >= 10 ||
                            c == 0           && high >= 9  && low >= 10 ||
                            c == 0 && h == 1 && high >= 10 && low <= 9) {
                            expCorr = 0x66;
                        } else {
                            throw new RuntimeException("Undefined: " + n + ", " + c + ", " + h + ", " + a);
                        }

                        cpu.reg_PC = 0x0000;
                        cpu.executeOp();

                        int expA = n == 0 ? a + expCorr : a - expCorr;
                        expA &= 0xFF;

                        boolean expC =
                                (c == 0 && high4(expA) >= 9 && low4(expA) > 9) ||
                                (c == 0 && high4(expA) > 9 && low4(expA) <= 9) ||
                                (c == 1);

                        boolean expH =
                                (n == 0 && low4(expA) > 9) ||
                                (n == 1 && h == 1 && low4(expA) <= 5);

                        System.out.println("Fixture: N=" + n + ", C=" + c + ", H=" + h + ", A=" + a + " (" + hex8(a) + "), corr=" + hex8(expCorr) + ", expA=" + hex8(expA) + ", expC=" + expC + ", expH=" + expH);
                        System.out.println("Result:  N=" + cpu.reg_F.N + ", C=" + cpu.reg_F.C + ", H=" + cpu.reg_F.H + ", A=" + cpu.reg_A + " (" + hex8(cpu.reg_A) + ")");
                        assertEquals(hex8(expA), hex8(cpu.reg_A));
                        assertEquals(expC, cpu.reg_F.C);
                        assertEquals(expH, cpu.reg_F.H);
                    }
                }
            }
        }
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


    private String hex8(int val) {
        return String.format("0x%02X", val);
    }

    private String hex16(int val) {
        return String.format("0x%04X", val);
    }

    private int low4(int val8) {
        return val8 & 0x0F;
    }

    private int high4(int val8) {
        return (val8 >> 4) & 0x0F ;
    }

}
