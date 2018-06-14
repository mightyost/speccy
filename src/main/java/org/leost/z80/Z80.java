package org.leost.z80;

/**
 */
public class Z80 {

    protected class Flags {
        boolean S, Z, Y, H, X, P, N, C;

        int value() {
            return
                toInt(S) << 7 |
                toInt(Z) << 6 |
                toInt(Y) << 5 |
                toInt(H) << 4 |
                toInt(X) << 3 |
                toInt(P) << 2 |
                toInt(N) << 1 |
                toInt(C) << 0;
        }

        void value(int val8) {
            S = bit(val8, 7) > 0;
            Z = bit(val8, 6) > 0;
            Y = bit(val8, 5) > 0;
            H = bit(val8, 4) > 0;
            X = bit(val8, 3) > 0;
            P = bit(val8, 2) > 0;
            N = bit(val8, 1) > 0;
            C = bit(val8, 0) > 0;
        }
    }

    protected int reg_A, reg_B, reg_C, reg_D, reg_E, reg_H, reg_L;  // Main register set
    protected int reg_a, reg_b, reg_c, reg_d, reg_e, reg_h, reg_l;  // Alternate register set

    protected Flags reg_F = new Flags(); // Flags
    protected Flags reg_f = new Flags(); // Flags

    protected int reg_I;          // Interrupt Page Address Register
    protected int reg_R;          // Memory Refresh Register
    protected int reg_IX, reg_IY; // Index Register
    protected int reg_SP;         // Stack Pointer
    protected int reg_PC;         // Program pointer

    protected int opAddr;         // Program pointer for last op code

    protected int IM = 0;         // Interrupt mode
    protected boolean IFF1, IFF2; // The two interrupt flip-flops

    private Memory mem;
    private IO io;

    private boolean isHalted = false;

    public void init(Memory mem, IO io) {
        this.mem = mem;
        this.io = io;


        reset();
    }

    public void reset() {
        reg_PC = 0;
        reg_SP = 0xFFFF;
        IFF1 = false;
        IFF2 = false;
        reg_A = reg_B = reg_C = reg_D = reg_E = reg_H = reg_L = 0xFF;
        reg_a = reg_b = reg_c = reg_d = reg_e = reg_h = reg_l = 0xFF;
        reg_F.value(0xFF);
        IM = 0;
    }

    private int BC() {
        return to16(reg_B, reg_C);
    }

    private int DE() {
        return to16(reg_D, reg_E);
    }

    private int HL() {
        return to16(reg_H, reg_L);
    }

    private int AF() {
        return to16(reg_A, reg_F.value());
    }

    private void BC(int val16) {
        reg_B = high8(val16);
        reg_C = low8(val16);
    }

    private void DE(int val16) {
        reg_D = high8(val16);
        reg_E = low8(val16);
    }

    private void HL(int val16) {
        reg_H = high8(val16);
        reg_L = low8(val16);
    }

    private void AF(int val16) {
        reg_A = high8(val16);
        reg_F.value(low8(val16));
    }

    private int to16(int h8, int l8) {
        return (h8 & 0xFF) << 8 | (l8 & 0xFF);
    }

    private int low8(int val16) {
        return val16 & 0xFF;
    }

    private int high8(int val16) {
        return (val16 >> 8) & 0xFF ;
    }

    private int low4(int val8) {
        return val8 & 0x0F;
    }

    private int high4(int val8) {
        return (val8 >> 4) & 0x0F ;
    }

    private int readOp() {
        if (isHalted) {
            return 0x00; // NOP
        }

        opAddr = reg_PC;
        return mem.read8(reg_PC++);
    }

    private int readNn() {
        int l = mem.read8(reg_PC++);
        int h = mem.read8(reg_PC++);
        return to16(h, l);
    }

    private int readN() {
        return mem.read8(reg_PC++);
    }

    public void run() {
        while (true) {
            executeOp();
        }
    }


    protected void executeOp() {
        int n, nn, h, l;
        Flags f;

        int code = readOp();

        switch (code) {

            case 0x00:  // NOP
                logOp("NOP");
                break;

            case 0x01:  // LD BC, nn
                nn = readNn();
                BC(nn);
                logOp("LD BC, %s", hex16(nn));
                break;

            case 0x02:  // LD (BC), A
                mem.write8(BC(), reg_A);
                logOp("LD (BC), A");
                break;

            case 0x03:  // INC BC
                BC(inc16(BC()));
                logOp("INC BC");
                break;

            case 0x04:  // INC B
                reg_B = inc8(reg_B);
                logOp("INC B");
                break;

            case 0x05:  // DEC B
                reg_B = dec8(reg_B);
                logOp("DEC B");
                break;

            case 0x06:  // LD B, n
                reg_B = n = readN();
                logOp("LD B, %s", hex8(n));
                break;

            case 0x07:  // RLCA
                rlca();
                logOp("RLCA");
                break;

            case 0x08:  // EX AF, AF'
                n = reg_A;
                reg_A = reg_a;
                reg_a = n;
                f = reg_F;
                reg_F = reg_f;
                reg_f = f;
                logOp("EX AF, AF'");
                break;

            case 0x09:  // ADD HL, BC
                HL(add16(HL(), BC()));
                logOp("ADD HL, BC");
                break;

            case 0x0A:  // LD A, (BC)
                reg_A = mem.read8(BC());
                logOp("LD A, (BC)");
                break;

            case 0x0B:  // DEC BC
                BC(dec16(BC()));
                logOp("DEC BC");
                break;

            case 0x0C:  // INC C
                reg_C = inc8(reg_C);
                logOp("INC C");
                break;

            case 0x0D:  // DEC C
                reg_C = dec8(reg_C);
                logOp("DEC C");
                break;

            case 0x0E:  // LD C, n
                reg_C = n = readN();
                logOp("LD C, %s", hex8(n));
                break;

            case 0x0F:  // RRCA
                rrca();
                logOp("RRCA");
                break;

            case 0x10:  // DJNZ (PC+e)
                n = readN();
                reg_B = (reg_B - 1) & 0xFF;
                if (reg_B != 0) {
                    reg_PC += (byte) n;
                }
                logOp("DJNZ %s", hex8(n));
                break;

            case 0x11:  // LD DE, nn
                nn = readNn();
                DE(nn);
                logOp("LD DE, %s", hex16(nn));
                break;

            case 0x12:  // LD (DE), A
                mem.write8(DE(), reg_A);
                logOp("LD (DE), A");
                break;

            case 0x13:  // INC DE
                DE(inc16(DE()));
                logOp("INC DE");
                break;

            case 0x14:  // INC D
                reg_D = inc8(reg_D);
                logOp("INC D");
                break;

            case 0x15:  // DEC D
                reg_D = dec8(reg_D);
                logOp("DEC D");
                break;

            case 0x16:  // LD D, n
                reg_D = n = readN();
                logOp("LD D, %s", hex8(n));
                break;

            case 0x17:  // RLA
                rla();
                logOp("RLA");
                break;

            case 0x18:  // JR e
                n = readN();
                reg_PC -= 2;
                reg_PC += (byte) n;
                logOp("JR %s", hex8(n));
                break;

            case 0x19:  // ADD HL, DE
                HL(add16(HL(), DE()));
                logOp("ADD HL, DE");
                break;

            case 0x1A:  // LD A, (DE)
                reg_A = mem.read8(DE());
                logOp("LD A, (DE)");
                break;

            case 0x1B:  // DEC DE
                DE(dec16(DE()));
                logOp("DEC DE");
                break;

            case 0x1C:  // INC E
                reg_E = inc8(reg_E);
                logOp("INC E");
                break;

            case 0x1D:  // DEC E
                reg_E = dec8(reg_E);
                logOp("DEC E");
                break;

            case 0x1E:  // LD E, n
                reg_E = n = readN();
                logOp("LD E, %s", hex8(n));
                break;

            case 0x1F:  // RRA
                rra();
                logOp("RRA");
                break;

            case 0x20:  // JR NZ, e
                n = readN();
                if (!reg_F.Z) {
                    reg_PC -= 2;
                    reg_PC += (byte) n;
                }
                logOp("JR NZ, %s", hex8(n));
                break;

            case 0x21:  // LD HL, nn
                nn = readNn();
                HL(nn);
                logOp("LD HL, %s", hex16(nn));
                break;

            case 0x22:  // LD (nn), HL
                nn = readNn();
                mem.write8(nn+0, reg_L);
                mem.write8(nn+1, reg_H);
                logOp("LD (%s), HL", hex16(nn));
                break;

            case 0x23:  // INC HL
                HL(inc16(HL()));
                logOp("INC HL");
                break;

            case 0x24:  // INC H
                reg_H = inc8(reg_H);
                logOp("INC H");
                break;

            case 0x25:  // DEC H
                reg_H = dec8(reg_H);
                logOp("DEC H");
                break;

            case 0x26:  // LD H, n
                reg_H = n = readN();
                logOp("LD H, %s", hex8(n));
                break;

            case 0x27:  // DAA
                daa();
                logOp("DAA");
                break;

            case 0x28:  // JR Z, e
                n = readN();
                if (reg_F.Z) {
                    reg_PC -= 2;
                    reg_PC += (byte) n;
                }
                logOp("JR Z, %s", hex8(n));
                break;

            case 0x29:  // ADD HL, HL
                HL(add16(HL(), HL()));
                logOp("ADD HL, HL");
                break;

            case 0x2A:  // LD HL, (nn)
                nn = readNn();
                reg_H = mem.read8(nn+1);
                reg_L = mem.read8(nn + 0);
                logOp("LD HL, (nn)");
                break;

            case 0x2B:  // DEC HL
                HL(dec16(HL()));
                logOp("DEC HL");
                break;

            case 0x2C:  // INC L
                reg_L = inc8(reg_L);
                logOp("INC L");
                break;

            case 0x2D:  // DEC L
                reg_L = dec8(reg_L);
                logOp("DEC L");
                break;

            case 0x2E:  // LD L, n
                reg_L = n = readN();
                logOp("LD L, %s", hex8(n));
                break;

            case 0x2F:  // CPL
                reg_A = reg_A ^ 0xFF;
                reg_F.H = true;
                reg_F.N = true;
                logOp("CPL");
                break;

            case 0x30:  // JR NC, e
                n = readN();
                if (!reg_F.C) {
                    reg_PC -= 2;
                    reg_PC += (byte) n;
                }
                logOp("JR NC, %s", hex8(n));
                break;

            case 0x31:  // LD SP, nn
                nn = readNn();
                reg_SP = nn;
                logOp("LD SP, %s", hex16(nn));
                break;

            case 0x32:  // LD (nn), A
                nn = readNn();
                mem.write8(nn, reg_A);
                logOp("LD (%s), A", hex16(nn));
                break;

            case 0x33:  // INC SP
                reg_SP = inc16(reg_SP);
                logOp("INC SP");
                break;

            case 0x34:  // INC (HL)
                n = mem.read8(HL());
                n = inc8(n);
                mem.write8(HL(), n);
                logOp("INC (HL)");
                break;

            case 0x35:  // DEC (HL)
                n = mem.read8(HL());
                n = dec8(n);
                mem.write8(HL(), n);
                logOp("DEC (HL)");
                break;

            case 0x36:  // LD (HL), n
                n = readN();
                mem.write8(HL(), n);
                logOp("LD (HL), %s", hex8(n));
                break;

            case 0x37:  // SCF
                reg_F.C = true;
                reg_F.N = false;
                reg_F.H = false;
                logOp("SCF");
                break;

            case 0x38:  // JR C, e
                n = readN();
                if (reg_F.C) {
                    reg_PC -= 2;
                    reg_PC += (byte) n;
                }
                logOp("JR C, %s", hex8(n));
                break;

            case 0x39:  // ADD HL, SP
                HL(add16(HL(), reg_SP));
                logOp("ADD HL, SP");
                break;

            case 0x3A:  // LD A, (nn)
                nn = readNn();
                reg_A = mem.read8(nn);
                logOp("LD A, (%s)", hex16(nn));
                break;

            case 0x3B:  // DEC SP
                reg_SP = dec16(reg_SP);
                logOp("DEC SP");
                break;

            case 0x3C:  // INC A
                reg_A = inc8(reg_A);
                logOp("INC A");
                break;

            case 0x3D:  // DEC A
                reg_A = dec8(reg_A);
                logOp("DEC A");
                break;

            case 0x3E:  // LD A, n
                reg_A = n = readN();
                logOp("LD A, %s", hex8(n));
                break;

            case 0x3F:  // CCF
                reg_F.H = reg_F.C;
                reg_F.N = false;
                reg_F.C = !reg_F.C;
                logOp("CCF");
                break;

            case 0x40:  // LD B, B
                logOp("LD B, B");
                break;

            case 0x41:  // LD B, C
                reg_B = reg_C;
                logOp("LD B, C");
                break;

            case 0x42:  // LD B, D
                reg_B = reg_D;
                logOp("LD B, D");
                break;

            case 0x43:  // LD B, E
                reg_B = reg_E;
                logOp("LD B, E");
                break;

            case 0x44:  // LD B, F
                reg_B = reg_F.value();
                logOp("LD B, F");
                break;

            case 0x45:  // LD B, L
                reg_B = reg_L;
                logOp("LD B, L");
                break;

            case 0x46:  // LD B, (HL)
                reg_B = mem.read8(to16(reg_H, reg_L));
                logOp("LD B, (HL)");
                break;

            case 0x47:  // LD B, A
                reg_B = reg_A;
                logOp("LD B, A");
                break;

            case 0x48:  // LD C, B
                reg_C = reg_B;
                logOp("LD C, B");
                break;

            case 0x49:  // LD C, C
                logOp("LD C, C");
                break;

            case 0x4A:  // LD C, D
                reg_C = reg_D;
                logOp("LD C, D");
                break;

            case 0x4B:  // LD C, E
                reg_C = reg_E;
                logOp("LD C, E");
                break;

            case 0x4C:  // LD C, F
                reg_C = reg_F.value();
                logOp("LD C, F");
                break;

            case 0x4D:  // LD C, L
                reg_C = reg_L;
                logOp("LD C, L");
                break;

            case 0x4E:  // LD C, (HL)
                reg_C = mem.read8(to16(reg_H, reg_L));
                logOp("LD C, (HL)");
                break;

            case 0x4F:  // LD C, A
                reg_C = reg_A;
                logOp("LD C, A");
                break;

            case 0x50:  // LD D, B
                reg_D = reg_B;
                logOp("LD D, B");
                break;

            case 0x51:  // LD D, C
                reg_D = reg_C;
                logOp("LD D, C");
                break;

            case 0x52:  // LD D, D
                logOp("LD D, D");
                break;

            case 0x53:  // LD D, E
                reg_D = reg_E;
                logOp("LD D, E");
                break;

            case 0x54:  // LD D, F
                reg_D = reg_F.value();
                logOp("LD D, F");
                break;

            case 0x55:  // LD D, L
                reg_D = reg_L;
                logOp("LD D, L");
                break;

            case 0x56:  // LD D, (HL)
                reg_D = mem.read8(to16(reg_H, reg_L));
                logOp("LD D, (HL)");
                break;

            case 0x57:  // LD D, A
                reg_D = reg_A;
                logOp("LD D, A");
                break;

            case 0x58:  // LD E, B
                reg_E = reg_B;
                logOp("LD E, B");
                break;

            case 0x59:  // LD E, C
                reg_E = reg_C;
                logOp("LD E, C");
                break;

            case 0x5A:  // LD E, D
                reg_E = reg_D;
                logOp("LD E, D");
                break;

            case 0x5B:  // LD E, E
                logOp("LD E, E");
                break;

            case 0x5C:  // LD E, F
                reg_E = reg_F.value();
                logOp("LD E, F");
                break;

            case 0x5D:  // LD E, L
                reg_E = reg_L;
                logOp("LD E, L");
                break;

            case 0x5E:  // LD E, (HL)
                reg_E = mem.read8(to16(reg_H, reg_L));
                logOp("LD E, (HL)");
                break;

            case 0x5F:  // LD E, A
                reg_E = reg_A;
                logOp("LD E, A");
                break;

            case 0x60:  // LD H, B
                reg_H = reg_B;
                logOp("LD H, B");
                break;

            case 0x61:  // LD H, C
                reg_H = reg_C;
                logOp("LD H, C");
                break;

            case 0x62:  // LD H, D
                reg_H = reg_D;
                logOp("LD H, D");
                break;

            case 0x63:  // LD H, E
                reg_H = reg_E;
                logOp("LD H, E");
                break;

            case 0x64:  // LD H, F
                reg_H = reg_F.value();
                logOp("LD H, F");
                break;

            case 0x65:  // LD H, L
                reg_H = reg_L;
                logOp("LD H, L");
                break;

            case 0x66:  // LD H, (HL)
                reg_H = mem.read8(to16(reg_H, reg_L));
                logOp("LD H, (HL)");
                break;

            case 0x67:  // LD H, A
                reg_H = reg_A;
                logOp("LD H, A");
                break;

            case 0x68:  // LD L, B
                reg_L = reg_B;
                logOp("LD L, B");
                break;

            case 0x69:  // LD L, C
                reg_L = reg_C;
                logOp("LD L, C");
                break;

            case 0x6A:  // LD L, D
                reg_L = reg_D;
                logOp("LD L, D");
                break;

            case 0x6B:  // LD L, E
                reg_L = reg_E;
                logOp("LD L, E");
                break;

            case 0x6C:  // LD L, F
                reg_L = reg_F.value();
                logOp("LD L, F");
                break;

            case 0x6D:  // LD L, L
                logOp("LD L, L");
                break;

            case 0x6E:  // LD L, (HL)
                reg_L = mem.read8(to16(reg_H, reg_L));
                logOp("LD L, (HL)");
                break;

            case 0x6F:  // LD L, A
                reg_L = reg_A;
                logOp("LD L, A");
                break;

            case 0x70:  // LD (HL), B
                mem.write8(to16(reg_H, reg_L), reg_B);
                logOp("LD (HL), B");
                break;

            case 0x71:  // LD (HL), C
                mem.write8(to16(reg_H, reg_L), reg_C);
                logOp("LD (HL), C");
                break;

            case 0x72:  // LD (HL), D
                mem.write8(to16(reg_H, reg_L), reg_D);
                logOp("LD (HL), D");
                break;

            case 0x73:  // LD (HL), E
                mem.write8(to16(reg_H, reg_L), reg_E);
                logOp("LD (HL), E");
                break;

            case 0x74:  // LD (HL), F
                mem.write8(to16(reg_H, reg_L), reg_F.value());
                logOp("LD (HL), F");
                break;

            case 0x75:  // LD (HL), L
                mem.write8(to16(reg_H, reg_L), reg_L);
                logOp("LD (HL), L");
                break;

            case 0x76:  // HALT
                isHalted = true;
                logOp("HALT");
                break;

            case 0x77:  // LD (HL), A
                mem.write8(to16(reg_H, reg_L), reg_A);
                logOp("LD (HL), A");
                break;

            case 0x78:  // LD A, B
                reg_A = reg_B;
                logOp("LD A, B");
                break;

            case 0x79:  // LD A, C
                reg_A = reg_C;
                logOp("LD A, C");
                break;

            case 0x7A:  // LD A, D
                reg_A = reg_D;
                logOp("LD A, D");
                break;

            case 0x7B:  // LD A, E
                reg_A = reg_E;
                logOp("LD A, E");
                break;

            case 0x7C:  // LD A, F
                reg_A = reg_F.value();
                logOp("LD A, F");
                break;

            case 0x7D:  // LD A, L
                reg_A = reg_L;
                logOp("LD A, L");
                break;

            case 0x7E:  // LD A, (HL)
                reg_A = mem.read8(to16(reg_H, reg_L));
                logOp("LD A, (HL)");
                break;

            case 0x7F:  // LD A, A
                logOp("LD A, A");
                break;

            case 0x80:  // ADD A, B
                reg_A = add8(reg_A, reg_B);
                logOp("ADD A, B");
                break;

            case 0x81:  // ADD A, C
                reg_A = add8(reg_A, reg_C);
                logOp("ADD A, C");
                break;

            case 0x82:  // ADD A, D
                reg_A = add8(reg_A, reg_D);
                logOp("ADD A, D");
                break;

            case 0x83:  // ADD A, E
                reg_A = add8(reg_A, reg_E);
                logOp("ADD A, E");
                break;

            case 0x84:  // ADD A, H
                reg_A = add8(reg_A, reg_H);
                logOp("ADD A, H");
                break;

            case 0x85:  // ADD A, L
                reg_A = add8(reg_A, reg_L);
                logOp("ADD A, L");
                break;

            case 0x86:  // ADD A, (HL)
                n = mem.read8(HL());
                reg_A = add8(reg_A, n);
                logOp("ADD A, (HL)");
                break;

            case 0x87:  // ADD A, A
                reg_A = add8(reg_A, reg_A);
                logOp("ADD A, A");
                break;

            case 0x88:  // ADC A, B
                reg_A = adc8(reg_A, reg_B);
                logOp("ADC A, B");
                break;

            case 0x89:  // ADC A, C
                reg_A = adc8(reg_A, reg_C);
                logOp("ADC A, C");
                break;

            case 0x8A:  // ADC A, D
                reg_A = adc8(reg_A, reg_D);
                logOp("ADC A, D");
                break;

            case 0x8B:  // ADC A, E
                reg_A = adc8(reg_A, reg_E);
                logOp("ADC A, E");
                break;

            case 0x8C:  // ADC A, H
                reg_A = adc8(reg_A, reg_H);
                logOp("ADC A, H");
                break;

            case 0x8D:  // ADC A, L
                reg_A = adc8(reg_A, reg_L);
                logOp("ADC A, L");
                break;

            case 0x8E:  // ADC A, (HL)
                reg_A = adc8(reg_A, mem.read8(HL()));
                logOp("ADC A, (HL)");
                break;

            case 0x8F:  // ADC A, A
                reg_A = adc8(reg_A, reg_A);
                logOp("ADC A, A");
                break;

            case 0x90:  // SUB A, B
                reg_A = sub8(reg_A, reg_B);
                logOp("SUB B");
                break;

            case 0x91:  // SUB A, C
                reg_A = sub8(reg_A, reg_C);
                logOp("SUB C");
                break;

            case 0x92:  // SUB A, D
                reg_A = sub8(reg_A, reg_D);
                logOp("SUB D");
                break;

            case 0x93:  // SUB A, E
                reg_A = sub8(reg_A, reg_E);
                logOp("SUB E");
                break;

            case 0x94:  // SUB A, H
                reg_A = sub8(reg_A, reg_H);
                logOp("SUB H");
                break;

            case 0x95:  // SUB A, L
                reg_A = sub8(reg_A, reg_L);
                logOp("SUB L");
                break;

            case 0x96:  // SUB A, (HL)
                n = mem.read8(HL());
                reg_A = sub8(reg_A, n);
                logOp("SUB (HL)");
                break;

            case 0x97:  // SUB A, A
                reg_A = sub8(reg_A, reg_A);
                logOp("SUB A");
                break;

            case 0x98:  // SBC A, B
                reg_A = sbc8(reg_A, reg_B);
                logOp("SBC B");
                break;

            case 0x99:  // SBC A, C
                reg_A = sbc8(reg_A, reg_C);
                logOp("SBC C");
                break;

            case 0x9A:  // SBC A, D
                reg_A = sbc8(reg_A, reg_D);
                logOp("SBC A, D");
                break;

            case 0x9B:  // SBC A, E
                reg_A = sbc8(reg_A, reg_E);
                logOp("SBC E");
                break;

            case 0x9C:  // SBC A, H
                reg_A = sbc8(reg_A, reg_H);
                logOp("SBC H");
                break;

            case 0x9D:  // SBC A, L
                reg_A = sbc8(reg_A, reg_L);
                logOp("SBC L");
                break;

            case 0x9E:  // SBC A, (HL)
                n = mem.read8(HL());
                reg_A = sbc8(reg_A, n);
                logOp("SBC (HL)");
                break;

            case 0x9F:  // SBC A, A
                reg_A = sbc8(reg_A, reg_A);
                logOp("SBC A");
                break;

            case 0xA0:  // AND B
                reg_A = and8(reg_A, reg_B);
                logOp("AND B");
                break;

            case 0xA1:  // AND C
                reg_A = and8(reg_A, reg_C);
                logOp("AND C");
                break;

            case 0xA2:  // AND D
                reg_A = and8(reg_A, reg_D);
                logOp("AND D");
                break;

            case 0xA3:  // AND E
                reg_A = and8(reg_A, reg_E);
                logOp("AND E");
                break;

            case 0xA4:  // AND H
                reg_A = and8(reg_A, reg_H);
                logOp("AND H");
                break;

            case 0xA5:  // AND L
                reg_A = and8(reg_A, reg_L);
                logOp("AND L");
                break;

            case 0xA6:  // AND (HL)
                n = mem.read8(HL());
                reg_A = and8(reg_A, n);
                logOp("AND (HL)");
                break;

            case 0xA7: // AND A
                reg_A = and8(reg_A, reg_A);
                logOp("AND A");
                break;

            case 0xA8: // XOR B
                reg_A = xor8(reg_A, reg_B);
                logOp("XOR B");
                break;

            case 0xA9: // XOR C
                reg_A = xor8(reg_A, reg_C);
                logOp("XOR C");
                break;

            case 0xAA: // XOR D
                reg_A = xor8(reg_A, reg_D);
                logOp("XOR D");
                break;

            case 0xAB: // XOR E
                reg_A = xor8(reg_A, reg_E);
                logOp("XOR E");
                break;

            case 0xAC: // XOR H
                reg_A = xor8(reg_A, reg_H);
                logOp("XOR H");
                break;

            case 0xAD: // XOR L
                reg_A = xor8(reg_A, reg_L);
                logOp("XOR L");
                break;

            case 0xAE: // XOR (HL)
                n = mem.read8(HL());
                reg_A = xor8(reg_A, n);
                logOp("XOR (HL)");
                break;

            case 0xAF: // XOR A
                reg_A = xor8(reg_A, reg_A);
                logOp("XOR A");
                break;

            case 0xB0: // OR B
                reg_A = or8(reg_A, reg_B);
                logOp("OR B");
                break;

            case 0xB1: // OR C
                reg_A = or8(reg_A, reg_C);
                logOp("OR C");
                break;

            case 0xB2: // OR D
                reg_A = or8(reg_A, reg_D);
                logOp("OR D");
                break;

            case 0xB3: // OR E
                reg_A = or8(reg_A, reg_E);
                logOp("OR E");
                break;

            case 0xB4: // OR H
                reg_A = or8(reg_A, reg_H);
                logOp("OR H");
                break;

            case 0xB5: // OR L
                reg_A = or8(reg_A, reg_L);
                logOp("OR L");
                break;

            case 0xB6: // OR (HL)
                n = mem.read8(HL());
                reg_A = or8(reg_A, n);
                logOp("OR (HL)");
                break;

            case 0xB7: // OR A
                reg_A = or8(reg_A, reg_A);
                logOp("OR A");
                break;

            case 0xB8: // CP B
                sub8(reg_A, reg_B);
                logOp("CP B");
                break;

            case 0xB9: // CP C
                sub8(reg_A, reg_C);
                logOp("CP C");
                break;

            case 0xBA: // CP D
                sub8(reg_A, reg_D);
                logOp("CP D");
                break;

            case 0xBB: // CP E
                sub8(reg_A, reg_E);
                logOp("CP E");
                break;

            case 0xBC: // CP H
                sub8(reg_A, reg_H);
                logOp("CP H");
                break;

            case 0xBD: // CP L
                sub8(reg_A, reg_L);
                logOp("CP L");
                break;

            case 0xBE: // CP (HL)
                n = mem.read8(HL());
                sub8(reg_A, n);
                logOp("CP (HL)");
                break;

            case 0xBF: // CP A
                sub8(reg_A, reg_A);
                logOp("CP A");
                break;

            case 0xC0: // RET NZ
                if (!reg_F.Z) {
                    ret();
                }
                logOp("RET NZ");
                break;

            case 0xC1: // POP BC
                BC(pop16());
                logOp("POP BC");
                break;

            case 0xC2: // JP NZ, nn
                nn = readNn();
                if (!reg_F.Z) {
                    reg_PC = nn;
                }
                logOp("JP NZ, %s", hex16(nn));
                break;

            case 0xC3: // JP nn
                nn = readNn();
                reg_PC = nn;
                logOp("JP %s", hex16(nn));
                break;

            case 0xC4: // CALL NZ, nn
                nn = readNn();
                if (!reg_F.Z) {
                    push16(reg_PC);
                    reg_PC = nn;
                }
                logOp("CALL NZ, %s", hex16(nn));
                break;

            case 0xC5: // PUSH BC
                push16(BC());
                logOp("PUSH BC");
                break;

            case 0xC6:  // ADD A, n
                n = readN();
                reg_A = add8(reg_A, n);
                logOp("ADD A, %s", hex8(n));
                break;

            case 0xC7:  // RST 0x00
                push16(reg_PC);
                reg_PC = 0x00;
                logOp("RST 0x00");
                break;

            case 0xC8:  // RET Z
                if (reg_F.Z) {
                    reg_PC = pop16();
                }
                logOp("RET Z");
                break;

            case 0xC9:  // RET
                reg_PC = pop16();
                logOp("RET");
                break;

            case 0xCA: // JP Z, nn
                nn = readNn();
                if (reg_F.Z) {
                    reg_PC = nn;
                }
                logOp("JP Z, %s", hex16(nn));
                break;

            case 0xCB:
                executeCB();
                break;

            case 0xCC: // CALL Z, nn
                nn = readNn();
                if (reg_F.Z) {
                    push16(reg_PC);
                    reg_PC = nn;
                }
                logOp("CALL Z, %s", hex16(nn));
                break;

            case 0xCD: // CALL nn
                nn = readNn();
                push16(reg_PC);
                reg_PC = nn;
                logOp("CALL %s", hex16(nn));
                break;

            case 0xCE:  // ADC A, n
                n = readN();
                reg_A = adc8(reg_A, n);
                logOp("ADC A, %s", hex8(n));
                break;

            case 0xCF:  // RST 0x08
                push16(reg_PC);
                reg_PC = 0x08;
                logOp("RST 0x08");
                break;

            case 0xD0:  // RET NZ
                if (!reg_F.Z) {
                    reg_PC = pop16();
                }
                logOp("RET NZ");
                break;

            case 0xD1: // POP DE
                DE(pop16());
                logOp("POP DE");
                break;

            case 0xD2: // JP NC, nn
                nn = readNn();
                if (!reg_F.C) {
                    reg_PC = nn;
                }
                logOp("JP NC, %s", hex16(nn));
                break;

            case 0xD3: // OUT (n), A
                n = readN();
                io.out8(n, reg_A);
                logOp("OUT (%n), A", hex8(n));
                break;

            case 0xD4: // CALL NC, nn
                nn = readNn();
                if (!reg_F.C) {
                    push16(reg_PC);
                    reg_PC = nn;
                }
                logOp("CALL NC, %s", hex16(nn));
                break;

            case 0xD5: // PUSH DE
                push16(DE());
                logOp("PUSH DE");
                break;

            case 0xD6:  // SUB n
                n = readN();
                reg_A = sub8(reg_A, n);
                logOp("SUB %s", hex8(n));
                break;

            case 0xD7:  // RST 0x10
                push16(reg_PC);
                reg_PC = 0x10;
                logOp("RST 0x10");
                break;

            case 0xD8:  // RET C
                if (reg_F.C) {
                    reg_PC = pop16();
                }
                logOp("RET C");
                break;

            case 0xD9: // EXX
                n = reg_B; reg_B = reg_b; reg_b = n;
                n = reg_C; reg_C = reg_c; reg_c = n;
                n = reg_D; reg_D = reg_d; reg_d = n;
                n = reg_E; reg_E = reg_e; reg_e = n;
                n = reg_H; reg_H = reg_h; reg_h = n;
                n = reg_L; reg_L = reg_l; reg_l = n;
                logOp("EXX");
                break;

            case 0xDA: // JP C, nn
                nn = readNn();
                if (reg_F.C) {
                    reg_PC = nn;
                }
                logOp("JP C, %s", hex16(nn));
                break;

            case 0xDB:  // IN A, (n)
                n = readN();
                reg_A = io.in8(to16(reg_A, n));
                logOp("IN A, (%s)", n);
                break;

            case 0xDC: // CALL C, nn
                nn = readNn();
                if (reg_F.C) {
                    push16(reg_PC);
                    reg_PC = nn;
                }
                logOp("CALL C, %s", hex16(nn));
                break;

            case 0xDD:  // LD w. indexed registers (IX / IY)
                executeDD();
                break;

            case 0xDE:  // SBC n
                n = readN();
                reg_A = sbc8(reg_A, n);
                logOp("SBC %s", hex8(n));
                break;

            case 0xDF:  // RST 0x18
                push16(reg_PC);
                reg_PC = 0x18;
                logOp("RST 0x18");
                break;

            case 0xE0:  // RET PO
                if (!reg_F.P) {
                    reg_PC = pop16();
                }
                logOp("RET PO");
                break;

            case 0xE1: // POP HL
                HL(pop16());
                logOp("POP HL");
                break;

            case 0xE2: // JP PO, nn
                nn = readNn();
                if (!reg_F.P) {
                    reg_PC = nn;
                }
                logOp("JP PO, %s", hex16(nn));
                break;

            case 0xE3: // EX (SP), HL
                h = reg_H;
                l = reg_L;
                reg_L = mem.read8(reg_SP+0);
                reg_H = mem.read8(reg_SP+1);
                mem.write8(reg_SP+0, l);
                mem.write8(reg_SP+1, h);
                logOp("EX (SP), HL");
                break;

            case 0xE4: // CALL PO, nn
                nn = readNn();
                if (!reg_F.P) {
                    push16(reg_PC);
                    reg_PC = nn;
                }
                logOp("CALL NZ, %s", hex16(nn));
                break;

            case 0xE5: // PUSH HL
                push16(HL());
                logOp("PUSH HL");
                break;

            case 0xE6:  // AND n
                n = readN();
                reg_A = and8(reg_A, n);
                logOp("AND %s", hex8(n));
                break;

            case 0xE7:  // RST 0x20
                push16(reg_PC);
                reg_PC = 0x20;
                logOp("RST 0x20");
                break;

            case 0xE8:  // RET PE
                if (reg_F.P) {
                    reg_PC = pop16();
                }
                logOp("RET PE");
                break;

            case 0xE9: // JP (HL)
                reg_PC = HL();
                logOp("JP (HL)");
                break;

            case 0xEA: // JP PE, nn
                nn = readNn();
                if (reg_F.P) {
                    reg_PC = nn;
                }
                logOp("JP PE, %s", hex16(nn));
                break;

            case 0xEB: // EX DE, HL
                n = reg_H; reg_H = reg_D; reg_D = n;
                n = reg_L; reg_L = reg_E; reg_E = n;
                logOp("EXX");
                break;

            case 0xEC: // CALL PE, nn
                nn = readNn();
                if (reg_F.P) {
                    push16(reg_PC);
                    reg_PC = nn;
                }
                logOp("CALL PE, %s", hex16(nn));
                break;

            case 0xED:
                executeED();
                break;

            case 0xEE: // XOR n
                n = readN();
                reg_A = xor8(reg_A, n);
                logOp("XOR %s", hex8(n));
                break;

            case 0xEF:  // RST 0x28
                push16(reg_PC);
                reg_PC = 0x28;
                logOp("RST 0x28");
                break;

            case 0xF0:  // RET P
                if (!reg_F.S) {
                    reg_PC = pop16();
                }
                logOp("RET P");
                break;

            case 0xF1: // POP AF
                AF(pop16());
                logOp("POP AF");
                break;

            case 0xF2: // JP P, nn
                nn = readNn();
                if (!reg_F.S) {
                    reg_PC = nn;
                }
                logOp("JP Z, %s", hex16(nn));
                break;

            case 0xF3: // DI
                IFF1 = IFF2 = false;
                logOp("DI");
                break;

            case 0xF4: // CALL P, nn
                nn = readNn();
                if (!reg_F.S) {
                    push16(reg_PC);
                    reg_PC = nn;
                }
                logOp("CALL P, %s", hex16(nn));
                break;

            case 0xF5: // PUSH AF
                push16(AF());
                logOp("PUSH AF");
                break;

            case 0xF6: // OR n
                n = readN();
                reg_A = or8(reg_A, n);
                logOp("OR %s", hex8(n));
                break;

            case 0xF7:  // RST 0x30
                push16(reg_PC);
                reg_PC = 0x30;
                logOp("RST 0x30");
                break;

            case 0xF8:  // RET M
                if (reg_F.S) {
                    reg_PC = pop16();
                }
                logOp("RET M");
                break;

            case 0xF9:  // LD SP, HL
                reg_SP = HL();
                logOp("LD SP, HL");
                break;

            case 0xFA: // JP M, nn
                nn = readNn();
                if (reg_F.S) {
                    reg_PC = nn;
                }
                logOp("JP M, %s", hex16(nn));
                break;

            case 0xFB: // EI
                IFF1 = IFF2 = true;
                logOp("EI");
                break;

            case 0xFC: // CALL M, nn
                nn = readNn();
                if (reg_F.S) {
                    push16(reg_PC);
                    reg_PC = nn;
                }
                logOp("CALL M, %s", hex16(nn));
                break;

            case 0xFD:  // LD w. indexed registers (IX / IY)
                executeFD();
                break;

            case 0xFE: // CP n
                n = readN();
                sub8(reg_A, n);
                logOp("CP %s", hex8(n));
                break;

            case 0xFF:  // RST 0x38
                push16(reg_PC);
                reg_PC = 0x38;
                logOp("RST 0x38");
                break;

            default:
                throw new RuntimeException("Not implemented yet: " + hex8(code));

        }
    }

    private void executeCB() {
        int d, n, r, b = 0;

        int code = readOp();

        switch (code) {
            case 0x00:  // RLC B
                reg_B = rlc8(reg_B);
                logOp("RLC B");
                break;

            case 0x01:  // RLC C
                reg_C = rlc8(reg_C);
                logOp("RLC C");
                break;

            case 0x02:  // RLC D
                reg_D = rlc8(reg_D);
                logOp("RLC D");
                break;

            case 0x03:  // RLC E
                reg_E = rlc8(reg_E);
                logOp("RLC E");
                break;

            case 0x04:  // RLC H
                reg_H = rlc8(reg_H);
                logOp("RLC H");
                break;

            case 0x05:  // RLC L
                reg_L = rlc8(reg_L);
                logOp("RLC L");
                break;

            case 0x06:  // RLC (HL)
                n = mem.read8(HL());
                n = rlc8(n);
                mem.write8(HL(), n);
                logOp("RLC (HL)");
                break;

            case 0x07:  // RLC A
                reg_A = rlc8(reg_A);
                logOp("RLC A");
                break;

            case 0x08:  // RRC B
                reg_B = rrc8(reg_B);
                logOp("RRC B");
                break;

            case 0x09:  // RRC C
                reg_C = rrc8(reg_C);
                logOp("RRC C");
                break;

            case 0x0A:  // RRC D
                reg_D = rrc8(reg_D);
                logOp("RRC D");
                break;

            case 0x0B:  // RRC E
                reg_E = rrc8(reg_E);
                logOp("RRC E");
                break;

            case 0x0C:  // RRC H
                reg_H = rrc8(reg_H);
                logOp("RRC H");
                break;

            case 0x0D:  // RRC L
                reg_L = rrc8(reg_L);
                logOp("RRC L");
                break;

            case 0x0E:  // RRC (HL)
                n = mem.read8(HL());
                n = rrc8(n);
                mem.write8(HL(), n);
                logOp("RRC (HL)");
                break;

            case 0x0F:  // RRC A
                reg_A = rrc8(reg_A);
                logOp("RRC A");
                break;

            case 0x10:  // RL B
                reg_B = rl8(reg_B);
                logOp("RL B");
                break;

            case 0x11:  // RL C
                reg_C = rl8(reg_C);
                logOp("RL C");
                break;

            case 0x12:  // RL D
                reg_D = rl8(reg_D);
                logOp("RL D");
                break;

            case 0x13:  // RL E
                reg_E = rl8(reg_E);
                logOp("RL E");
                break;

            case 0x14:  // RL H
                reg_H = rl8(reg_H);
                logOp("RL H");
                break;

            case 0x15:  // RL L
                reg_L = rl8(reg_L);
                logOp("RL L");
                break;

            case 0x16:  // RL (HL)
                n = mem.read8(HL());
                n = rl8(n);
                mem.write8(HL(), n);
                logOp("RL (HL)");
                break;

            case 0x17:  // RL A
                reg_A = rl8(reg_A);
                logOp("RL A");
                break;

            case 0x18:  // RR B
                reg_B = rr8(reg_B);
                logOp("RR B");
                break;

            case 0x19:  // RR C
                reg_C = rr8(reg_C);
                logOp("RR C");
                break;

            case 0x1A:  // RR D
                reg_D = rr8(reg_D);
                logOp("RR D");
                break;

            case 0x1B:  // RR E
                reg_E = rr8(reg_E);
                logOp("RR E");
                break;

            case 0x1C:  // RR H
                reg_H = rr8(reg_H);
                logOp("RR H");
                break;

            case 0x1D:  // RR L
                reg_L = rr8(reg_L);
                logOp("RR L");
                break;

            case 0x1E:  // RR (HL)
                n = mem.read8(HL());
                n = rr8(n);
                mem.write8(HL(), n);
                logOp("RR (HL)");
                break;

            case 0x1F:  // RR A
                reg_A = rr8(reg_A);
                logOp("RR A");
                break;

            case 0x20:  // SLA B
                reg_B = sla8(reg_B);
                logOp("SLA B");
                break;

            case 0x21:  // SLA C
                reg_C = sla8(reg_C);
                logOp("SLA C");
                break;

            case 0x22:  // SLA D
                reg_D = sla8(reg_D);
                logOp("SLA D");
                break;

            case 0x23:  // SLA E
                reg_E = sla8(reg_E);
                logOp("SLA E");
                break;

            case 0x24:  // SLA H
                reg_H = sla8(reg_H);
                logOp("SLA H");
                break;

            case 0x25:  // SLA L
                reg_L = sla8(reg_L);
                logOp("SLA L");
                break;

            case 0x26:  // SLA (HL)
                n = mem.read8(HL());
                n = sla8(n);
                mem.write8(HL(), n);
                logOp("SLA (HL)");
                break;

            case 0x27:  // SLA A
                reg_A = sla8(reg_A);
                logOp("SLA A");
                break;

            case 0x28:  // SRA B
                reg_B = sra8(reg_B);
                logOp("SRA B");
                break;

            case 0x29:  // SRA C
                reg_C = sra8(reg_C);
                logOp("SRA C");
                break;

            case 0x2A:  // SRA D
                reg_D = sra8(reg_D);
                logOp("SRA D");
                break;

            case 0x2B:  // SRA E
                reg_E = sra8(reg_E);
                logOp("SRA E");
                break;

            case 0x2C:  // SRA H
                reg_H = sra8(reg_H);
                logOp("SRA H");
                break;

            case 0x2D:  // SRA L
                reg_L = sra8(reg_L);
                logOp("SRA L");
                break;

            case 0x2E:  // SRA (HL)
                n = mem.read8(HL());
                n = sra8(n);
                mem.write8(HL(), n);
                logOp("SRA (HL)");
                break;

            case 0x2F:  // SRA A
                reg_A = sra8(reg_A);
                logOp("SRA A");
                break;

            case 0x30:  // SLL B
                reg_B = sll8(reg_B);
                logOp("SLL B");
                break;

            case 0x31:  // SLL C
                reg_C = sll8(reg_C);
                logOp("SLL C");
                break;

            case 0x32:  // SLL D
                reg_D = sll8(reg_D);
                logOp("SLL D");
                break;

            case 0x33:  // SLL E
                reg_E = sll8(reg_E);
                logOp("SLL E");
                break;

            case 0x34:  // SLL H
                reg_H = sll8(reg_H);
                logOp("SLL H");
                break;

            case 0x35:  // SLL L
                reg_L = sll8(reg_L);
                logOp("SLL L");
                break;

            case 0x36:  // SLL (HL)
                n = mem.read8(HL());
                n = sll8(n);
                mem.write8(HL(), n);
                logOp("SLL (HL)");
                break;

            case 0x37:  // SLL A
                reg_A = sll8(reg_A);
                logOp("SLL A");
                break;

            case 0x38:  // SRL B
                reg_B = srl8(reg_B);
                logOp("SRL B");
                break;

            case 0x39:  // SRL C
                reg_C = srl8(reg_C);
                logOp("SRL C");
                break;

            case 0x3A:  // SRL D
                reg_D = srl8(reg_D);
                logOp("SRL D");
                break;

            case 0x3B:  // SRL E
                reg_E = srl8(reg_E);
                logOp("SRL E");
                break;

            case 0x3C:  // SRL H
                reg_H = srl8(reg_H);
                logOp("SRL H");
                break;

            case 0x3D:  // SRL L
                reg_L = srl8(reg_L);
                logOp("SRL L");
                break;

            case 0x3E:  // SRL (HL)
                n = mem.read8(HL());
                n = srl8(n);
                mem.write8(HL(), n);
                logOp("SRL (HL)");
                break;

            case 0x3F:  // SRL A
                reg_A = srl8(reg_A);
                logOp("SRL A");
                break;

            case 0x40:  // BIT 0, B
            case 0x41:  // BIT 0, C
            case 0x42:  // BIT 0, D
            case 0x43:  // BIT 0, E
            case 0x44:  // BIT 0, H
            case 0x45:  // BIT 0, L
            case 0x47:  // BIT 0, A
            case 0x48:  // BIT 1, B
            case 0x49:  // BIT 1, C
            case 0x4A:  // BIT 1, D
            case 0x4B:  // BIT 1, E
            case 0x4C:  // BIT 1, H
            case 0x4D:  // BIT 1, L
            case 0x4F:  // BIT 1, A
            case 0x50:  // BIT 2, B
            case 0x51:  // BIT 2, C
            case 0x52:  // BIT 2, D
            case 0x53:  // BIT 2, E
            case 0x54:  // BIT 2, H
            case 0x55:  // BIT 2, L
            case 0x57:  // BIT 2, A
            case 0x58:  // BIT 3, B
            case 0x59:  // BIT 3, C
            case 0x5A:  // BIT 3, D
            case 0x5B:  // BIT 3, E
            case 0x5C:  // BIT 3, H
            case 0x5D:  // BIT 3, L
            case 0x5F:  // BIT 3, A
            case 0x60:  // BIT 4, B
            case 0x61:  // BIT 4, C
            case 0x62:  // BIT 4, D
            case 0x63:  // BIT 4, E
            case 0x64:  // BIT 4, H
            case 0x65:  // BIT 4, L
            case 0x67:  // BIT 4, A
            case 0x68:  // BIT 5, B
            case 0x69:  // BIT 5, C
            case 0x6A:  // BIT 5, D
            case 0x6B:  // BIT 5, E
            case 0x6C:  // BIT 5, H
            case 0x6D:  // BIT 5, L
            case 0x6F:  // BIT 5, A
            case 0x70:  // BIT 6, B
            case 0x71:  // BIT 6, C
            case 0x72:  // BIT 6, D
            case 0x73:  // BIT 6, E
            case 0x74:  // BIT 6, H
            case 0x75:  // BIT 6, L
            case 0x77:  // BIT 6, A
            case 0x78:  // BIT 7, B
            case 0x79:  // BIT 7, C
            case 0x7A:  // BIT 7, D
            case 0x7B:  // BIT 7, E
            case 0x7C:  // BIT 7, H
            case 0x7D:  // BIT 7, L
            case 0x7F:  // BIT 7, A
                r = code & 0x07;
                bit8((code >> 3) & 0x07, readReg8Val(r));
                logOp("BIT %s, %s", (code >> 3) & 0x07, reg8Name(r));
                break;

            case 0x4E:  // BIT 1, (HL)
            case 0x56:  // BIT 2, (HL)
            case 0x5E:  // BIT 3, (HL)
            case 0x66:  // BIT 4, (HL)
            case 0x6E:  // BIT 5, (HL)
            case 0x76:  // BIT 6, (HL)
            case 0x7E:  // BIT 7, (HL)
                bit8((code >> 3) & 0x07, mem.read8(HL()));
                logOp("BIT %s, (HL)", (code >> 3) & 0x07);
                break;

            case 0x80:  // RES 0, B
            case 0x81:  // RES 0, C
            case 0x82:  // RES 0, D
            case 0x83:  // RES 0, E
            case 0x84:  // RES 0, H
            case 0x85:  // RES 0, L
            case 0x87:  // RES 0, A
            case 0x88:  // RES 1, B
            case 0x89:  // RES 1, C
            case 0x8A:  // RES 1, D
            case 0x8B:  // RES 1, E
            case 0x8C:  // RES 1, H
            case 0x8D:  // RES 1, L
            case 0x8F:  // RES 1, A
            case 0x90:  // RES 2, B
            case 0x91:  // RES 2, C
            case 0x92:  // RES 2, D
            case 0x93:  // RES 2, E
            case 0x94:  // RES 2, H
            case 0x95:  // RES 2, L
            case 0x97:  // RES 2, A
            case 0x98:  // RES 3, B
            case 0x99:  // RES 3, C
            case 0x9A:  // RES 3, D
            case 0x9B:  // RES 3, E
            case 0x9C:  // RES 3, H
            case 0x9D:  // RES 3, L
            case 0x9F:  // RES 3, A
            case 0xA0:  // RES 4, B
            case 0xA1:  // RES 4, C
            case 0xA2:  // RES 4, D
            case 0xA3:  // RES 4, E
            case 0xA4:  // RES 4, H
            case 0xA5:  // RES 4, L
            case 0xA7:  // RES 4, A
            case 0xA8:  // RES 5, B
            case 0xA9:  // RES 5, C
            case 0xAA:  // RES 5, D
            case 0xAB:  // RES 5, E
            case 0xAC:  // RES 5, H
            case 0xAD:  // RES 5, L
            case 0xAF:  // RES 5, A
            case 0xB0:  // RES 6, B
            case 0xB1:  // RES 6, C
            case 0xB2:  // RES 6, D
            case 0xB3:  // RES 6, E
            case 0xB4:  // RES 6, H
            case 0xB5:  // RES 6, L
            case 0xB7:  // RES 6, A
            case 0xB8:  // RES 7, B
            case 0xB9:  // RES 7, C
            case 0xBA:  // RES 7, D
            case 0xBB:  // RES 7, E
            case 0xBC:  // RES 7, H
            case 0xBD:  // RES 7, L
            case 0xBF:  // RES 7, A
                b = (code >> 3) & 0x07;
                r = (code >> 0) & 0x07;
                n = res8(b, readReg8Val(r));
                writeReg8Val(r, n);
                logOp("RES %s, %s", b, reg8Name(r));
                break;

            case 0x86:  // RES 0, (HL)
            case 0x8E:  // RES 1, (HL)
            case 0x96:  // RES 2, (HL)
            case 0x9E:  // RES 3, (HL)
            case 0xA6:  // RES 4, (HL)
            case 0xAE:  // RES 5, (HL)
            case 0xB6:  // RES 6, (HL)
            case 0xBE:  // RES 7, (HL)
                b = (code >> 3) & 0x07;
                n = res8(b, HL());
                HL(n);
                logOp("RES %s, (HL)", b);
                break;

            case 0xC0:  // SET 0, B
            case 0xC1:  // SET 0, C
            case 0xC2:  // SET 0, D
            case 0xC3:  // SET 0, E
            case 0xC4:  // SET 0, H
            case 0xC5:  // SET 0, L
            case 0xC7:  // SET 0, A
            case 0xC8:  // SET 1, B
            case 0xC9:  // SET 1, C
            case 0xCA:  // SET 1, D
            case 0xCB:  // SET 1, E
            case 0xCC:  // SET 1, H
            case 0xCD:  // SET 1, L
            case 0xCF:  // SET 1, A
            case 0xD0:  // SET 2, B
            case 0xD1:  // SET 2, C
            case 0xD2:  // SET 2, D
            case 0xD3:  // SET 2, E
            case 0xD4:  // SET 2, H
            case 0xD5:  // SET 2, L
            case 0xD7:  // SET 2, A
            case 0xD8:  // SET 3, B
            case 0xD9:  // SET 3, C
            case 0xDA:  // SET 3, D
            case 0xDB:  // SET 3, E
            case 0xDC:  // SET 3, H
            case 0xDD:  // SET 3, L
            case 0xDF:  // SET 3, A
            case 0xE0:  // SET 4, B
            case 0xE1:  // SET 4, C
            case 0xE2:  // SET 4, D
            case 0xE3:  // SET 4, E
            case 0xE4:  // SET 4, H
            case 0xE5:  // SET 4, L
            case 0xE7:  // SET 4, A
            case 0xE8:  // SET 5, B
            case 0xE9:  // SET 5, C
            case 0xEA:  // SET 5, D
            case 0xEB:  // SET 5, E
            case 0xEC:  // SET 5, H
            case 0xED:  // SET 5, L
            case 0xEF:  // SET 5, A
            case 0xF0:  // SET 6, B
            case 0xF1:  // SET 6, C
            case 0xF2:  // SET 6, D
            case 0xF3:  // SET 6, E
            case 0xF4:  // SET 6, H
            case 0xF5:  // SET 6, L
            case 0xF7:  // SET 6, A
            case 0xF8:  // SET 7, B
            case 0xF9:  // SET 7, C
            case 0xFA:  // SET 7, D
            case 0xFB:  // SET 7, E
            case 0xFC:  // SET 7, H
            case 0xFD:  // SET 7, L
            case 0xFF:  // SET 7, A
                b = (code >> 3) & 0x07;
                r = (code >> 0) & 0x07;
                n = set8(b, readReg8Val(r));
                writeReg8Val(r, n);
                logOp("SET %s, %s", b, reg8Name(r));
                break;

            case 0xC6:  // SET 0, (HL)
            case 0xCE:  // SET 1, (HL)
            case 0xD6:  // SET 2, (HL)
            case 0xDE:  // SET 3, (HL)
            case 0xE6:  // SET 4, (HL)
            case 0xEE:  // SET 5, (HL)
            case 0xF6:  // SET 6, (HL)
            case 0xFE:  // SET 7, (HL)
                b = (code >> 3) & 0x07;
                n = set8(b, HL());
                HL(n);
                logOp("SET %s, (HL)", b);
                break;

            default:
                throw new RuntimeException("Not implemented yet: 0xCB " + hex8(code));
        }
    }


    private void executeDD() {
        int r, d, pp = 0;
        int n, nn, adr = 0;

        int code = readOp();

        switch (code) {

            case 0x09:  // ADD IX, BC
            case 0x19:  // ADD IX, DE
            case 0x29:  // ADD IX, IX
            case 0x39:  // ADD IX, SP
                pp = (code >> 4) & 0x03;
                reg_IX = add16(reg_IX, readReg16Val(pp));
                logOp("ADD IX, %s", hex16(pp));
                break;

            case 0x21:  // LD IX, nn
                nn = readNn();
                reg_IX = nn;
                logOp("LD IX, %s", hex16(nn));
                break;

            case 0x22:  // LD (nn), IX
                nn = readNn();
                mem.write8(nn, reg_IX);
                logOp("LD (%s), IX", hex16(nn));
                break;

            case 0x23:  // INC IX
                reg_IX = inc16(reg_IX);
                logOp("INC IX");
                break;

            case 0x2A:  // LD IX, (nn)
                nn = readNn();
                reg_IX = mem.read8(nn);
                logOp("LD IX, (%s)", hex16(nn));
                break;

            case 0x2B:  // DEC IX
                reg_IX = dec16(reg_IX);
                logOp("DEC IX");
                break;

            case 0x34:  // INC (IX+d)
                d = readN();
                adr = reg_IX + (byte) d;
                n = inc8(mem.read8(adr));
                mem.write8(adr, n);
                logOp("INC (IX + %s)", hex8(d));
                break;

            case 0x35:  // DEC (IX+d)
                d = readN();
                adr = reg_IX + (byte) d;
                n = dec8(mem.read8(adr));
                mem.write8(adr, n);
                logOp("DEC (IX + %s)", hex8(d));
                break;

            case 0x3A:  // LD A, (nn)
                int nL = readOp();
                int nH = readOp();
                nn = nH << 8 | nL;
                reg_A = mem.read8(nn);
                logOp("LD A, (%s))", hex8(nn));
                break;

            case 0x46:  // LD B, (IX + d)
            case 0x4E:  // LD C, (IX + d)
            case 0x56:  // LD D, (IX + d)
            case 0x5E:  // LD E, (IX + d)
            case 0x66:  // LD H, (IX + d)
            case 0x6E:  // LD L, (IX + d)
            case 0x7E:  // LD A, (IX + d)
                r = (code >> 3) & 0x07;
                d = readOp();
                writeReg8Val(r, mem.read8(reg_IX + (byte) d));
                logOp("LD %s, (IX + %s)", reg8Name(r), hex8(d));
                break;

            case 0x70:  // LD (IX + d), B
            case 0x71:  // LD (IX + d), C
            case 0x72:  // LD (IX + d), D
            case 0x73:  // LD (IX + d), E
            case 0x74:  // LD (IX + d), H
            case 0x75:  // LD (IX + d), L
            case 0x77:  // LD (IX + d), A
                r = code & 0x07;
                d = readOp();
                mem.write8(reg_IX + (byte) d, readReg8Val(r));
                logOp("LD (IX + %s), %s", hex8(d), reg8Name(r));
                break;


            case 0x86:  // ADD A, (IX + d)
                d = readOp();
                reg_A = add8(reg_A, mem.read8(reg_IX + (byte) d));
                logOp("ADD A, (IX + %s)", hex8(d));
                break;

            case 0x8E:  // ADC A, (IX + d)
                d = readOp();
                reg_A = adc8(reg_A, mem.read8(reg_IX + (byte) d));
                logOp("ADC A, (IX + %s)", hex8(d));
                break;

            case 0x96:  // SUB A, (IX + d)
                d = readOp();
                reg_A = sub8(reg_A, mem.read8(reg_IX + (byte) d));
                logOp("SUB A, (IX + %s)", hex8(d));
                break;

            case 0x9E:  // SBC A, (IX + d)
                d = readOp();
                reg_A = sbc8(reg_A, mem.read8(reg_IX + (byte) d));
                logOp("SBC A, (IX + %s)", hex8(d));
                break;

            case 0xA6:  // AND A, (IX + d)
                d = readOp();
                reg_A = and8(reg_A, mem.read8(reg_IX + (byte) d));
                logOp("AND A, (IX + %s)", hex8(d));
                break;

            case 0xAE:  // XOR A, (IX + d)
                d = readOp();
                reg_A = xor8(reg_A, mem.read8(reg_IX + (byte) d));
                logOp("XOR A, (IX + %s)", hex8(d));
                break;

            case 0xB6:  // OR A, (IX + d)
                d = readOp();
                reg_A = or8(reg_A, mem.read8(reg_IX + (byte) d));
                logOp("OR A, (IX + %s)", hex8(d));
                break;

            case 0xBE:  // CP A, (IX + d)
                d = readOp();
                cp8(reg_A, mem.read8(reg_IX + (byte) d));
                logOp("CP A, (IX + %s)", hex8(d));
                break;

            case 0xE1:  // POP IX
                reg_IX = pop16();
                logOp("POP IX");
                break;

            case 0xE3:  // EX (SP), IX
                n = pop16();
                push16(reg_IX);
                reg_IX = n;
                logOp("EX (SP), IX");
                break;

            case 0xE5: // JP (IX)
                reg_PC = reg_IX;
                logOp("JP (IX)");
                break;

            case 0xF9: // LD SP, IX
                reg_SP = reg_IX;
                logOp("LD SP, IX");
                break;

            default:
                throw new RuntimeException("Not implemented yet");
        }
    }

    private void executeED() {

        int r,v,ss,dd,nn;

        int code = readOp();
        switch (code) {
//z
            case 0x40:  // IN B, (C)
            case 0x48:  // IN C, (C)
            case 0x50:  // IN D, (C)
            case 0x58:  // IN E, (C)
            case 0x60:  // IN H, (C)
            case 0x68:  // IN L, (C)
            case 0x78:  // IN A, (C)
                r = (code >> 3) & 0x07;
                v = io.in8(to16(reg_B, reg_C));
                writeReg8Val(r, v);
                logOp("IN %s, (C)", reg8Name(r));
                break;

            case 0x41: // OUT (C), B
            case 0x49: // OUT (C), C
            case 0x51: // OUT (C), D
            case 0x59: // OUT (C), E
            case 0x61: // OUT (C), H
            case 0x69: // OUT (C), L
            case 0x79: // OUT (C), A
                r = (code >> 3) & 0x07;
                io.out8(to16(reg_B, reg_C), reg_A);
                logOp("OUT (C), %s", reg8Name(r));
                break;

            case 0x42: // SBC HL, BC
            case 0x52: // SBC HL, DE
            case 0x62: // SBC HL, HL
            case 0x72: // SBC HL, SP
                ss = (code >> 4) & 0x03;
                nn = sbc16(HL(), readReg16Val(ss));
                HL(nn);
                logOp("SBC HL, BC");
                break;

            case 0x43:  // LD (nn), BC
            case 0x53:  // LD (nn), DE
            case 0x73:  // LD (nn), SP
                nn = readNn();
                dd = (code >> 4) & 0x03;
                v = readReg16Val(dd);
                mem.write8(nn+0, low4(v));
                mem.write8(nn+1, high4(v));
                logOp("LD (%s), %s", hex16(nn), reg16Name(dd));
                break;

            case 0x44:  // NEG
                reg_A = sub8(0, reg_A);
                logOp("NEG");
                break;

            case 0x45:  // RETN
                reg_PC = pop16();
                IFF1 = IFF2;
                logOp("RETN");
                break;

            case 0x46:  // IM 0
                IM = 0;
                logOp("IM 0");
                break;

            case 0x47:  // LD I, A
                reg_I = reg_A;
                logOp("LD I, A");
                break;

            case 0x4A:  // ADC HL,BC
            case 0x5A:  // ADC HL,DE
            case 0x6A:  // ADC HL,HL
            case 0x7A:  // ADC HL,SP
                ss = (code >> 4) & 0x03;
                v = adc16(HL(), readReg16Val(ss));
                HL(v);
                logOp("ADC HL,%s", reg16Name(ss));
                break;

            case 0x4B:  // LD BC,(nn)
            case 0x5B:  // LD DE,(nn)
            case 0x7B:  // LD SP,(nn)
                ss = (code >> 4) & 0x03;
                nn = readNn();
                v = to16(mem.read8(nn + 1), mem.read8(nn));
                writeReg16Val(ss, v);
                logOp("LD %s,(%s)", reg16Name(ss), hex16(nn));
                break;

            case 0x4D:  // RETI
                // TODO

            case 0x4F:  // LD R, A
                reg_R = reg_A;
                logOp("LD R, A");
                break;

            case 0x56:  // IM 1
                IM = 1;
                logOp("IM 1");
                break;

            case 0x57:  // LD A, I
                /*
                  S is set if I-Register is negative; reset otherwise
                  Z is set if I-Register is zero; reset otherwise
                  H is reset
                  P/V contains contents of IFF2
                  N is reset
                  C is not affected
                  TODO: If an interrupt occurs during execution of this instruction, the Parity flag contains a 0.
                 */
                reg_A = reg_I;
                reg_F.S = reg_I < 0;
                reg_F.Z = reg_I == 0;
                reg_F.H = false;
                reg_F.P = IFF2;
                reg_F.N = false;
                logOp("LD A, I");
                break;

            case 0x5F:  // LD A, R
                /*
                  S is set if R-Register is negative; reset otherwise
                  Z is set if R-Register is zero; reset otherwise
                  H is reset
                  P/V contains contents of IFF2
                  N is reset
                  C is not affected
                  TODO: If an interrupt occurs during execution of this instruction, the Parity flag contains a 0.
                 */
                reg_A = reg_R;
                reg_F.S = reg_R < 0;
                reg_F.Z = reg_R == 0;
                reg_F.H = false;
                reg_F.P = IFF2;
                reg_F.N = false;
                logOp("LD A, R");
                break;

            case 0x5E:  // IM 2
                IM = 2;
                logOp("IM 2");
                break;

            default:
                throw new RuntimeException("Not implemented yet");
        }
    }

    private void executeFD() {

        int d = 0;

        int code = readOp();

        switch (code) {

            case 0x46:  // LD B, (IY + d)
                d = readOp();
                reg_B = mem.read8(reg_IY + (byte) d);
                break;

            case 0x4E:  // LD C, (IY + d)
                d = readOp();
                reg_C = mem.read8(reg_IY + (byte) d);
                break;

            case 0x56:  // LD D, (IY + d)
                d = readOp();
                reg_D = mem.read8(reg_IY + (byte) d);
                break;

            case 0x5E:  // LD E, (IY + d)
                d = readOp();
                reg_E = mem.read8(reg_IY + (byte) d);
                break;

            case 0x66:  // LD H, (IY + d)
                d = readOp();
                reg_H = mem.read8(reg_IY + (byte) d);
                break;

            case 0x6E:  // LD L, (IY + d)
                d = readOp();
                reg_L = mem.read8(reg_IY + (byte) d);
                break;

            case 0x70:  // LD (IY + d), B
                d = readOp();
                mem.write8(reg_IY + (byte) d, reg_B);
                break;

            case 0x71:  // LD (IY + d), C
                d = readOp();
                mem.write8(reg_IY + (byte) d, reg_C);
                break;

            case 0x72:  // LD (IY + d), D
                d = readOp();
                mem.write8(reg_IY + (byte) d, reg_D);
                break;

            case 0x73:  // LD (IY + d), E
                d = readOp();
                mem.write8(reg_IY + (byte) d, reg_E);
                break;

            case 0x74:  // LD (IY + d), F
                d = readOp();
                mem.write8(reg_IY + (byte) d, reg_F.value());
                break;

            case 0x75:  // LD (IY + d), L
                d = readOp();
                mem.write8(reg_IY + (byte) d, reg_L);
                break;

            case 0x77:  // LD (IY + d), A
                d = readOp();
                mem.write8(reg_IY + (byte) d, reg_A);
                break;

            case 0x7E:  // LD A, (IX + d)
                d = readOp();
                reg_A = mem.read8(reg_IX + (byte) d);
                break;

            default:
                throw new RuntimeException("Not implemented yet");
        }
    }

    private void writeReg8Val(int r, int val) {
        switch (r) {
            case 0: reg_B = val; break;
            case 1: reg_C = val; break;
            case 2: reg_D = val; break;
            case 3: reg_E = val; break;
            case 4: reg_H = val; break;
            case 5: reg_L = val; break;
            case 6: reg_C = val; break;
            case 7: reg_A = val; break;
            default:
                throw new RuntimeException("Unknown reg r: " + r);
        }
    }

    private int readReg8Val(int r) {
        switch (r) {
            case 0: return reg_B;
            case 1: return reg_C;
            case 2: return reg_D;
            case 3: return reg_E;
            case 4: return reg_H;
            case 5: return reg_L;
            case 6: return reg_C;
            case 7: return reg_A;
            default:
                throw new RuntimeException("Unknown reg r: " + r);
        }
    }

    private String reg8Name(int r) {
        switch (r) {
            case 0: return "B";
            case 1: return "C";
            case 2: return "D";
            case 3: return "E";
            case 4: return "H";
            case 5: return "L";
            case 6: return "C";
            case 7: return "A";
            default:
                throw new RuntimeException("Unknown reg r: " + r);
        }
    }

    private void writeReg16Val(int pp, int val) {
        switch (pp) {
            case 0: BC(val); break;
            case 1: DE(val); break;
            case 2: reg_IX = val; break;
            case 3: reg_SP = val; break;
            default:
                throw new RuntimeException("Unknown reg pp: " + pp);
        }
    }

    private int readReg16Val(int pp) {
        switch (pp) {
            case 0: return BC();
            case 1: return DE();
            case 2: return reg_IX;
            case 3: return reg_SP;
            default:
                throw new RuntimeException("Unknown reg pp: " + pp);
        }
    }

    private String reg16Name(int pp) {
        switch (pp) {
            case 0: return "BC";
            case 1: return "DE";
            case 2: return "IX";
            case 3: return "SP";
            default:
                throw new RuntimeException("Unknown reg pp: " + pp);
        }
    }

    private int rlc8(int val8) {
        int b7 = bit(val8, 7);

        val8 <<= 1;
        val8 |= b7;
        val8 &= 0xFF;

        reg_F.S = bit(val8, 7) > 0;
        reg_F.Z = val8 == 0;
        reg_F.H = false;
        reg_F.P = parityEven8(val8);
        reg_F.N = false;
        reg_F.C = b7 > 0;

        return val8;
    }

    private int rrc8(int val8) {
        int b0 = bit(val8, 0);

        val8 >>= 1;
        val8 |= b0 << 7;
        val8 &= 0xFF;

        reg_F.S = bit(val8, 7) > 0;
        reg_F.Z = val8 == 0;
        reg_F.H = false;
        reg_F.P = parityEven8(val8);
        reg_F.N = false;
        reg_F.C = b0 > 0;

        return val8;
    }

    private int rl8(int val8) {
        int b7 = bit(val8, 7);

        val8 <<= 1;
        val8 |= reg_F.C ? 0x01 : 0x00;
        val8 &= 0xFF;

        reg_F.S = bit(val8, 7) > 0;
        reg_F.Z = val8 == 0;
        reg_F.H = false;
        reg_F.P = parityEven8(val8);
        reg_F.N = false;
        reg_F.C = b7 > 0;

        return val8;
    }

    private int rr8(int val8) {
        int b0 = bit(val8, 0);

        val8 >>= 1;
        val8 |= reg_F.C ? 0x80 : 0x00;
        val8 &= 0xFF;

        reg_F.S = bit(val8, 7) > 0;
        reg_F.Z = val8 == 0;
        reg_F.H = false;
        reg_F.P = parityEven8(val8);
        reg_F.N = false;
        reg_F.C = b0 > 0;

        return val8;
    }

    private int sla8(int val8) {
        int b7 = bit(val8, 7);

        val8 <<= 1;
        val8 &= 0xFF;

        reg_F.S = bit(val8, 7) > 0;
        reg_F.Z = val8 == 0;
        reg_F.H = false;
        reg_F.P = parityEven8(val8);
        reg_F.N = false;
        reg_F.C = b7 > 0;

        return val8;
    }

    private int sra8(int val8) {
        int b7 = val8 & 0x80;
        int b0 = val8 & 0x01;

        val8 >>= 1;
        val8 |= b7;
        val8 &= 0xFF;

        reg_F.S = bit(val8, 7) > 0;
        reg_F.Z = val8 == 0;
        reg_F.H = false;
        reg_F.P = parityEven8(val8);
        reg_F.N = false;
        reg_F.C = b0 > 0;

        return val8;
    }

    private int sll8(int val8) {
        int b7 = bit(val8, 7);

        val8 <<= 1;
        val8 |= 0x01;
        val8 &= 0xFF;

        reg_F.S = bit(val8, 7) > 0;
        reg_F.Z = val8 == 0;
        reg_F.H = false;
        reg_F.P = parityEven8(val8);
        reg_F.N = false;
        reg_F.C = b7 > 0;

        return val8;
    }

    private int srl8(int val8) {
        int b0 = bit(val8, 0);

        val8 >>= 1;
        val8 &= 0xFF;

        reg_F.S = false;
        reg_F.Z = val8 == 0;
        reg_F.H = false;
        reg_F.P = parityEven8(val8);
        reg_F.N = false;
        reg_F.C = b0 > 0;

        return val8;
    }

    private void bit8(int pos, int val8) {
//        reg_F.S = false; // Unknown
        reg_F.Z = bit(val8, pos) == 0;
        reg_F.H = true;
//        reg_F.P = parityEven8(val8); // Unknown
        reg_F.N = false;
    }

    private int res8(int pos, int val) {
        return val & ~(1 << pos);
    }

    private int set8(int pos, int val) {
        return val | (1 << pos);
    }

    private int inc8(int val8) {
        int res = (val8 + 1) & 0xFF;

        reg_F.S = bit(res, 7) != 0;
        reg_F.Z = res == 0;
        reg_F.H = (res & 0x0F) == 0x00;  // H_FLAG = (RESULT&0x0F)==0x00
        reg_F.P = res == 0x80;
        reg_F.N = false;

        return res;
    }

    private int dec8(int val8) {
        int res = (val8 - 1) & 0xFF;

        reg_F.S = bit(res, 7) != 0;
        reg_F.Z = res == 0;
        reg_F.H = (res & 0x0F) == 0x0F;  // H_FLAG = (RESULT&0x0F)==0x0F
        reg_F.P = res == 0x7F;
        reg_F.N = true;

        return res;
    }

    private int inc16(int val16) {
        return (val16 + 1) & 0xFFFF;
    }

    private int dec16(int val16) {
        return (val16 - 1) & 0xFFFF;
    }

    private void rlca() {
        int n = bit(reg_A, 7);
        reg_A = ((reg_A << 1) + n) & 0xFF;
        reg_F.C = n == 1;
        reg_F.H = false;
        reg_F.N = false;
    }

    private void rrca() {
        int n = bit(reg_A, 0);
        reg_A = ((reg_A >> 1) + (n << 7)) & 0xFF;
        reg_F.C = n == 1;
        reg_F.H = false;
        reg_F.N = false;
    }

    private void rla() {
        int n = bit(reg_A, 7);
        reg_A = (reg_A << 1) & 0xFF;
        reg_A = reg_A | (reg_F.C ? 1 : 0);
        reg_F.C = n == 1;
        reg_F.H = false;
        reg_F.N = false;
    }

    private void rra() {
        int n = bit(reg_A, 0);
        reg_A >>= 1;
        reg_A |= reg_F.C ? (1 << 7) : 0;
        reg_A &= 0xFF;
        reg_F.C = n == 1;
        reg_F.H = false;
        reg_F.N = false;
    }

    private void daa() {
        int corr = 0;

        corr += low4(reg_A)  > 0x09 || reg_F.H ? 0x06 : 0x00;
        corr += high4(reg_A) > 0x09 || reg_F.C ? 0x60 : 0x00;

        // Special case
        if (low4(reg_A) > 0x09 && high4(reg_A) > 0x08 && !reg_F.C ) {
            corr = 0x66;
        }

        reg_A += reg_F.N ? -corr : corr;
        reg_A &= 0xFF;

        int low = low4(reg_A);
        int high = high4(reg_A);

        reg_F.S = bit(reg_A, 7) != 0;
        reg_F.Z = reg_A == 0;
        reg_F.C = reg_F.C ||
                  high >= 0x09 && low >= 0x0A ||
                  high >= 0x0A && low <= 0x09;
        reg_F.H = reg_F.N && reg_F.H && low <= 0x05 ||
                 !reg_F.N && low >= 0x0A;
        reg_F.Y = bit(reg_A, 5) != 0;
        reg_F.X = bit(reg_A, 3) != 0;
    }

    private int add8(int a8, int b8) {
        int res = a8 + b8;
        byte res8 = (byte) res;

        reg_F.S = res8 < 0;
        reg_F.Z = res8 == 0;
        reg_F.H = (res8 & 0x0F) < (a8 & 0x0F);
        reg_F.P = a8 >= 0 && b8 >= 0 && res8 < 0 || a8 < 0 && b8 < 0 && res8 >= 0;
        reg_F.N = false;
        reg_F.C = (res & 0x100) != 0;

        return res & 0xFF;
    }

    private int adc8(int a8, int b8) {
        int carry = reg_F.C ? 1 : 0;
        return add8(a8, b8 + carry);
    }

    private int add16(int a16, int b16) {
        int res = a16 + b16;
// TODO H ?
        reg_F.H = (res & 0x0FFF) < (a16 & 0x0FFF);
        reg_F.H = ((a16 ^ b16 ^ res) & 0x10) != 0; // FLAG = (A^B^RESULT)&0x10
        reg_F.N = false;
        reg_F.C = (res & 0x10000) != 0;

        return res & 0xFFFF;
    }

    private int adc16(int a8, int b8) {
        int carry = reg_F.C ? 1 : 0;
        return add16(a8, b8 + carry);
    }

    public static void main(String[] args) {
//        for (int a16 = -0x00ff; a16 < 0xffff; a16++) {
//            for (int b16 = -0x00ff; b16 < 0xffff; b16++) {
//                int res = a16 + b16;
//                boolean h1 = (res & 0x0FFF) < (a16 & 0x0FFF);
//                boolean h2 = ((a16 ^ b16 ^ res) & 0x10) != 0; // FLAG = (A^B^RESULT)&0x10
//
//                if (h1 != h2) {
//                    System.out.println("h1: " + h1 + ", h2: " + h2 +", a16: " + a16 + ", b16: " + b16);
//                }
//            }
//        }

        for (int a = -0xff; a < 0xff; a++) {
            for (int b = -0xff; b < 0xff; b++) {
                int res = a+b;
                byte res8 = (byte) res;

                boolean h1 = (res & 0x0F) < (a & 0x0F);
                boolean h2 = (((a & 0xf) + (b & 0xf)) & 0x10) != 0;
                if (h1 != h2) {
                    System.out.println("h1: " + h1 + ", h2: " + h2 +", a: " + a + ", b: " + b);
                }
            }
        }
    }
    private int sub8(int a8, int b8) {
        int res = a8 - b8;
        byte res8 = (byte) res;

        reg_F.S = res8 < 0;
        reg_F.Z = res8 == 0;
        reg_F.H = (res8 & 0x0F) > (a8 & 0x0F);
        reg_F.P = a8 >= 0 && b8 >= 0 && res8 < 0 || a8 < 0 && b8 < 0 && res8 >= 0;
        reg_F.N = true;
        reg_F.C = (res & 0x100) != 0;

        return res & 0xFF;
    }

    private int sbc8(int a8, int b8) {
        int carry = reg_F.C ? 1 : 0;
        return sub8(a8, b8 + carry);
    }

    private int sbc16(int a16, int b16) {
        int carry = reg_F.C ? 1 : 0;

        int res = a16 - b16 - carry;
        int res16 = res & 0xFFFF;

        reg_F.S = (res & 0x8000) != 0;
        reg_F.Z = res == 0;
        reg_F.H = (res & 0x0FFF) > (b16 & 0x0FFF);
        reg_F.P = res < -0xFFFF || res > 0xEFFF;
        reg_F.N = true;
        reg_F.C = (res & 0x10000) != 0;

        return res16;
    }

    private int and8(int a8, int b8) {
        int res = a8 & b8;
        byte res8 = (byte) res;

        reg_F.S = res8 < 0;
        reg_F.Z = res8 == 0;
        reg_F.H = true;
        reg_F.P = parityEven8(res8);
        reg_F.N = false;
        reg_F.C = false;

        return res & 0xFF;
    }

    private int xor8(int a8, int b8) {
        int res = a8 ^ b8;
        byte res8 = (byte) res;

        reg_F.S = res8 < 0;
        reg_F.Z = res8 == 0;
        reg_F.H = false;
        reg_F.P = parityEven8(res8);
        reg_F.N = false;
        reg_F.C = false;

        return res & 0xFF;
    }

    private int or8(int a8, int b8) {
        int res = a8 | b8;
        byte res8 = (byte) res;

        reg_F.S = res8 < 0;
        reg_F.Z = res8 == 0;
        reg_F.H = false;
        reg_F.P = parityEven8(res8);
        reg_F.N = false;
        reg_F.C = false;

        return res & 0xFF;
    }

    private void cp8(int a8, int b8) {
        sub8(a8, b8);
    }

    private void ret() {
        reg_PC = pop16();
    }

    private int pop16() {
        int l = mem.read8(reg_SP++);
        int h = mem.read8(reg_SP++);
        return to16(h, l);
    }

    private void push16(int val16) {
        mem.write8(--reg_SP, high8(val16));
        mem.write8(--reg_SP, low8(val16));
    }

    private int bit(int val, int pos) {
        return (val >> pos) & 0x01;
    }

    private boolean parityEven8(int val8) {
        return (Integer.bitCount(val8 & 0xFF) & 0x01) == 0;
    }

    private int toInt(boolean val) {
        return val ? 1 : 0;
    }

    private static String hex8(int val) {
        return String.format("0x%02X", val);
    }

    private static String hex16(int val) {
        return String.format("0x%04X", val);
    }

    private void log(String msg, Object ... args) {
        System.out.printf(msg, args);
        System.out.println();
    }

    private void logOp(String op, Object ... args) {
        System.out.printf("0x%04X:\t", opAddr);
        System.out.printf(op, args);
        System.out.printf("\t\t\t; A:%s B:%s C:%s D:%s E:%s H:%s L:%s IX:%s IY:%s I:%s R:%s F:%s", hex8(reg_A), hex8(reg_B), hex8(reg_C), hex8(reg_D), hex8(reg_E), hex8(reg_H), hex8(reg_L), hex16(reg_IX), hex16(reg_IY), hex8(reg_I), hex8(reg_R), hex8(reg_F.value()));
        System.out.println();
    }

    protected void loadRam(int address, byte ... data) {
        for (int val8 : data) {
            mem.write8(address++, val8 & 0xFF);
        }
    }

    protected void loadRam(int address, int ... data) {
        for (int val8 : data) {
            mem.write8(address++, val8);
        }
    }

}
