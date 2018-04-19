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

    protected int opAddr;       // Program pointer for last op code

    protected int IFF1, IFF2;     // The two interrupt flip-flops

    private Memory mem;
    private IO io;

    private boolean isHalted = false;

    public void init(Memory mem, IO io) {
        this.mem = mem;
        this.io = io;
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
        int n, nn;
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
                logOp("SUB A, B");
                break;

            case 0x91:  // SUB A, C
                reg_A = sub8(reg_A, reg_C);
                logOp("SUB A, C");
                break;

            case 0x92:  // SUB A, D
                reg_A = sub8(reg_A, reg_D);
                logOp("SUB A, D");
                break;

            case 0x93:  // SUB A, E
                reg_A = sub8(reg_A, reg_E);
                logOp("SUB A, E");
                break;

            case 0x94:  // SUB A, H
                reg_A = sub8(reg_A, reg_H);
                logOp("SUB A, H");
                break;

            case 0x95:  // SUB A, L
                reg_A = sub8(reg_A, reg_L);
                logOp("SUB A, L");
                break;



            case 0xDB:  // IN A, (n)
                n = readN();
                reg_A = io.in8(to16(reg_A, n));
                logOp("IN A, (%s)", n);
                break;

            case 0xDD:  // LD w. indexed registers (IX / IY)
                executeDD();
                break;

            case 0xED:
                executeED();
                break;

            case 0xFD:  // LD w. indexed registers (IX / IY)
                executeFD();
                break;

            default:
                throw new RuntimeException("Not implemented yet: " + hex8(code));

        }
    }

    private void executeDD() {
        int d = 0;
        int n = 0;

        int code = readOp();

        switch (code) {

            case 0x3A:  // LD A, (nn)
                int nL = readOp();
                int nH = readOp();
                reg_A = mem.read8(nH << 8 | nL);
                break;

            case 0x46:  // LD B, (IX + d)
                d = readOp();
                reg_B = mem.read8(reg_IX + (byte) d);
                break;

            case 0x4E:  // LD C, (IX + d)
                d = readOp();
                reg_C = mem.read8(reg_IX + (byte) d);
                break;

            case 0x56:  // LD D, (IX + d)
                d = readOp();
                reg_D = mem.read8(reg_IX + (byte) d);
                break;

            case 0x5E:  // LD E, (IX + d)
                d = readOp();
                reg_E = mem.read8(reg_IX + (byte) d);
                break;

            case 0x66:  // LD H, (IX + d)
                d = readOp();
                reg_H = mem.read8(reg_IX + (byte) d);
                break;

            case 0x6E:  // LD L, (IX + d)
                d = readOp();
                reg_L = mem.read8(reg_IX + (byte) d);
                break;

            case 0x70:  // LD (IX + d), B
                d = readOp();
                mem.write8(reg_IX + (byte) d, reg_B);
                break;

            case 0x71:  // LD (IX + d), C
                d = readOp();
                mem.write8(reg_IX + (byte) d, reg_C);
                break;

            case 0x72:  // LD (IX + d), D
                d = readOp();
                mem.write8(reg_IX + (byte) d, reg_D);
                break;

            case 0x73:  // LD (IX + d), E
                d = readOp();
                mem.write8(reg_IX + (byte) d, reg_E);
                break;

            case 0x74:  // LD (IX + d), F
                d = readOp();
                mem.write8(reg_IX + (byte) d, reg_F.value());
                break;

            case 0x75:  // LD (IX + d), L
                d = readOp();
                mem.write8(reg_IX + (byte) d, reg_L);
                break;

            case 0x77:  // LD (IX + d), A
                d = readOp();
                mem.write8(reg_IX + (byte) d, reg_A);
                break;

            case 0xD5:  // LD B, n
                n = readOp();
                reg_B = n;
                break;

            case 0xDE:  // LD C, n
                n = readOp();
                reg_C = n;
                break;

            case 0x1B:  // LD D, n
                n = readOp();
                reg_D = n;
                break;

            case 0x1E:  // LD E, n
                n = readOp();
                reg_E = n;
                break;

            case 0x2B:  // LD H, n
                n = readOp();
                reg_H = n;
                break;

            case 0x36:  // LD L, n
                n = readOp();
                reg_L = n;
                break;

            default:
                throw new RuntimeException("Not implemented yet");
        }
    }

    private void executeED() {

        int code = readOp();
        switch (code) {

            case 0x47:  // LD I, A
                reg_I = reg_A;
                break;

            case 0x4F:  // LD R, A
                reg_R = reg_A;
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
                reg_F.P = IFF2 == 1;
                reg_F.N = false;
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
                reg_F.P = IFF2 == 1;
                reg_F.N = false;
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

    private int inc8(int val8) {
        int result = (val8 + 1) & 0xFF;

        reg_F.S = bit(result, 7) != 0;
        reg_F.Z = result == 0;
        reg_F.H = (result & 0x0F) == 0x00;  // H_FLAG = (RESULT&0x0F)==0x00
        reg_F.P = result == 0x80;
        reg_F.N = false;

        return result;
    }

    private int dec8(int val8) {
        int result = (val8 - 1) & 0xFF;

        reg_F.S = bit(result, 7) != 0;
        reg_F.Z = result == 0;
        reg_F.H = (result & 0x0F) == 0x0F;  // H_FLAG = (RESULT&0x0F)==0x0F
        reg_F.P = result == 0x7F;
        reg_F.N = true;

        return result;
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
        int res16 = a8 + b8;
        byte res8 = (byte) res16;

        reg_F.S = res8 < 0;
        reg_F.Z = res8 == 0;
        reg_F.H = (res8 & 0x0F) < (a8 & 0x0F);
        reg_F.P = a8 >= 0 && b8 >= 0 && res8 < 0 || a8 < 0 && b8 < 0 && res8 >= 0;
        reg_F.N = false;
        reg_F.C = res16 > 0xFF;

        return res8;
    }

    private int adc8(int a8, int b8) {
        int carry = reg_F.C ? 1 : 0;
        return add8(a8, b8 + carry);
    }

    private int add16(int a16, int b16) {
        int res = a16 + b16;

        reg_F.H = (res & 0x0FFF) < (a16 & 0x0FFF);
        reg_F.H = ((a16 ^ b16 ^ res) & 0x10) != 0; // FLAG = (A^B^RESULT)&0x10
        reg_F.N = false;
        reg_F.C = res > 0xFFFF;

        return res & 0xFFFF;
    }

    private int sub8(int a8, int b8) {
        int res16 = a8 + b8;
        byte res8 = (byte) res16;

        reg_F.S = res8 < 0;
        reg_F.Z = res8 == 0;
        reg_F.H = (res8 & 0x0F) > (a8 & 0x0F);
        reg_F.P = a8 >= 0 && b8 >= 0 && res8 < 0 || a8 < 0 && b8 < 0 && res8 >= 0;
        reg_F.N = false;
        reg_F.C = res16 > 0xFF;

        return res8;
    }

    private int bit(int val, int pos) {
        return (val >> pos) & 0x01;
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
