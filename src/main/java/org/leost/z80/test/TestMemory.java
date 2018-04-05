package org.leost.z80.test;

import org.leost.z80.Memory;

/**
 */
public class TestMemory implements Memory {

    private int[] mem = new int[48*1024];

    public int read8(int address) {
        return mem[address];
    }

    public void write8(int address, int data) {
        mem[address] = data;
    }

    public void load(int address16, byte[] data) {
        for (int val8 : data) {
            write8(address16++, val8 & 0xFF);
        }

    }
}
