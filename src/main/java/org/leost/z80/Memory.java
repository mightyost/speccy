package org.leost.z80;

/**
 */
public interface Memory {

    public int read8(int address);

    public void write8(int address, int data);

}
