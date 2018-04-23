package org.leost.z80;

/**
 */
public interface IO {

    int in8(int port8);

    void out8(int port8, int val8);

}
