package org.leost.z80;

/**
 */
public interface IO {

    int in8(int port16);

    void out8(int port16, int val8);

}
