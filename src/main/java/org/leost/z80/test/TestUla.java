package org.leost.z80.test;

import org.leost.z80.IO;
import org.leost.z80.Memory;

/**
 */
public class TestUla implements IO {

    private Memory mem;

    public void init(Memory mem) {
        this.mem = mem;
    }

    @Override
    public int in8(int port16) {
        return 0;
    }

    @Override
    public void out8(int port16, int val8) {
    }
}
