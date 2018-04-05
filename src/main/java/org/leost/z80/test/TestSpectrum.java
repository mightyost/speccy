package org.leost.z80.test;

import org.leost.z80.Z80;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 */
public class TestSpectrum {
    public static void main(String[] args) throws Exception {

        Path path = Paths.get("asm/a1.bin");
        byte[] data = Files.readAllBytes(path);

        Z80 z80 = new Z80();

        TestMemory mem = new TestMemory();
        TestUla ula = new TestUla();

        mem.load(0, data);

        z80.init(mem, ula);
        ula.init(mem);

        z80.run();
    }
}
