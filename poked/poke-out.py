#!/usr/bin/env python3

import asyncio
import sys


async def main():
    (r, w) = await asyncio.open_unix_connection("poked.ipc")
    w.write(b"\x81")  # means output channel 1 (MSB is 1 means output)
    await w.drain()

    while True:
        lbytes = await r.readexactly(2)  # all payloads has 2-byte length (le)
        l = int.from_bytes(lbytes, byteorder="little")
        data = await r.readexactly(l)
        cmd = data[0]
        data = data[1:]

        if cmd == 1:  # New iteration of invocation of compiler
            iteration_n = int.from_bytes(data[:8], byteorder="little")
            print(f"\n//--- {iteration_n}")
        elif cmd == 2:  # Output (produced by running the Poke code)
            output = data[:-1].decode("ascii")  # discard the b"\x00" at end
            if output:
                print(output, end="")
                sys.stdout.flush()


asyncio.run(main())
