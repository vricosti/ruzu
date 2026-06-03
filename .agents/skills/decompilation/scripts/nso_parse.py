#!/usr/bin/env python3
"""Print NSO header + dynamic section summary.

Usage: nso_parse.py <nso_path>
"""

import sys
from _nso_common import Nso, DT_NAMES


def main():
    if len(sys.argv) != 2:
        print(__doc__, file=sys.stderr)
        sys.exit(2)

    nso = Nso(sys.argv[1])
    print(f"=== NSO: {sys.argv[1]} ===")
    print(f".text   mem 0x{nso.text_mem_off:08X}-0x{nso.text_mem_off + nso.text_size:08X}  size 0x{nso.text_size:X}")
    print(f".rodata mem 0x{nso.rodata_mem_off:08X}-0x{nso.rodata_mem_off + nso.rodata_size:08X}  size 0x{nso.rodata_size:X}")
    print(f".rwdata mem 0x{nso.data_mem_off:08X}-0x{nso.data_mem_off + nso.data_size:08X}  size 0x{nso.data_size:X}")
    print(f".bss     mem 0x{nso.data_mem_off + nso.data_size:08X}-0x{nso.data_mem_off + nso.data_size + nso.bss_size:08X}  size 0x{nso.bss_size:X}")
    print(f"dynstr in rodata: off=0x{nso.dynstr_off_in_rodata:X}  size=0x{nso.dynstr_size:X}")
    print(f"dynsym in rodata: off=0x{nso.dynsym_off_in_rodata:X}  size=0x{nso.dynsym_size:X}  ({nso.n_symbols()} entries)")

    print(f"\n=== MOD0 ===")
    print(f"MOD0 header at .text+0x{nso.mod0_off:X}")
    print(f"DYNAMIC at mem 0x{nso.dynamic_mem_off:X}")

    print(f"\n=== DYNAMIC entries ===")
    dyn = nso.parse_dynamic()
    for name, val in dyn.items():
        if name == 'NEEDED':
            print(f"  NEEDED: {len(val)} entries")
        else:
            print(f"  {name:12s} = 0x{val:08X}")

    if 'JMPREL' in dyn:
        n_pltrel = dyn['PLTRELSZ'] // 8
        print(f"\n=== JMPREL: {n_pltrel} entries ===")
        print(f"  (PLT[N] for N>=1 corresponds to JMPREL[N-1] in standard ARM ELF layout)")


if __name__ == '__main__':
    main()
