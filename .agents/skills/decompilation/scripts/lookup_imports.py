#!/usr/bin/env python3
"""List imported functions in an NSO, optionally filtered by regex.

Usage:
  lookup_imports.py <nso_path> [regex_filter]

Examples:
  lookup_imports.py /tmp/mk8d_extract/exefs/main 'nn::audio'
  lookup_imports.py /tmp/mk8d_extract/exefs/main 'GetReleased'
"""

import re
import sys

from _nso_common import Nso


def main():
    if len(sys.argv) < 2 or len(sys.argv) > 3:
        print(__doc__, file=sys.stderr)
        sys.exit(2)

    nso = Nso(sys.argv[1])
    filt = re.compile(sys.argv[2]) if len(sys.argv) == 3 else None

    imports = []
    for sym in nso.iter_symbols():
        if sym is None:
            continue
        # Imported function: type=2 (STT_FUNC) and shndx=0 (SHN_UNDEF)
        if sym['type'] == 2 and sym['shndx'] == 0:
            imports.append(sym)

    print(f"Total imported functions: {len(imports)}\n")
    matched = 0
    for sym in imports:
        if filt and not filt.search(sym['name']):
            continue
        matched += 1
        # Truncate very long names
        name = sym['name']
        if len(name) > 200:
            name = name[:197] + '...'
        print(f"  sym[{sym['i']:4d}]  {name}")
    if filt:
        print(f"\n{matched} of {len(imports)} match {sys.argv[2]!r}")


if __name__ == '__main__':
    main()
