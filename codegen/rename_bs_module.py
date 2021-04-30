# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later

# A script to adapt generated ocaml module name for ReScript

import sys
import os
from pathlib import Path


def pascalCase(name):
    return name[0].upper() + name[1:]


def pascalCases(name):
    return "".join(map(pascalCase, name.split("_")))


def fix_module(filepath):
    newFile = filepath.rename(filepath.parent / pascalCases(filepath.name))
    print("Renamed", filepath, newFile)
    if filepath.name.split(".")[0].endswith("_bs"):
        typeName = pascalCase(filepath.name.split("_bs")[0] + "_types")
        newTypeName = pascalCases(typeName)
        content = newFile.read_text()
        # TODO(tristanC): figure out a better solution
        # We are using snake case attribute encoding, patch the decoder:
        content = content.replace("Regex", "_regex")
        print("Replaced", typeName, newTypeName)
        newFile.write_text(content.replace(typeName, newTypeName))


def main(root):
    for filename in os.listdir(root):
        if filename[0].islower() and not filename.endswith(".js"):
            fix_module(Path(root) / filename)


if __name__ == "__main__":
    main(sys.argv[1])
