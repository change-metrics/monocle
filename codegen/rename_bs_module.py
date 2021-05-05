# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later

# A script to adapt generated ocaml module name for ReScript:
# Replace snake_case to CamelCase and fix module import

import sys
import os
from pathlib import Path


def pascalCase(name):
    return name[0].upper() + name[1:]


def pascalCases(name):
    return "".join(map(pascalCase, name.split("_")))


def fix_module(filepath):
    newFile = filepath.rename(filepath.parent / pascalCases(filepath.name))
    content = newFile.read_text()
    for shared_msg in ("Timestamp",):
        for suffix in ("types", "bs"):
            content = content.replace(
                shared_msg + "_" + suffix, shared_msg + pascalCase(suffix)
            )
    print("Renamed", filepath, newFile)
    if filepath.name.split(".")[0].endswith("_bs"):
        typeName = pascalCase(filepath.name.split("_bs")[0] + "_types")
        newTypeName = pascalCases(typeName)
        # TODO(tristanC): figure out a better solution
        # We are using snake case attribute encoding, patch the decoder:
        content = content.replace("Regex", "_regex").replace("taskTypes", "task_types")
        print("Replaced", typeName, newTypeName)
        content = content.replace(typeName, newTypeName)
    newFile.write_text(content)


def fixable_file(filename):
    return all(
        [
            filename[0].islower(),
            not filename.endswith(".js"),
            not filename.endswith(".res"),
        ]
    )


def main(root):
    for filename in filter(fixable_file, os.listdir(root)):
        fix_module(Path(root) / filename)


if __name__ == "__main__":
    main(sys.argv[1])
