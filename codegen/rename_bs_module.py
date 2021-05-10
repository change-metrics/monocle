# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later

# A script to adapt generated ocaml module name for ReScript:
# Replace snake_case to CamelCase and fix module import

import sys
import os
import re
import functools
from pathlib import Path


def pascalCase(name):
    return name[0].upper() + name[1:]


def pascalCases(name):
    return "".join(map(pascalCase, name.split("_")))


def snake_case(name):
    return "".join([(c if c.islower() else "_" + c.lower()) for c in name])


def fix_field_name(content):
    # the generated encoder/decoder for field names are using camelCase
    # while python/haskell are using snake_case.
    # This function fix that:
    return functools.reduce(
        lambda acc, field: acc.replace(field, '"' + snake_case(field[1:])),
        re.findall('"[a-z]+[A-Z][^"]', content),
        content,
    )


def fix_timestamp(content):
    # Fix timestamp message encoding which is a rfc3339 string, not an object
    return functools.reduce(
        lambda acc, field: acc.replace(
            field + '" (Js.Json.object_', field + '" (Js.Json.string'
        ),
        # TODO: add new timestamp field to this list, e.g. when this error happens:
        #   Js.Dict.set json "updated_at" (Js.Json.object_ json');
        # This has type: string,  Somewhere wanted: Js.Json.t Js.Dict.t
        ["timestamp", "updated_at", "created_at", "changed_at"],
        content,
    )


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
        content = content.replace(typeName, newTypeName)
    newFile.write_text(fix_timestamp(fix_field_name(content)))


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
