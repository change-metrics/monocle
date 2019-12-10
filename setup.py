#!/usr/bin/env python

from setuptools import setup

setup(
    name='monocle',
    version='0.0.1',
    packages=['monocle'],
    entry_points={
        'console_scripts': [
            'monocle=monocle.main:main',
        ]
    }
)
