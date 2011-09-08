#!/usr/bin/python

from distutils.core import setup

setup(name='psdf',
      version='1.0',
      description='Python utilities for the Particle Stream Data Format',
      author='Will M. Farr',
      author_email='w-farr@northwestern.edu',
      url='https://github.com/jmakino/Particle-Stream-Data-Format',
      packages=['psdf'],
      scripts=['bin/energy.py', 'bin/input_output.py', 'bin/plummer_model.py',
               'bin/system_to_standard_units.py'],
      requires=['yaml'])
