#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup
import os.path

setup(name='Golbarg',
      version='0.2',
      description='A static blog generator',
      long_description=open('README.rst').read(),
      author='Thomas Jost',
      author_email='thomas.jost@gmail.com',
      url='http://github.com/Schnouki/Golbarg',
      requires=['Jinja2', 'Markdown', 'PyYAML'],
      platforms='any',
      scripts=['golbarg'],
      data_files=[(os.path.join('share', 'golbarg'),
                   ["COPYING", "README.rst", "config.yaml.example", "Makefile.example",
                    "golbarg.el"])],
      classifiers = [
          'Development Status :: 4 - Beta',
          'Environment :: Console',
          'Intended Audience :: End Users/Desktop',
          'License :: OSI Approved :: GNU General Public License (GPL)',
          'Operating System :: OS Independent',
          'Programming Language :: Python :: 2.6',
          'Topic :: Internet :: WWW/HTTP :: Dynamic Content :: News/Diary',
          'Topic :: Internet :: WWW/HTTP :: Site Management',
          ]
      )
