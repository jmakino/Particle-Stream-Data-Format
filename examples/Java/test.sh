#!/bin/bash

set -e

export CLASSPATH="build:build/test:lib/snakeyaml-1.9.jar"

java PSDFTest
