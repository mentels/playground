#!/bin/bash

function ls() {
    command ls -alt
}

echo "Aliased ls "
ls

echo "Normal ls"
command ls
