#!/bin/bash

while read line; do
    ./sudoku_solver.sh $line
done < $1
