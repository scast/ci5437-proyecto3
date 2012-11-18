#!/bin/bash

ENCODER=~/proyecto3/encoder
DECODER=~/proyecto3/decoder
SAT_SOLVER=glucose_2.0/
CNF=~/.sudoku.temporal
# Corremos el generador de dimacs
$ENCODER $1 > $CNF

# Corremos el SAT solver
cd $SAT_SOLVER
./glucose.sh $CNF | sed '/^$/d' | sed '/^c/d' | $DECODER $2

# rm $CNF
