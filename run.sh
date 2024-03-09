#!/bin/bash

rm -r .models
rm -r .reports
rm -r .logs
mkdir .models
mkdir .reports
mkdir .logs

npm run build

LOG_FILE=".logs/$(date +%s).log"
touch $LOG_FILE
SECONDS=0

ACTIVATION_FUNCTIONS=("sigmoid" "tanh")
TEST_DATA_LIMIT=1000

TRAINING_DATA_LIMITS=("20" "50")

#LEARNING_RATES=("0.01" "0.02" "0.03" "0.04" "0.05")
LEARNING_RATES=("0.05" "0.07" "0.09")

LAYERS=('--neurons=100' '--neurons=100 --neurons=20' '--neurons=40 --neurons=40')

for TRAINING_DATA_LIMIT in ${TRAINING_DATA_LIMITS[@]}; do
  for ACTIVATION_FUNCTION in ${ACTIVATION_FUNCTIONS[@]}; do
    for LEARNING_RATE in ${LEARNING_RATES[@]}; do
      for ((LAYER_INDEX = 0; LAYER_INDEX < ${#LAYERS[@]}; LAYER_INDEX++)); do

        # Run the train command with the current neuron parameter
        output=$(npm run build && ./index.js train --data-path=.data/mnist_train.csv --learning-rate=$LEARNING_RATE --activation-function=$ACTIVATION_FUNCTION --data-limit=$TRAINING_DATA_LIMIT ${LAYERS[$LAYER_INDEX]})

        # Extract the name of the model from the output
        model_path=$(echo "${output}" | sed -n 's/^.*Saved model: : \"\(.*\)\".*$/\1/p')

        # Run the test command with the model-path parameter
        result=$(npm run build && ./index.js test --model-path=${model_path} --data-path=.data/mnist_test.csv --data-limit=$TEST_DATA_LIMIT)

        accuracy=$(echo "${result}" | sed -n 's/^TOTAL:\s*\(.*\)%.*$/\1/p')

        duration="$((SECONDS / 60)) m $((SECONDS % 60)) s"
        SECONDS=0
        echo "Total accuracy ${accuracy}% - [${duration}] - (${model_path})"
        echo "Total accuracy ${accuracy}% - [${duration}] - (${model_path})" >> $LOG_FILE
      done
    done
  done
done