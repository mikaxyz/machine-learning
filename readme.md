# Experiment: Neural Network in Elm

Experiments in classifying MNIST images using Elm. Current status is best case of [~94% accuracy with a learning time of ~1.5 hours with layer setup [784,100,10]](/.models/lr=0.2,af=sigmoid,in=784,out=10,l=[100],i=60000.mnist_train_csv.json).

## How to use?

`npm install && npm build`

### Extract MNIST data to .data
`npm run extract-mnist-data`

### Test above mentioned model and store report in .reports
`npm run test-model`

### Test above mentioned modeland store model in .models
`npm run train`

## Todo:

- [ ] Make it fast-er
- [ ] Try more configurations generating better accuracy
- [ ] Represent digits in binary? (4 output instead of 10)
- [ ] Visualize learning/testing phases (with elm-charts?)
- [ ] Fix `ActivationFunction.Tanh`. Not sure what the correct way is but something to handle negative values it produces.
- [ ] RELU?