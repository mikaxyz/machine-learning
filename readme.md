# Experimental Neural Network in Elm

Experiments in classifying MNIST images using Elm. Current status is best case of ~94% accuracy with a learning time of ~1.5 hours with layer setup [784,100,10].

Todo:

[ ] Make it fast-er
[ ] Try more configurations generating better accuracy
[ ] Represent digits in binary? (4 output instead of 10)
[ ] Visualize learning/testing phases (with elm-charts?)
[ ] Fix `ActivationFunction.Tanh`. Not sure what the correct way is but something to handle negative values it produces.
[ ] RELU?