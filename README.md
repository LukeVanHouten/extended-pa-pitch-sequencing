# extended-pa-pitch-sequencing

WIP. Analyzing pitch sequences for plate appearances that are "extended" by foul balls on two-strike counts, i.e. pitches that did not change the count.

Currently, the Keras RNN model used is in a working state, but I have limited the foul ball aspect for now for simplicity as the model precision is poor (~62%). So it just tracks pitch sequencing vs. the predicted positive or negative outcome (for the pitcher) within each extended PA. I will continue to work on the foul ball part of the project in this repo as this is the more interesting part of the project, but maybe with a different model.