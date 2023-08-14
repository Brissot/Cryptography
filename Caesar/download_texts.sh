#!/bin/bash

curl "https://raw.githubusercontent.com/mmcky/nyu-econ-370/master/notebooks/data/book-war-and-peace.txt" -o war_n_peace.txt
curl "https://raw.githubusercontent.com/teropa/nlp/master/resources/corpora/gutenberg/shakespeare-caesar.txt" -o caesar.txt
curl "https://gist.githubusercontent.com/provpup/2fc41686eab7400b796b/raw/b575bd01a58494dfddc1d6429ef0167e709abf9b/hamlet.txt" -o hamlet.txt
curl "https://raw.githubusercontent.com/martin-gorner/tensorflow-rnn-shakespeare/master/shakespeare/tempest.txt" -o tempest.txt