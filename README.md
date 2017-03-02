[![Build Status](https://travis-ci.org/dlindelof/cartwheel.svg?branch=master)](https://travis-ci.org/dlindelof/cartwheel)

This is the repository of the source code for a paper on the Bayesian evaluation
of energy conservation measures.

The paper is written with LyX, but I keep in this repository the generated .Rnw
file. To generate the LaTeX file, run

```
Rscript -e "library(knitr); knit('cartwheel.Rnw')"
```

The PDF is built automatically by Travis and published to S3. The most recent version
of this paper can always be downloaded here: https://s3.eu-west-2.amazonaws.com/cartwheel-lindelof/cartwheel.pdf

