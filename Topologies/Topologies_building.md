# Halliday's topology constraint

For the Halliday's constraint, we build a majority consensus tree from the topologies of [Halliday and Goswami 2015](http://onlinelibrary.wiley.com/doi/10.1111/bij.12731/abstract) without branch length (using `ape::consensus` in R).

# Beck's topology constraint

For the Beck's constraint, we build a majority consensus tree from the 7 constrained topologies of [Beck and Lee 2014](http://rspb.royalsocietypublishing.org/content/281/1793/20141278) without branch length (using `ape::consensus` in R).
We selected the constrained trees only to avoid to much noise from the unconstrained topologies.

# SuperTree's topology constraint

For the super tree topology, we build a simple supertree from the two topologies described above using [clann](http://chriscreevey.github.io/clann/).
We ran the supertree search on both trees using the clann default parameters (Most Similar Supertree searched with a Sub-tree Pruning Regrafting algorithm for 10 heuristic searches).