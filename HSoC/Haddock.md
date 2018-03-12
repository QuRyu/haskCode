# Haddock 
## Exploration 
* Haddock’s counterparts of parsed types in GHC 
* relevant pars of GHC that should be changed 
* a look at GHCi 


## Design Space
* add more information to hi file or reformat hi file? (the latter might break current projects, tho) 
* Hi file’s interface to Haddock (though this is not a relevant issue in this project) 


## Some thoughts
* As far as I understand, I only need to change ghc’s ModIface and ModGuts,  the function to extract ModIface from ModGuts, and MkIface, if GHC is well designed in terms of modularity 
* how should I change GHCi? 