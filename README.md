ray-tracer
==========

A path tracer written in Haskell, based on [Peter Shirley's notes][shirley].


Building
--------

To build the code, run

    cabal new-build

To run the ray-tracer, use

    cabal new-run ray-tracer

To pass options, place them after a `--`:

    cabal new-run ray-tracer -- --samples 64

To run in parallel on `j` cores, call with `+RTS -Nj`, where `j` is an integer:

    cabal new-run ray-tracer -- +RTS -N4

Note: if rendering through `ghci`, it is recommended that you compile the
`Trace` module at least with `cd src && ghc -c -dynamic Trace.hs`.  This can
improve runtimes substantially.


Acknowledgements
----------------

See [NOTICES](NOTICES).


[shirley]: https://drive.google.com/file/d/1_MZBMUSO25pg1gyeHa_D8WXu7r37CvC6
