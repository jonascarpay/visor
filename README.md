## Visor

Visor uses machine learning to watch a game of Melee and produce a neat graph.
More generally, it is a library for extracting information from streams of games.
It is very modular, and can be (relatively) easily adapted to other games and purposes.
I wrote it, together with the `convoluted` library that powers it, while learning Haskell.

Note that it is in early alpha, and I'm still experimenting heavily with the API.
Furthermore, the data set I'm using to train the network is still relatively small and will e.g. not work with all characters.

That said, contributions are very welcome!

For more information, see reddit link

### Building
Due to the way capturing the screen is implemented, visor can currently only be used on OS X.

Visor is written in Haskell and uses the `stack` build system.
The numerical part depends on an installation of llvm 3.7.
You will probably have to run (some permutation of) the following commands:
```
brew install stack llvm37
ln -s /usr/local/bin/opt-3.7 /usr/local/bin/opt
ln -s /usr/local/bin/llc-3.7 /usr/local/bin/llc
```
Then just clone, cd into the directory, and run `stack install`.
You might have to run `stack setup` at some point during the process.
It requires the univers font to be present in `data/fonts`, which you will have to provide yourself.

### Usage
You will typically want to run `visor-app watch x y w h`, where (x,y) is the top left corner of the screen, and w and h are its width and height.
Different commands can be found in `app/Main.hs`.
