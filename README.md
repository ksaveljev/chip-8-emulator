CHIP-8 emulator
===============

My first attempt at writing an emulator. Ever. This is a somewhat working
emulator interpreting CHIP-8 language. There are a few issues known to me and I
am pretty sure quite a few which are still hiding. Nevertheless I'm quite happy
about the state of this project as it is the biggest Haskell project for me so
far.

    cabal sandbox init
    cabal install --dependencies-only
    cabal run -- roms/TETRIS

![Example gif output](http://ksaveljev.github.io/tetris.gif)

#### Known issues:

- no sound :(
- in some games (like Connect4) the pressed key appears to be "stuck" for a
  moment and you make a lot of moves
- in tetris it doesn't finish the game as the block collision detection fails
  (as far as I can understand) at the top level


This project has been influenced by:

- [Cowgod's Chip-8 Technical Reference v1.0](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM)
- [A bit of ST and a bit of IO: Designing a DCPU-16 emulator](http://jaspervdj.be/posts/2012-04-12-st-io-dcpu-16.html)
- [Other Chip8-Haskell implementation](https://github.com/Wollw/Chip8-Haskell)
