# ascii-tty-hs

ASCII images converter and player implemented in Haskell using friday and JuicyPixels library

# Demonstration

![demo](media/demo.gif)

# Usage

ascii-tty-hs takes all images from folder (`./frames` by default) with names consist of numbers and orders it in ascending order. Then,
it scales image to console window size with maximum value of 101 symbol for console height. If console supports ANSI, it will print image symbol
with according color. After pause (1 second by default), it will go to the next image from list

## Commands

ascii-tty-hs has only command - `run`. It will start the program

## Flags

- `-f/--frames <PATH>` - set path for images folder
- `--pause/-p <time in ms>` - set timeout after image printing
- `--start/-s <N>` - start showing images at Nth frame
- `--shuffle` - flag for shuffling images after full list pass

## Building

If you don't have stack, you can build binary file in Docker using Docker buildX plugin:

```bash
make build
# you will find binary in deployment folder
```
