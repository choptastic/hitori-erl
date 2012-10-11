# Hitori-erl
A Hitori puzzle solver written in Erlang
(c) 2012 Jesse Gumm
Release under the MIT License

## About

This has been attempted on 12x12 and above and seems to take a long time. I haven't done a pile of optimizations.

This will process an 8x8 grid in around 250ms

## Why

Just for fun

## Usage

Load the source:

```erl
> c(hitori).
```

You can load your own puzzles by entering them as a 2-dimensional list. For example, to load a 5x5 puzzle:

```erlang
> B = hitori:from_list([
	[1,2,3,4,5],
	[2,3,4,5,1],
	[3,4,5,1,2],
	[4,5,1,2,3],
	[5,4,3,2,1]
]).
```

Then solve the puzzle with `hitori:solve/1`:

```erlang
> hitori:solve(B),
[{orig,[[1,2,3,4,5],
        [2,3,4,5,1],
        [3,4,5,1,2],
        [4,5,1,2,3],
        [5,4,3,2,1]]},
 {solved,[[1,2,x,4,5],
          [2,3,4,5,1],
          [3,4,5,1,2],
          [4,5,1,x,3],
          [5,x,3,2,x]]}]
```

It Returns the original unsolved puzzle and the solved puzzle with blacked out squares replaced with the atom `x`
