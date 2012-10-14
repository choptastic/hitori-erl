# Hitori-erl
A Hitori puzzle solver written in Erlang
(c) 2012 Jesse Gumm
Release under the MIT License

## Why

Just for fun

## Complexity and runspeed

This is not the fastest algorithm in the world, but it's at least slightly smarter than brute force.

It's basic run-time is O(N!) where N represents the number of "offending squares" (squares which are
potentially necessary to make black to solve). Speeding up the algorithm depends largely on determining
a better mechanism to find a proper "mark black", such as marking squares that *must* be black. The current
method of finding which squares to try first is basically any squares that are potentially black and are
found in both a row and a column. Beyond that, no intelligence is used to speed up the process.

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
