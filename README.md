# sugarc
`sugarc` is a program and Haskell library for designing and optimising sugar
cane farms in [Minecraft](https://en.wikipedia.org/wiki/Minecraft).

> [!IMPORTANT]
> Right now, the program is in a very early alpha stage, and likely contains
> bugs or weird, unintuitive behaviour. Furthermore it is currently CLI-only.

Despite its current limitations, `sugarc` provides the building blocks to make
a farm of any possible size or configuration.

## Planned Features
- [x] Sugar cane layout optimisation
- [x] Basic shape primitives (rectangular, ellipsical)
- [x] Fine-grained block masking
- [x] Manual phase selection
- [ ] Machine Readable Output
- [ ] Image Output
- [ ] A small DSL to have a declarative farm
- [ ] Editor utilities (e.g., syntax highlighting, LSP)
- [ ] A simple GUI editor

## Usage
This is all subject to change, but the help message[^1] provides a good idea of
`sugarc`'s current capabilities:
```
usage: sugarc [-h|--help] from <SHAPE> [OPTIONS]

Options:
  -b, -block MASK          remove a section of the farm (see section `Mask Arguments`).
  -a, -allow MASK          add a section to the farm (see section `Mask Arguments`).
  -g, -gravity ALIGNMENT   set the alignment for subsequent masks (see section `Gravity Alignment`.
  -e, -emit TYPE           how to display selected farms.
                             Accepted Values:
                             • stdout/-
  -t, -take STRATEGY       which farms to display.
                             Accepted Values:
                             • one: show the first optimal layout
                             • any: show all optimal layouts
                             • all: show all layouts
                             • farm: just show the farm without optimising
                             • <INT>: number of layout [0..4]
```

Examples:
1. Get every optimal layout for a 10 wide square farm:
```
sugarc from square 10
```

2. Same as above but shorter:
```
sugarc -f square 10
```

3. Get a single optimal layout for a 10 wide circle farm:
```
sugarc from circle 10 -take one
```

4. Get a single optimal layout for an 11 wide square with a 3 wide square in the center removed:
```
sugarc from circle 11 -block square 3 -take one
```

5. Get all possible farms for a 10 by 15 rectangular farm with 2x2 squares removed from each corner:
```
sugarc from rectangle 10 15 \
    -gravity top+left \
    -block square 2 \
    -gravity top+right \
    -block square 2 \
    -gravity bottom+left \
    -block square 2 \
    -gravity bottom+right \
    -block square 2 \
    -take all 2
```

[^1]: This is a heavily modified excerpt of the help message. More details can
be found by executing the binary or looking in the module `SugarCane.Info`

## AI/LLM Disclosure
LLMs are only used for design discussion/reference. Every single line of code is
directly typed by me without exception. In other words, I use an LLM as a
"rubber ducky" or a faster web search, **not** as a coding agent.

Using it in this way in combination with a linter helps me write code more
efficiently and correctly without giving up my own agency (no pun intended).
