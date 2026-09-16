import argparse
import math
from enum import Enum
from itertools import repeat
from typing import cast

class BlockState(Enum):
    SOIL = 1
    WATER = 2
    UNUSABLE = 3
    IGNORE = 4

    def display(self) -> str:
        match self:
            case BlockState.SOIL:
                return '\x1b[92m█\x1b[0m' #]]
            case BlockState.WATER:
                return '\x1b[94m░\x1b[0m' #]]
            case BlockState.UNUSABLE:
                return '\x1b[93m▓\x1b[0m' #]]
            case BlockState.IGNORE:
                return ' '

def cli() -> argparse.Namespace:
    p = argparse.ArgumentParser('test')
    p.add_argument('w', type=int)
    p.add_argument('-c', action='store_true')
    return p.parse_args()

def init_grid(width: int, circle = False) -> list[list[BlockState]]:
    grid = []
    for _ in repeat(None, width):
        grid.append([BlockState.SOIL] * width)

    if not circle:
        return grid

    centre: float = (width - 1) / 2
    radius: float = width / 2

    for y in range(width):
        for x in range (width):
            horizontal_delta = x - centre
            vertical_delta = y - centre
            if math.sqrt(horizontal_delta**2 + vertical_delta**2) > radius:
                grid[y][x] = BlockState.IGNORE

    return grid

def apply_pattern(congruency: int, grid: list[list[BlockState]]) -> None:
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            if grid[y][x] == BlockState.IGNORE:
                continue
            if ((2*x + y) % 5) == congruency:
                grid[y][x] = BlockState.WATER

    for y in range(len(grid)):
        for x in range(len(grid[0])):
            if grid[y][x] == BlockState.WATER:
                continue
            if grid[y][x] == BlockState.IGNORE:
                continue

            surroundings: list[BlockState] = []
            if x + 1 < len(grid[0]):
                surroundings.append(grid[y][x+1])
            if x - 1 >= 0:
                surroundings.append(grid[y][x-1])
            if y + 1 < len(grid):
                surroundings.append(grid[y+1][x])
            if y - 1 >= 0:
                surroundings.append(grid[y-1][x])

            if not any(block == BlockState.WATER for block in surroundings):
                grid[y][x] = BlockState.UNUSABLE

def main() -> None:
    args = cli()
    width = cast(int, args.w)
    circle = cast(bool, args.c)
    grid = init_grid(width, circle)

    apply_pattern(0, grid)

    for row in grid:
        for block in row:
            print(f'{block.display()}', end='')
        print('\n', end='')


if __name__ == '__main__':
    main()
