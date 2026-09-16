import argparse
import copy
import math
from enum import Enum
from itertools import repeat
from typing import cast, Literal

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

type BlockGrid = list[list[BlockState]]

def cli() -> argparse.Namespace:
    p = argparse.ArgumentParser('test')
    p.add_argument('width', type=int)
    p.add_argument('-c', '--circle', action='store_true')
    p.add_argument('-a', '--show-all', action='store_true')
    p.add_argument('--ignore-centre', type=int)
    return p.parse_args()

def init_grid(width: int, circle = False, block_centre: int | None = None) -> BlockGrid:
    grid = []
    for _ in repeat(None, width):
        grid.append([BlockState.SOIL] * width)

    centre: float = (width - 1) / 2

    if block_centre is not None:
        for y in range(width):
            for x in range(width):
                horizontal_delta = abs(x - centre)
                vertical_delta = abs(y - centre)
                if horizontal_delta < block_centre and vertical_delta < block_centre:
                    grid[y][x] = BlockState.IGNORE

    if not circle:
        return grid

    radius: float = width / 2

    for y in range(width):
        for x in range (width):
            horizontal_delta = x - centre
            vertical_delta = y - centre
            if math.sqrt(horizontal_delta**2 + vertical_delta**2) > radius:
                grid[y][x] = BlockState.IGNORE

    return grid

def apply_pattern(congruency: int, grid: BlockGrid) -> None:
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

def score_grid(grid: BlockGrid, metric: BlockState = BlockState.UNUSABLE) -> int:
    '''Scores a grid by counting unusable blocks. A lower score is more optimal.'''
    return sum(1 for row in grid for block in row if block == metric)

def optimal_offsets(grid: BlockGrid) -> list[int]:
    '''Finds all minimum-scored layouts.'''
    scores: dict[int, int] = {}
    for i in range(0, 5):
        new_grid = copy.deepcopy(grid)
        apply_pattern(i, new_grid)
        scores[i] = score_grid(new_grid)

    print(scores)
    min_score = min(scores.values())
    return [offset for offset, score in scores.items() if score == min_score]

def display_grid(grid: BlockGrid) -> None:
    for row in grid:
        for block in row:
            print(f'{block.display()}', end='')
        print('\n', end='')

def main() -> None:
    args = cli()
    width = cast(int, args.width)
    circle = cast(bool, args.circle)
    show_all = cast(bool, args.show_all)
    ignore_centre = cast(int | None, args.ignore_centre)
    grid = init_grid(width, circle, ignore_centre)

    display_offsets = range(0, 5) if show_all else optimal_offsets(grid)

    for offset in display_offsets:
        display = copy.deepcopy(grid)
        apply_pattern(offset, display)
        soil = score_grid(display, metric=BlockState.SOIL)
        water = score_grid(display, metric=BlockState.WATER)
        print(f'Grid with offset {offset} — {soil} soil blocks, {water} water sources')
        display_grid(display)

if __name__ == '__main__':
    main()
