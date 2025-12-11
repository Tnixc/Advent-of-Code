import { INPUT } from "@/10";

const test = `0123
1234
8765
9876`;
const t2 = `89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732`;

console.time("");
const grid = INPUT.split("\n").map((x) => x.split("").map((y) => Number(y)));
const dirs = [
  [-1, 0],
  [0, -1],
  [0, +1],
  [+1, 0],
];

const trailHeadtoPeaks: string[] = [];
function hike(origin: string, y: number, x: number) {
  if (grid[y][x] === 9) {
    trailHeadtoPeaks.push(`${origin} ${y} ${x}`);
  } else {
    for (const direction of dirs) {
      const [dy, dx] = direction;
      if (grid[y - dy]?.at(x - dx) && grid[y - dy][x - dx] === grid[y][x] + 1) {
        hike(origin, y - dy, x - dx);
      }
    }
  }
}

for (let y = 0; y < grid.length; y++) {
  for (let x = 0; x < grid[0].length; x++) {
    if (grid[y][x] === 0) {
      hike(`${y} ${x}`, y, x);
    }
  }
}
console.log([new Set(trailHeadtoPeaks).size, trailHeadtoPeaks.length]);
console.timeEnd("");
