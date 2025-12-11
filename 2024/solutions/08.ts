import { INPUT } from "@/08";
const input = INPUT;

const test = `............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............`;

function encode(a: number, b: number, c = 0) {
  return (a << 10) | (b << 2) | c;
}

function main(str: string) {
  const grid = str.split("\n").map((line) => line.split(""));
  const antinodes = new Set();
  const antinodes2 = new Set();
  const h = grid.length;
  const w = grid[0].length;

  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      const cell = grid[y][x];
      if (cell === ".") continue;
      for (let sy = 0; sy < h; sy++) {
        for (let sx = 0; sx < w; sx++) {
          if (sy === y && sx === x) continue;
          const searchCell = grid[sy][sx];
          if (searchCell === cell) {
            const dy = y - sy;
            const dx = x - sx;
            if (grid[sy - dy] && grid[sy - dy][sx - dx]) {
              antinodes.add(encode(sy - dy, sx - dx));
            }

            let by = sy - dy;
            let bx = sx - dx;
            while (grid[by]?.[bx]) {
              antinodes2.add(encode(by, bx));
              by -= dy;
              bx -= dx;
            }

            let fy = sy + dy;
            let fx = sx + dx;
            while (grid[fy]?.[fx]) {
              antinodes2.add(encode(fy, fx));
              fy += dy;
              fx += dx;
            }
          }
        }
      }
    }
  }

  return [antinodes.size, antinodes2.size];
}
console.time("");
console.log(main(input));
console.timeEnd("");
