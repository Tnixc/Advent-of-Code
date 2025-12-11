import { INPUT } from "@/06";
const input = INPUT;

const _test = `....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...`;

const grid = input.split("\n").map((x) => x.split(""));
const sy = grid.map((x) => x.includes("^")).indexOf(true);
const sx = grid[sy].indexOf("^");

function encode(a: number, b: number, c = 0) {
  return (a << 10) | (b << 2) | c;
}

function decode(num: number) {
  return {
    y: num >> 10,
    x: (num >> 2) & 0xff,
    h: num & 0x3,
  };
}

const moves = [
  [-1, 0], // up
  [0, 1], // right
  [1, 0], // down
  [0, -1], // left
];

function part1(y: number, x: number) {
  const path = new Set<number>();

  let heading = 0;

  while (true) {
    path.add(encode(y, x));

    const [dy, dx] = moves[heading % 4];
    const newY = y + dy;
    const newX = x + dx;

    if (newY < 0 || newY >= grid.length || newX < 0 || newX >= grid[0].length) {
      break;
    }
    if (grid[newY][newX] !== "#") {
      //move
      y = newY;
      x = newX;
    } else {
      //turn
      heading++;
    }
  }
  console.log(path.size);
  return path;
}

function part2(sy: number, sx: number, p1path: Set<number>) {
  let c2 = 0;
  for (const point of Array.from(p1path)) {
    const { y: try_y, x: try_x } = decode(point);
    grid[try_y][try_x] = "#";
    let [y, x] = [sy, sx];
    let heading = 0;
    const path = new Set();
    while (true) {
      const currentState = encode(y, x, heading % 4);
      if (path.has(currentState)) {
        // we've been here
        c2++;
        break;
      }
      path.add(currentState);

      const [dy, dx] = moves[heading % 4];
      const newY = y + dy;
      const newX = x + dx;

      if (newY < 0 || newY >= grid.length || newX < 0 || newX >= grid[0].length) {
        break;
      }

      if (grid[newY][newX] !== "#") {
        y = newY;
        x = newX;
      } else {
        heading++;
      }
    }

    // set it back
    grid[try_y][try_x] = ".";
  }

  console.log(c2);
}
console.time("Part 1");
const p1 = part1(sy, sx);
console.timeEnd("Part 1");
console.time("Part 2");
part2(sy, sx, p1);
console.timeEnd("Part 2");
