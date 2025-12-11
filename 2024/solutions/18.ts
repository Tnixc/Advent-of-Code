import { INPUT } from "@/18";
import Heap from "heap-js";
const t = `5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0`;

const bytes = INPUT.split("\n").map((k) => {
  const a = k.split(",");
  return {
    x: a[0],
    y: a[1],
  };
});

const size = 71;

// ok = 2958

const time = 2959;
const grid = [];

for (let y = 0; y < size; y++) {
  grid.push(new Array(size).fill("."));
}

let last = [];
for (let i = 0; i < time; i++) {
  const k = bytes[i];
  grid[k.y][k.x] = "#";
  last = [k.x, k.y];
}

// console.log(grid.map((x) => x.join(" ")).join("\n"));

// console.log(grid);
const start = { y: 0, x: 0, cost: 0 };
const compare = (a, b) => a.cost - b.cost;

const pq = new Heap(compare);

const dirs = [
  [-1, 0],
  [0, 1],
  [1, 0],
  [0, -1],
];

const visited = new Set();
function walk() {
  let now = pq.pop();
  while (now) {
    if (now.y === size - 1 && now.x === size - 1) {
      return now;
    }
    if (visited.has(`${now.y} ${now.x}`)) {
      now = pq.pop();
      continue;
    }
    visited.add(`${now.y} ${now.x}`);
    for (let nh = 0; nh < 4; nh++) {
      const [ny, nx] = [now.y + dirs[nh][0], now.x + dirs[nh][1]];
      if (visited.has(`${ny} ${nx}`)) continue;
      if (ny < 0 || nx < 0 || ny >= size || nx >= size) continue;
      if (grid[ny]?.[nx] === "#") continue;
      pq.push({
        y: ny,
        x: nx,
        cost: now.cost + 1,
      });
    }
    now = pq.pop();
  }
  return null;
}

pq.push(start);

console.log(walk());
console.log(last);
