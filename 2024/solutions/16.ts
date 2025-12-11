import { INPUT } from "@/16";
import Heap from "heap-js";

const t = `###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############`;

const t2 = `#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################`;

const grid = INPUT.split("\n").map((x) => x.split(""));
const compare = (a, b) => a.cost - b.cost;
const pq = new Heap(compare);
const encodeState = (y, x, h) => `${y},${x},${h}`;

const start = { y: grid.length - 2, x: 1, h: 0, cost: 0, prev: null };

const dirs = [
  [-1, 0],
  [0, 1],
  [1, 0],
  [0, -1],
];

const dist = new Map(); // min dist for this node

const pred = new Map();

function walk() {
  pq.push(start);
  dist.set(encodeState(start.y, start.x, start.h), 0);
  pred.set(encodeState(start.y, start.x, start.h), []);

  let endStates = [];
  let minCost = Infinity;
  while (pq.size() > 0) {
    let now = pq.pop();
    const nowKey = encodeState(now.y, now.x, now.h);

    if (now.cost > (dist.get(nowKey) ?? Infinity)) {
      continue;
    }

    if (grid[now.y][now.x] === "E") {
      if (now.cost < minCost) {
        minCost = now.cost;
        endStates = [now];
      } else if (now.cost === minCost) {
        endStates.push(now);
      }
      continue;
    }

    for (let nh = 0; nh < 4; nh++) {
      const [ny, nx] = [now.y + dirs[nh][0], now.x + dirs[nh][1]];
      if (grid[ny]?.[nx] === "#") continue;
      if (Math.abs(now.h - nh) === 2) continue;

      let isTurn = now.h !== nh;

      const newCost = now.cost + (isTurn ? 1001 : 1);
      const neighborKey = encodeState(ny, nx, nh);
      const currentBest = dist.get(neighborKey) ?? Infinity;

      if (newCost < currentBest) {
        dist.set(neighborKey, newCost);
        pred.set(neighborKey, [nowKey]);
        pq.push({ y: ny, x: nx, h: nh, cost: newCost });
      } else if (newCost === currentBest) {
        pred.get(neighborKey).push(nowKey);
      }
    }
  }
  return { minCost, endStates, pred };
}

function getAllPaths(endStates, predecessors, start) {
  const allPaths = [];
  const startKey = encodeState(start.y, start.x, start.h);

  function backtrack(stateKey, path) {
    if (stateKey === startKey) {
      allPaths.push(path.reverse());
      return;
    }

    const preds = predecessors.get(stateKey) || [];
    for (const predKey of preds) {
      backtrack(predKey, [...path, predKey]);
    }
  }

  for (const endState of endStates) {
    const endKey = encodeState(endState.y, endState.x, endState.h);
    backtrack(endKey, [endKey]);
  }

  return allPaths;
}

let r = walk();
let b = getAllPaths(r.endStates, r.pred, start);
let k = new Set(
  b
    .flatMap((x) => x)
    .map((x) => [x.split(",")[0], x.split(",")[1]])
    .map((x) => `${x}`),
);
console.log(k.size);
