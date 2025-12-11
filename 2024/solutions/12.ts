import { INPUT } from "@/12";

const t = `AAAA
BBCD
BBCC
EEEC`;
const c = `AAABBBAA
BBBBBBBB`;
const t2 = `RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE`;

const grid = INPUT.split("\n").map((x) => x.split(""));
const dirs = [
  [-1, 0],
  [+1, 0],
  [0, -1],
  [0, +1],
];

const res = new Map();

let searched = new Set();
let currentKey: { y: number; x: number };
let currentArea = new Set();
let currentPerimeter = 0;
let corners = 0;

function flood(cell: string, origin: number, y: number, x: number) {
  const posKey = encode(y, x);
  if (searched.has(posKey)) return;
  searched.add(posKey);

  if (grid[y][x] === cell) {
    if (grid[y - 1]?.[x] != cell && grid[y][x - 1] != cell) corners++;
    if (grid[y - 1]?.[x] != cell && grid[y][x + 1] != cell) corners++;
    if (grid[y + 1]?.[x] != cell && grid[y][x - 1] != cell) corners++;
    if (grid[y + 1]?.[x] != cell && grid[y][x + 1] != cell) corners++;
    if (grid[y + 1]?.[x + 1] != cell && grid[y + 1]?.[x] === cell && grid[y][x + 1] === cell) {
      corners++;
    }
    if (grid[y - 1]?.[x - 1] != cell && grid[y - 1]?.[x] === cell && grid[y][x - 1] === cell) {
      corners++;
    }
    if (grid[y + 1]?.[x - 1] != cell && grid[y + 1]?.[x] === cell && grid[y][x - 1] === cell) {
      corners++;
    }
    if (grid[y - 1]?.[x + 1] != cell && grid[y - 1]?.[x] === cell && grid[y][x + 1] === cell) {
      corners++;
    }
  }

  for (const dir of dirs) {
    const [dy, dx] = dir;
    const [ny, nx] = [y - dy, x - dx];
    const search_cell = grid[ny]?.[nx];
    if (search_cell === cell) {
      if (ny < currentKey.y) currentKey.y = ny;
      if (nx < currentKey.x) currentKey.x = nx;
      // area set add ny,nx
      currentArea.add(encode(ny, nx));
      flood(cell, origin, ny, nx);
    } else {
      currentPerimeter++;
    }
  }
}

grid.forEach((row, y) =>
  row.forEach((cell, x) => {
    currentKey = { y: y, x: x };
    if (!res.has(`${cell} ${currentKey.y} ${currentKey.x}`)) {
      searched = new Set();
      currentArea = new Set();
      currentPerimeter = 0;
      currentArea.add(encode(y, x));
      corners = 0;
      flood(cell, encode(y, x), y, x);
      res.set(`${cell} ${currentKey.y} ${currentKey.x}`, {
        cell,
        currentArea,
        currentPerimeter,
        corners,
      });
    }
  }),
);

let c1 = 0;

[...res].forEach((v) => {
  const price = v[1].currentArea.size * v[1].currentPerimeter;
  c1 += price;
});

console.log(c1);

let c2 = 0;

[...res].forEach((v) => {
  const price = v[1].currentArea.size * v[1].corners;
  c2 += price;
});

console.log(c2);
// pain
// [...res].forEach((v) => {
//   console.log(v[1].cell);
//   let periSetX = new Set();
//   let periSetY = new Set();
//   v[1].currentPerimeter.forEach((el, i, a) => {
//     if (el.hy === -1 || el.hy === 1) {
//       // moving in y direction, meaing you want to scan horizontally, meaning same y level
//       const same_level = a
//         .filter((k) => k.y === el.y && k.hy === el.hy)
//         .map((k) => `${k.x} ${k.hy}`);
//       periSetX.add(`${same_level}`);
//     } else {
//       // moving in x
//       const same_level = a
//         .filter((k) => k.x === el.x && k.hx === el.hx)
//         .map((k) => `${k.y} ${k.hx}`);
//       periSetY.add(`${same_level}`);
//     }
//   });
//
//   const kX = [];
//   const kY = [];
//   [...periSetY].forEach((item) => {
//     const t = item.split(",").map((k) => k.split(" "));
//     kY.push(t);
//   });
//   [...periSetX].forEach((item) => {
//     const t = item.split(",").map((k) => k.split(" "));
//     kX.push(t);
//   });
//
//   let total = 0;
//   const peri = kY.concat(kX);
//
//   peri.forEach((p) => {
//     total += 1;
//     const v = p.map((k) => Number(k[0]));
//     // console.log(p);
//     // if (v.length > 1) {
//     //   // 1 = decreasing, -1 = increasing
//     //   // let isIncreasing = p[0][1] === "1";
//     //   let isIncreasing = v[1] > v[0];
//     //   if (v.length > 2) isIncreasing = v[2] > v[0];
//     //
//     //   let breaks = 0;
//     //   if (isIncreasing) {
//     //     for (let i = 1; i < v.length; i++) {
//     //       if (v[i] != v[i - 1] + 1) {
//     //         total += 1;
//     //         breaks++;
//     //       }
//     //     }
//     //   } else {
//     //     for (let i = 1; i < v.length; i++) {
//     //       if (v[i] != v[i - 1] - 1) {
//     //         total += 1;
//     //         breaks++;
//     //       }
//     //     }
//     //   }
//     let breaks = countBreaks(v);
//     console.log(v, breaks);
//     total += breaks;
//     // }
//   });
//   console.log(total);
//   console.log("===");
//   // console.log(peri);
//   c2 += total * v[1].currentArea.size;
// });
//
// console.log(c2);
//
function encode(a: number, b: number) {
  return (a << 12) | b;
}
//
// function decode(num: number) {
//   return {
//     y: num >> 12,
//     x: num & 0xff,
//   };
// }
//
