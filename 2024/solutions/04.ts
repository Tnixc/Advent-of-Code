import { INPUT } from "@/04";
const input = INPUT;
const t = `M.S.
.A..
M.S.
....`;
const t2 = `.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........`;
console.time("Part 1");
const grid = input.split("\n").map((x) => `____${x}____`.split(""));
for (let i = 0; i < 4; i++) {
  grid.unshift(Array(grid[0].length).fill("_"));
  grid.push(Array(grid[0].length).fill("_"));
}

let c1 = 0;
for (let y = 4; y < grid.length - 4; y++) {
  for (let x = 4; x < grid[0].length - 4; x++) {
    // horizontal
    let horz = [grid[y][x], grid[y][x + 1], grid[y][x + 2], grid[y][x + 3]];
    if (["XMAS", "SAMX"].includes(horz.join(""))) c1 += 1;
    // vert
    let vert = [grid[y][x], grid[y + 1][x], grid[y + 2][x], grid[y + 3][x]].join("");

    if (["XMAS", "SAMX"].includes(vert)) c1 += 1;

    // diag TL to BR
    let diag1 = [grid[y][x], grid[y + 1][x + 1], grid[y + 2][x + 2], grid[y + 3][x + 3]].join("");

    if (["XMAS", "SAMX"].includes(diag1)) c1 += 1;

    // diag TR to BL
    let diag2 = [grid[y][x], grid[y + 1][x - 1], grid[y + 2][x - 2], grid[y + 3][x - 3]].join("");

    if (["XMAS", "SAMX"].includes(diag2)) c1 += 1;
  }
}

console.log(c1);

console.timeEnd("Part 1");
console.time("Part 2");
let c2 = 0;
for (let y = 4; y < grid.length - 4; y++) {
  for (let x = 4; x < grid[0].length - 4; x++) {
    // extract 3x3
    let scan = [
      [grid[y - 1][x - 1], grid[y - 1][x], grid[y - 1][x + 1]],
      [grid[y][x - 1], grid[y][x], grid[y][x + 1]],
      [grid[y + 1][x - 1], grid[y + 1][x], grid[y + 1][x + 1]],
    ];
    let diag1 = [scan[0][0], scan[1][1], scan[2][2]].join("");
    let diag2 = [scan[0][2], scan[1][1], scan[2][0]].join("");
    if (["MAS", "SAM"].includes(diag1) && ["MAS", "SAM"].includes(diag2)) c2 += 1;
  }
}

console.log(c2);
console.timeEnd("Part 2");
