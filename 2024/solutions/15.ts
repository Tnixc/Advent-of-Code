import { INPUT_A, INPUT_B } from "@/15";

const t_a = `##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########`;

const t_b = `<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^`;

const t1a = `########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########
`;
const t1b = `<^^>>>vv<v>>v<<`;
const t2a = `#######
#...#.#
#.....#
#..OO.#
#..O..#
#..@..#
#######`;
const t2b = `<vv<<^^<<^^`;
const grid = INPUT_A.split("\n").map((l) => l.split(""));
const instructions = INPUT_B.replaceAll("\n", "").split("");

function exec1(grid: string[][], ins: string) {
  const [ry, rx] = findRobot(grid);
  let [cy, cx] = [null, null];

  if (ins === "v") {
    cy = ry + 1;
    cx = rx;
  } else if (ins === "^") {
    cy = ry - 1;
    cx = rx;
  } else if (ins === ">") {
    cx = rx + 1;
    cy = ry;
  } else if (ins === "<") {
    cx = rx - 1;
    cy = ry;
  }

  while (grid[cy][cx] != "." && grid[cy][cx] != "#") {
    if (ins === "v") {
      cy = cy + 1;
    } else if (ins === "^") {
      cy = cy - 1;
    } else if (ins === ">") {
      cx = cx + 1;
    } else if (ins === "<") {
      cx = cx - 1;
    }
  }
  if (grid[cy][cx] === "#") {
    return;
  }
  if (grid[cy][cx] === ".") {
    grid[cy][cx] = "O";
    if (ins === "v") {
      grid[ry + 1][rx] = "@";
    } else if (ins === "^") {
      grid[ry - 1][rx] = "@";
    } else if (ins === ">") {
      grid[ry][rx + 1] = "@";
    } else if (ins === "<") {
      grid[ry][rx - 1] = "@";
    }
    grid[ry][rx] = ".";
  }
}

function part1() {
  for (const ins of instructions) {
    console.log(ins);
    console.log(grid.map((l) => l.join("")).join("\n"));
    exec1(grid, ins);
    console.log("---");
  }
  console.log("FINAL");
  console.log(grid.map((l) => l.join("")).join("\n"));

  let total = 0;
  grid.forEach((row, rowIndex) => {
    row.forEach((cell, cellIndex) => {
      if (cell === "O") {
        total += rowIndex * 100 + cellIndex;
      }
    });
  });
  console.log(total);
}

function findRobot(grid: string[][]) {
  for (let y = 0; y < grid.length; y++) {
    if (grid[y].includes("@")) {
      return [y, grid[y].indexOf("@")];
    }
  }
}

function resize(grid: string[][]) {
  let ngrid = [];
  grid.forEach((row) => {
    let nrow = [];
    row.forEach((cell) => {
      if (cell === "#") {
        nrow.push("#");
        nrow.push("#");
      } else if (cell === ".") {
        nrow.push(".");
        nrow.push(".");
      } else if (cell === "O") {
        nrow.push("[");
        nrow.push("]");
      } else if (cell === "@") {
        nrow.push("@");
        nrow.push(".");
      }
    });
    ngrid.push(nrow);
  });
  return ngrid;
}

function exec2(grid: string[][], ins: string) {
  const [ry, rx] = findRobot(grid);
  let [cy, cx] = [null, null];
  let cQ = [];

  if (ins === "v") {
    cy = ry + 1;
    cx = rx;
  } else if (ins === "^") {
    cy = ry - 1;
    cx = rx;
  } else if (ins === ">") {
    cx = rx + 1;
    cy = ry;
  } else if (ins === "<") {
    cx = rx - 1;
    cy = ry;
  }

  cQ.push([cy, cx]);
  let rest = [];
  while (cQ.length > 0) {
    if (grid[cy][cx] === "#") return;
    if (ins === "v") {
      if (grid[cy][cx] === "[") {
        cQ.push([cy + 1, cx]);
        cQ.push([cy + 1, cx + 1]);
      }
      if (grid[cy][cx] === "]") {
        cQ.push([cy + 1, cx]);
        cQ.push([cy + 1, cx - 1]);
      }
    } else if (ins === "^") {
      if (grid[cy][cx] === "[") {
        cQ.push([cy - 1, cx]);
        cQ.push([cy - 1, cx + 1]);
      }
      if (grid[cy][cx] === "]") {
        cQ.push([cy - 1, cx]);
        cQ.push([cy - 1, cx - 1]);
      }
    } else if (ins === ">") {
      if (grid[cy][cx] === "[") {
        cQ.push([cy, cx + 2]);
      }
      if (grid[cy][cx] === "]") {
        cQ.push([cy, cx + 1]);
      }
    } else if (ins === "<") {
      if (grid[cy][cx] === "[") {
        cQ.push([cy, cx - 1]);
      }
      if (grid[cy][cx] === "]") {
        cQ.push([cy, cx - 2]);
      }
    }
    [cy, cx] = cQ.pop();
    rest.push([cy, cx]);
  }
  console.log(rest);
  const v = structuredClone(grid);
  rest.forEach((r) => (v[r[0]][r[1]] = "x"));
  console.log(v.map((l) => l.join("")).join("\n"));

  // move
  if (ins === "v") {
    const old = structuredClone(grid);
    rest.forEach((r) => {
      if (!rest.some((p) => p[0] === r[0] - 1 && p[1] === r[1])) {
        grid[r[0] - 1][r[1]] = ".";
      }
      grid[r[0]][r[1]] = old[r[0] - 1][r[1]];
    });
  } else if (ins === "^") {
    const old = structuredClone(grid);
    rest.forEach((r) => {
      if (!rest.some((p) => p[0] === r[0] + 1 && p[1] === r[1])) {
        grid[r[0] + 1][r[1]] = ".";
      }
      grid[r[0]][r[1]] = old[r[0] + 1][r[1]];
    });
  } else if (ins === "<") {
    rest.forEach((r) => {
      grid[r[0]][r[1]] = "[";
      grid[r[0]][r[1] + 1] = "]";
    });
  } else if (ins === ">") {
    rest.forEach((r) => {
      grid[r[0]][r[1]] = "]";
      grid[r[0]][r[1] - 1] = "[";
    });
  }
  if (ins === "v") {
    grid[ry + 1][rx] = "@";
  } else if (ins === "^") {
    grid[ry - 1][rx] = "@";
  } else if (ins === ">") {
    grid[ry][rx + 1] = "@";
  } else if (ins === "<") {
    grid[ry][rx - 1] = "@";
  }
  grid[ry][rx] = ".";
}

function part2() {
  const g = resize(grid);
  for (const ins of instructions) {
    console.log(ins);
    console.log(g.map((l) => l.join("")).join("\n"));
    exec2(g, ins);
    console.log("---");
  }
  console.log("FINAL");
  console.log(g.map((l) => l.join("")).join("\n"));

  let total = 0;
  g.forEach((row, rowIndex) => {
    row.forEach((cell, cellIndex) => {
      if (cell === "[") {
        total += rowIndex * 100 + cellIndex;
      }
    });
  });
  console.log(total);
}

part2();
