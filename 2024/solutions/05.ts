import { INPUT_A, INPUT_B } from "@/05";

const test_a = `47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13`;

const test_b = `75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47`;

const rules = INPUT_A.split("\n").map((x) => x.split("|").map((y) => Number(y)));
const lines = INPUT_B.split("\n").map((x) => x.split(",").map((y) => Number(y)));

const passed = [];
for (let line of lines) {
  let failed = false;
  let seen = [];
  for (let i of line) {
    for (let r of rules) {
      if (i === r[0] && seen.includes(r[1])) {
        failed = true;
        break;
      }
      if (failed) break;
    }
    seen.push(i);
    if (failed) break;
  }
  if (!failed) passed.push(line);
}
console.time("Part 1");
console.log(passed.map((x) => x[Math.floor(x.length / 2)]).reduce((a, b) => a + b));
console.timeEnd("Part 1");
console.time("Part 2");
// part 2
const incor = lines.filter((x) => !passed.includes(x));

// horrendously slow "do it enough and it must be right" method, since I only swap it once and some can have multiple mistakes
for (let k = 0; k < 10; k++) {
  for (let line of incor) {
    // let seen = [];
    let seen = new Set<number>();
    for (let i of line) {
      for (let r of rules) {
        if (i === r[0] && seen.has(r[1])) {
          [line[line.indexOf(i)], line[line.indexOf(r[1])]] = [line[line.indexOf(r[1])], line[line.indexOf(i)]];
        }
      }
      seen.add(i);
    }
  }
}

console.log(incor.map((x) => x[Math.floor(x.length / 2)]).reduce((a, b) => a + b));
console.timeEnd("Part 2");
