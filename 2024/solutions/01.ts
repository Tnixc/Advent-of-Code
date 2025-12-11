import { INPUT } from "@/01";

console.time("");
const sol = INPUT.split("\n")
  .map((x) => x.split("   ").map((x) => Number(x)))
  .reduce(
    (prev, x) => {
      prev[0].push(x[0]);
      prev[1].push(x[1]);
      return prev;
    },
    [[], []] as number[][],
  )
  .map((x) => x.sort());

console.log([
  sol[0].map((x, i) => Math.abs(x - sol[1][i])).reduce((prev, x) => prev + x),
  sol[0].map((x, _) => Math.abs(x * sol[1].filter((k) => k === x).length)).reduce((prev, x) => prev + x),
]);
console.timeEnd("");
