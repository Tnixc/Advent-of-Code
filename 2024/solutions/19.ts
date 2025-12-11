import { INPUT_A, INPUT_B } from "@/19";

const t_a = `r, wr, b, g, bwu, rb, gb, br`;
const t_b = `brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb`;

const pieces = INPUT_A.split(",")
  .map((x) => x.trim())
  .sort((a, b) => b.length - a.length);
const designs = INPUT_B.split("\n");

const cache1 = new Map<string, boolean>();
const cache2 = new Map<string, number>();

function canCompose(word: string): boolean {
  if (cache1.has(word)) return cache1.get(word) as boolean;
  if (word === "") return true;
  for (const p of pieces) {
    const l = p.length;
    if (l > word.length) continue;
    const start = word.substring(0, l);
    const rest = word.substring(l);
    if (start === p && canCompose(rest)) {
      cache1.set(word, true);
      return true;
    }
  }
  cache1.set(word, false);
  return false;
}

function posWays(word: string): number {
  if (cache2.has(word)) return cache2.get(word) as number;
  if (word === "") return 1;
  let nways = 0;
  for (const p of pieces) {
    const l = p.length;
    if (l > word.length) continue;
    const start = word.substring(0, l);
    const rest = word.substring(l);
    if (start === p) nways += posWays(rest);
  }
  cache2.set(word, nways);
  return nways;
}

const res1: boolean[] = [];
const can: string[] = [];
designs.forEach((x) => {
  let t = canCompose(x);
  res1.push(t);
  if (t) can.push(x);
});

console.log(res1.filter((x) => x).length);

const res2: number[] = [];
can.forEach((x) => res2.push(posWays(x)));

console.log(res2.reduce((a, b) => a + b));
