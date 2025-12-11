import { INPUT } from "@/11";

function main(input: string, depth: number): number {
  let nums = input.split(" ").map((x) => Number(x));
  let map_of_counts = new Map<number, number>();
  nums.forEach((item) => map_of_counts.set(item, 1));
  for (let blink = 0; blink < depth; blink++) {
    let next = new Map<number, number>();
    map_of_counts.forEach((v, k) => {
      if (k === 0) {
        next.set(1, (next.get(1) ?? 0) + v);
      } else if (k.toString().length % 2 === 0) {
        const s = k.toString();
        const k1 = Number(s.slice(0, s.length / 2));
        const k2 = Number(s.slice(s.length / 2));
        next.set(k1, (next.get(k1) ?? 0) + v);
        next.set(k2, (next.get(k2) ?? 0) + v);
      } else {
        next.set(k * 2024, (next.get(k * 2024) ?? 0) + v);
      }
    });
    map_of_counts = next;
  }
  let total = 0;
  map_of_counts.forEach((v) => (total += v));
  return total;
}

console.log(main(INPUT, 25));
console.log(main(INPUT, 75));
