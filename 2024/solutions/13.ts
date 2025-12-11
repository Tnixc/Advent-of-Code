import { INPUT } from "@/13";
const t = `Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279`;
const buttons = INPUT.split("\n\n")
  .map((l) => l.split("\n").map((k) => k.split(":")[1].split(",")))
  .map((k) => {
    return {
      a: Number(k[0][0].split("+")[1]),
      d: Number(k[0][1].split("+")[1]),
      b: Number(k[1][0].split("+")[1]),
      e: Number(k[1][1].split("+")[1]),
      c: Number(k[2][0].split("=")[1]),
      g: Number(k[2][1].split("=")[1]),
    };
  });

// console.log(buttons);
let c1 = 0;
let c2 = 0;

buttons.forEach((k) => {
  let [a, b, c, d, e, g] = [k.a, k.b, k.c, k.d, k.e, k.g];
  let y = (g * a - d * c) / (a * e - b * d);
  let x = (c - b * y) / a;
  c += 10000000000000;
  g += 10000000000000;
  if (Number.isInteger(x) && Number.isInteger(y)) c1 += 3 * x + y;
  y = (g * a - d * c) / (a * e - b * d);
  x = (c - b * y) / a;
  if (Number.isInteger(x) && Number.isInteger(y)) c2 += 3 * x + y;
});

console.log([c1, c2]);
