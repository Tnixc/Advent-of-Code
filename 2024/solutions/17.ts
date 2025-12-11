import { INPUT_A, INPUT_B } from "@/17";
const IA = `Register A: 729
Register B: 0
Register C: 0`;
const IB = `Program: 0,1,5,4,3,0`;

let A = Number(IA.split("\n")[0].split(" ").pop());
let B = Number(IA.split("\n")[1].split(" ").pop());
let C = Number(IA.split("\n")[2].split(" ").pop());
let program = IB.split(" ")[1]
  .split(",")
  .map((k) => Number(k));

let output = [];
let halt = false;
let state = { A, B, C, pointer: 0 };

// while (!halt) {
// console.log(state);
// let next = step(state.A, state.B, state.C, state.pointer);
// if (next.step) next.pointer += 2;
// console.log(next.pointer);
// if (next.pointer >= program.length) {
//   console.log("halt");
//   halt = true;
// }
//
// state = {
//   A: next.A,
//   B: next.B,
//   C: next.C,
//   pointer: next.pointer,
// };
// console.log(output);
//
// }
console.log(program);
console.log(state);
let next = step(state.A, state.B, state.C, state.pointer);
state = {
  A: next.A,
  B: next.B,
  C: next.C,
  pointer: next.step ? next.pointer + 2 : next.pointer,
};
console.log(state);
next = step(state.A, state.B, state.C, state.pointer);
state = {
  A: next.A,
  B: next.B,
  C: next.C,
  pointer: next.step ? next.pointer + 2 : next.pointer,
};
console.log(state);
next = step(state.A, state.B, state.C, state.pointer);

state = {
  A: next.A,
  B: next.B,
  C: next.C,
  pointer: next.step ? next.pointer + 2 : next.pointer,
};
console.log(state);

//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
function step(A: number, B: number, C: number, pointer: number) {
  const code = program[pointer] ?? null;
  const operand = program[pointer + 1] ?? null;
  if (code === null || operand === null) {
    halt = true;
    console.log("null");
    return null;
  }

  let step = true;

  const combo = (o: number) => {
    if ([0, 1, 2, 3].includes(o)) return o;
    if (o === 4) return A;
    if (o === 5) return B;
    if (o === 6) return C;
  };

  const literal = <T>(k: T): T => k;

  if (code === 0) {
    const nu = A;
    const de = Math.pow(2, combo(operand));
    const res = Math.floor(nu / de);
    A = res;
  } else if (code === 1) {
    const res = B ^ literal(operand);
    B = res;
  } else if (code === 2) {
    const res = combo(operand) % 8;
    B = res;
  } else if (code === 3) {
    if (A != 0) {
      if (pointer === literal(operand)) step = false;
      pointer = literal(operand);
    }
  } else if (code === 4) {
    const res = B ^ C;
    B = res;
  } else if (code === 5) {
    const res = combo(operand) % 8;
    output.push(res);
  } else if (code === 6) {
    const numerator = A;
    const demoninator = 2 ^ combo(operand);
    const res = Math.floor(numerator / demoninator);
    B = res;
  } else if (code === 7) {
    const numerator = A;
    const demoninator = 2 ^ combo(operand);
    const res = Math.floor(numerator / demoninator);
    C = res;
  }

  return {
    A,
    B,
    C,
    pointer,
    step,
  };
}
