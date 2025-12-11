import { INPUT } from "@/02";
console.time("Part 1");
const input = INPUT.split("\n").map((x) =>
  x
    .trim()
    .split(" ")
    .map((y) => Number(y)),
);
let c1 = 0;
for (let report of input) {
  if (isDecreasing(report, false) || isIncreasing(report, false)) c1 += 1;
}
console.log(c1);
console.timeEnd("Part 1");
console.time("Part 2");
let c2 = 0;
for (let report of input) {
  if (isDecreasing(report, true) || isIncreasing(report, true)) c2 += 1;
}

console.log(c2);
console.timeEnd("Part 2");

function isIncreasing(arr: number[], canFail: boolean) {
  let needsFail = false;
  let failIndex = 0;
  for (let i = 0; i < arr.length - 1; i++) {
    if (!(arr[i + 1] - arr[i] === 1 || arr[i + 1] - arr[i] === 2 || arr[i + 1] - arr[i] === 3)) {
      needsFail = true;
      failIndex = i;
      break;
    }
  }
  if (!needsFail) {
    return true;
  }
  if (needsFail && canFail) {
    let hasFail = false;
    if (!isIncreasing(arr.toSpliced(failIndex, 1), false) && !isIncreasing(arr.toSpliced(failIndex + 1, 1), false)) {
      hasFail = true;
    }
    return !hasFail;
  } else {
    return false;
  }
}

function isDecreasing(arr: number[], canFail: boolean) {
  let needsFail = false;
  let failIndex = 0;
  for (let i = 0; i < arr.length - 1; i++) {
    if (!(arr[i] - arr[i + 1] === 1 || arr[i] - arr[i + 1] === 2 || arr[i] - arr[i + 1] === 3)) {
      needsFail = true;
      failIndex = i;
      break;
    }
  }
  if (!needsFail) {
    return true;
  }
  if (needsFail && canFail) {
    let hasFail = false;
    if (!isDecreasing(arr.toSpliced(failIndex, 1), false) && !isDecreasing(arr.toSpliced(failIndex + 1, 1), false)) {
      hasFail = true;
    }
    return !hasFail;
  } else {
    return false;
  }
}
