export function encodeTwo(a: number, b: number) {
  return (a << 16) | b;
}

export function decodeTwo(num: number) {
  return [num >> 16, num & 0xffff];
}
