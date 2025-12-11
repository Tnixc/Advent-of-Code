import { INPUT } from "@/14";
const t = `p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3`;
const t1 = `p=2,4 v=2,-3`;

const robots = INPUT.split("\n").map((l) => {
  const v = l.split(" ");
  return {
    px: Number(v[0].split(",")[0].split("=")[1]),
    py: Number(v[0].split(",")[1]),
    dx: Number(v[1].split(",")[0].split("=")[1]),
    dy: Number(v[1].split(",")[1]),
  };
});

const h = 103;
const w = 101;

for (let i = 0; i < 10000; i++) {
  robots.forEach((r) => {
    let [ny, nx] = [r.py + r.dy, r.px + r.dx];

    if (ny >= h) ny -= h;
    if (ny < 0) ny += h;

    if (nx >= w) nx -= w;
    if (nx < 0) nx += w;

    r.py = ny;
    r.px = nx;
  });
  const g = [];
  for (let i = 0; i < h; i++) {
    g.push([]);
    for (let k = 0; k < w; k++) {
      g[i].push(0);
    }
  }

  robots.forEach((r) => {
    // console.log(r);
    g[r.py][r.px] = 1;
  });
  // for p2 just run this | grep 111111111
  console.log(
    g
      .map((l) => i + "|" + l.join(""))
      .join("\n")
      .replaceAll("0", "."),
  );
}

let [q1, q2, q3, q4] = [0, 0, 0, 0];

const ax = Math.ceil(w / 2) - 1;
const ay = Math.ceil(h / 2) - 1;
robots.forEach((r) => {
  if (r.py < ay && r.px > ax) q1++;
  if (r.py < ay && r.px < ax) q2++;
  if (r.py > ay && r.px < ax) q3++;
  if (r.py > ay && r.px > ax) q4++;
});

console.log(q1 * q2 * q3 * q4);
