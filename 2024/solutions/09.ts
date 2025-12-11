import { INPUT } from "@/09";
const input = INPUT;

const test = `2333133121414131402`;
const t2 = `2289937361462537565743654392191430243285795130435159294739821058371166572674439596984`; //expect 532512
const t3 = `12345`;

const chars = input.split("");
function p1() {
  let map = [];
  let isFree = false;
  let id = 0;
  for (const char of chars) {
    if (!isFree) {
      for (let j = 0; j < Number(char); j++) {
        map.push(id);
      }
      id++;
    } else {
      for (let j = 0; j < Number(char); j++) {
        map.push(null);
      }
    }
    isFree = !isFree;
  }

  const res = [];
  let itemIndex = map.length - 1;
  for (let i = 0; i <= itemIndex; i++) {
    if (map[i] !== null) {
      res.push(map[i]);
    } else {
      while (itemIndex > i && map[itemIndex] === null) {
        itemIndex--;
      }
      if (itemIndex > i) {
        res.push(map[itemIndex]);
        itemIndex--;
      }
    }
  }

  const v = res;
  let c1 = 0;
  for (let k = 0; k < v.length; k++) {
    c1 = c1 + k * v[k];
  }
  console.log(c1);
}

function p2() {
  const fileSpans = [];
  const freeSpans = [];
  let isFree = false;
  for (const char of chars) {
    if (!isFree) {
      fileSpans.push(Number(char));
    } else {
      freeSpans.push(Number(char));
    }
    isFree = !isFree;
  }
  const disk = []; // null is empty space, expanded representation
  const filePositions = new Map(); // map disk -> id: where it starts
  let fileId = 0;

  // basically rebuild the disk unsorted first
  for (let fileIndex = 0; fileIndex < fileSpans.length; fileIndex++) {
    const startPosition = disk.length;
    // place the current file's blocks (represented by fileid)
    for (let i = 0; i < fileSpans[fileIndex]; i++) {
      disk.push(fileId);
    }
    filePositions.set(fileId, startPosition);
    fileId++;
    // add empty spaces (null) after the file if specified
    if (fileIndex < freeSpans.length) {
      for (let i = 0; i < freeSpans[fileIndex]; i++) {
        disk.push(null);
      }
    }
  }

  for (let currentFile = fileSpans.length - 1; currentFile >= 0; currentFile--) {
    const originalStart = filePositions.get(currentFile);
    let fileSize = fileSpans[currentFile];

    let foundSpaceStart = null; // starting position of potential empty space
    let emptySpaceCount = 0; //  counter for continuous empty blocks

    // Scan disk from beginning up to file's current position looking for continuous empty space large enough for the file
    for (let position = 0; position < originalStart; position++) {
      if (disk[position] === null) {
        // found the start of an empty block
        if (foundSpaceStart === null) {
          // Mark the start of a new empty region
          foundSpaceStart = position;
        }
        emptySpaceCount++;
      } else {
        // empty region interrupted by a file
        foundSpaceStart = null;
        emptySpaceCount = 0;
      }

      // enough continuous empty space to fit the file
      if (emptySpaceCount === fileSize) {
        // move the file to the new position, each item one by one
        for (let offset = 0; offset < fileSize; offset++) {
          disk[foundSpaceStart + offset] = currentFile;
          disk[originalStart + offset] = null;
        }
        break;
      }
    }
  }
  const final = [];
  for (let k = 0; k < disk.length; k++) {
    if (disk[k] > 0) final.push(k * disk[k]);
  }
  console.log(final.reduce((a, b) => a + b));
}
console.time("Part 1");
p1();
console.timeEnd("Part 1");
console.time("Part 2");
p2();
console.timeEnd("Part 2");
