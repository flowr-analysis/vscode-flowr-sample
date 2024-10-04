"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getPositionAt = getPositionAt;
exports.consolidateNewlines = consolidateNewlines;
exports.makeSlicingCriteria = makeSlicingCriteria;
exports.makeSliceElements = makeSliceElements;
function getPositionAt(position, document) {
    const re = /([a-zA-Z0-9._:])+/;
    const wordRange = document.getWordRangeAtPosition(position, re);
    return wordRange;
}
function consolidateNewlines(text) {
    return text.replace(/\r\n/g, '\n');
}
function toSlicingCriterion(pos) {
    return `${pos.line + 1}:${pos.character + 1}`;
}
function makeSlicingCriteria(positions, doc, verbose = true) {
    positions = positions.map(pos => {
        const range = getPositionAt(pos, doc);
        pos = range?.start ?? pos;
        if (verbose) {
            console.log(`Extracting slice at ${pos.line + 1}:${pos.character + 1} in ${doc.fileName}`);
            console.log(`Token: ${doc.getText(range)}`);
        }
        return pos;
    });
    const criteria = positions.map(toSlicingCriterion);
    return criteria;
}
function makeSliceElements(sliceResponse, idToLocation) {
    const sliceElements = [];
    for (const id of sliceResponse) {
        const location = idToLocation(id);
        if (location) {
            sliceElements.push({ id, location });
        }
    }
    // sort by start
    sliceElements.sort((a, b) => {
        return a.location[0] - b.location[0] || a.location[1] - b.location[1];
    });
    return sliceElements;
}
//# sourceMappingURL=utils.js.map