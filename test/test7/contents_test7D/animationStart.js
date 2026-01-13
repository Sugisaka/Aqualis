const animationStartMap = {
start0: () => {
    repeatSeq(animationSeqID0, 6, 100, () => {
    repeatSeq(animationSeqID1, 6, 100, () => {
    });
    });
},
start1: () => {
    repeat(animationSeqID2, 20, 100);
},
start2: () => {
    repeat(animationSeqID3, 60, 300);
},
test: () => {}
};
