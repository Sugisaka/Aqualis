const animationResetMap = {
reset0: () => {
    animationSeqResetID0();
    animationSeqResetID1();
},
test: () => {}
};
function resetAll(){
    for (const key in animationResetMap) {
        if (typeof animationResetMap[key] === "function") {
            animationResetMap[key]();
        }
    }
}
