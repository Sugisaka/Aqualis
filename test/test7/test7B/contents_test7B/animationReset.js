const animationResetMap = {
test: () => {}
};
function resetAll(){
    for (const key in animationResetMap) {
        if (typeof animationResetMap[key] === "function") {
            animationResetMap[key]();
        }
    }
}
