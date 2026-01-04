function repeatSeq(fn, interval, Nt, onComplete)
{
    let t = 0;
    function run()
    {
        if (t < Nt)
        {
            fn(t);
            t++;
            setTimeout(run, interval);
        }
        else
        {
            onComplete();
        }
    }
    run();
}
function animationSeqID0(t){
    var e = document.getElementById("contentsID0");
    var x1 = 350;
    var y1 = 390;
    var x2 = 350+2*t;
    var y2 = 390;
    e.setAttribute("style","visibility: visible; stroke-width: 3px; stroke-dasharray: 4,4; stroke: #000000;");
    e.setAttribute("x1", x1);
    e.setAttribute("y1", y1);
    e.setAttribute("x2", x2);
    e.setAttribute("y2", y2);
}
function animationSeqID1(t){
    var e = document.getElementById("contentsID1");
    var x1 = 548;
    var y1 = 390;
    var x2 = 350+198*Math.cos(0.06346651825433924*t-0.0001);
    var y2 = 390-198*Math.sin(0.06346651825433924*t-0.0001);
    var a1 = 0;
    var a2 = 3.6363636363636362*t;
    var radiusX = 198;
    var radiusY = 198;
    var da = a2 - a1;
    if(da < 0.0) {da = a2 + 360 - a1;}
    var largerOrSmaller = 0;
    if(da > 180.0) {largerOrSmaller = 1;}
    d = "M " + x1 + " " + y1 + " A " + radiusX + " " + radiusY + " 0 " + largerOrSmaller + " 0 " + x2 + " " + y2 ;
    e.setAttribute("style","visibility: visible; stroke-width: 3px; stroke: #000000; fill: none;");
    e.setAttribute("d", d);
}
