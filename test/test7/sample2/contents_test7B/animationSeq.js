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
function repeat(fn, interval, Nt)
{
    let t = 0;
    function run()
    {
        if(t == Nt)
        {
            t = 0;
        }
        fn(t);
        t++;
        setTimeout(run, interval);
    }
    run();
}
