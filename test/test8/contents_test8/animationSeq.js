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
