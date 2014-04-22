
-record(stats, {
    found = 0,
    notfound = 0,
    error = [],
    consequetive_notfound = 0,
    start_time = now(),
    delayed=[]
}).
