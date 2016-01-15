
-record(stats, {
    found = 0,
    notfound = 0,
    error = [],
    consequetive_notfound = 0,
    start_time = now(),
    delayed=[]
}).

-record(opt, {
    host :: string(),
    host_original :: string(),
    from :: integer(),
    to :: integer(),
    blobs = yes :: edgesonly | tagsonly | no | yes | blobsonly,
    is_only_authoritative = false :: boolean(),
    is_skip_deleted = false :: boolean(),
    content_group :: integer(),
    secret :: string()
}).

