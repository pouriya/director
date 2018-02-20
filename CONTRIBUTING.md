I love pull requests from everyone. But it's good to explain your idea in issues before.  

# Contributing
Fork, then clone the repo:
```sh
~ $ git clone https://github.com/YOUR-USERNAME/director.git && cd director
```

Run tests before any changes and Make sure the tests pass:
```sh
~/director $ make test
/projects/director/rebar3 ct && /projects/director/rebar3 dialyzer
===> Verifying dependencies...
===> Compiling director
===> Running Common Test suites...
%%% director_SUITE ==> 1: OK
%%% director_SUITE ==> 2: OK
%%% director_SUITE ==> 3: OK
%%% director_SUITE ==> 4: OK
%%% director_SUITE ==> 5: OK
%%% director_SUITE ==> 6: OK
%%% director_SUITE ==> 7: OK
%%% director_SUITE ==> 8: OK
%%% director_SUITE ==> 9: OK
%%% director_SUITE ==> 10: OK
%%% director_table_ets_SUITE ==> 1: OK
%%% director_table_ets_SUITE ==> 2: OK
%%% director_table_ets_SUITE ==> 3: OK
%%% director_table_ets_SUITE ==> 4: OK
%%% director_table_ets_SUITE ==> 5: OK
%%% director_table_ets_SUITE ==> 6: OK
%%% director_table_list_SUITE ==> 1: OK
%%% director_table_list_SUITE ==> 2: OK
%%% director_table_list_SUITE ==> 3: OK
%%% director_table_list_SUITE ==> 4: OK
%%% director_table_list_SUITE ==> 5: OK
%%% director_table_list_SUITE ==> 6: OK
%%% director_table_mnesia_SUITE ==> 1: OK
%%% director_table_mnesia_SUITE ==> 2: OK
%%% director_table_mnesia_SUITE ==> 3: OK
%%% director_table_mnesia_SUITE ==> 4: OK
%%% director_table_mnesia_SUITE ==> 5: OK
%%% director_table_mnesia_SUITE ==> 6: OK
All 28 tests passed.
===> Verifying dependencies...
===> Compiling director
===> Dialyzer starting, this may take a while...
===> Updating plt...
===> Resolving files...
===> Checking 163 files in "/projects/director/_build/default/director_19.3_plt"...
===> Doing success typing analysis...
===> Resolving files...
===> Analyzing 7 files with "/projects/director/_build/default/director_19.3_plt"...
~/director $ 
```
Make your changes and run tests. Make sure the tests pass again. Add tests for your changes and again **Make sure the tests pass**. Push to your fork and [submit a pull request](https://github.com/pouriya-jahanbakhsh/director/compare/). At this point you're waiting on me.
