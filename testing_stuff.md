# Using Antidote test utils

To set up a multi DC test environnement I need:

to call test_utils:set_up_multi_dc()

Config is empty at first, when init per SUITE is called.

Here's what the config looks like: (in the multiple DC node failure)

```erlang
[{clusters,[[dev1@3b80f79a90bc,dev2@3b80f79a90bc],
                [dev3@3b80f79a90bc],
                [dev4@3b80f79a90bc]]},
     {nodes,[dev1@3b80f79a90bc,dev2@3b80f79a90bc]},
     {watchdog,<0.1127.0>},
     {tc_logfile,"/src/antidote/logs/ct_run.ct@3b80f79a90bc.2018-11-12_17.10.35/test.multidc.logs/run.2018-11-12_17.10.39/multiple_dcs_node_failure_suite.update_during_cluster_failure_test.html"},
     {tc_group_properties,[]},
     {tc_group_path,[]},
     {data_dir,"/src/antidote/test/multidc/multiple_dcs_node_failure_SUITE_data/"},
     {priv_dir,"/src/antidote/logs/ct_run.ct@3b80f79a90bc.2018-11-12_17.10.35/test.multidc.logs/run.2018-11-12_17.10.39/log_private/"}]
```

Here's mine:

```erlang
  [{watchdog,<0.1004.0>},
     {tc_logfile,"/src/antidote/logs/ct_run.ct@3b80f79a90bc.2018-11-12_17.10.35/test.multidc.logs/run.2018-11-12_17.10.39/causality_suite.append_failure_test.html"},
     {tc_group_properties,[{suite,causality_SUITE}]},
     {tc_group_path,[]},
     {data_dir,"/src/antidote/test/multidc/causality_SUITE_data/"},
     {priv_dir,"/src/antidote/logs/ct_run.ct@3b80f79a90bc.2018-11-12_17.10.35/test.multidc.logs/run.2018-11-12_17.10.39/log_private/"}]
```


DCs setup

Does __not__ change. 

It's line 438 of the test utils file

You have three DCs with the same partitionning scheme and the same number of partitions.

But the first DC had runs on two machines instead of a single one.

In the file, you have the problem of the DC names being harcoded: For instance, they are given a port number directly into the file. If this should be refactored so that may be changed during the tests or in a config file, you need to take care of this part.
