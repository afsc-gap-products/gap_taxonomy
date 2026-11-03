# GAP_TAXONOMY pipeline tests

```
tests/
  run_tests.R                      runner (see below)
  testthat/
    helper-setup.R                 sources code/utils/*, stubbing + Oracle helpers
    test-delta.R                   prepare_delta / clean_for_oracle
    test-sql-builders.R            MERGE / DELETE / INSERT / CREATE TABLE SQL
    test-registry.R                config.R registry + resolve_registry + QA/QC gate
    test-functions.R               name cleaning, active status, synonym codes, reasons
    test-qaqc.R                    the 7 RACE_DATA QA/QC checks (pass + fail cases)
    test-audit-ddl.R               audit table + trigger DDL, offline (mocked)
    test-audit-scratch-oracle.R    audit tables/triggers + full merge cycle in a scratch schema
    test-audit-production.R        READ-ONLY health check of the real audit tables/triggers
    test-pipeline-outputs.R        data contract tests on the objects from steps 01-04
```

```r
source("tests/run_tests.R")
run_gap_tax_tests()                                              # offline, no database
run_gap_tax_tests(oracle = TRUE, scratch_schema = "YOUR_USER")   # + Oracle tests
run_gap_tax_tests(contract = TRUE)                               # after running steps 01-04
```

* Oracle tests need `GAP_TAX_TEST_ORACLE=true` (set by `oracle = TRUE`). Scratch tests also need
  a schema where you can create tables and triggers (`GAP_TAX_TEST_SCHEMA`); they refuse to run in
  GAP_TAXONOMY, RACE_DATA or GAP_PRODUCTS and drop everything they create (`ZZT<random>_*`).
* If a `channel` object exists in your session it is reused; otherwise `oracle_connect()` prompts.
* Requires `testthat` >= 3.2.0 (for `local_mocked_bindings`), `withr`, and the packages in `load_packages.R`.
