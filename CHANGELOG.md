# Revision history for `euler-events-hs`


## 2.0.1 -- 2022-07-29

* Added UPDATE_ENROLLMENT mandate flow type.

## 2.0.0 -- 2022-04-25

* BREAKING CHANGES! Drop legacy prometheus metric interface.
  Use new metric instead

## 1.1.0 -- 2022-04-12

* Fix doctest test build for Macbook Intel cpu machines.

## 1.0.0 -- 2022-03-25

* New metric api

## 0.0.8 -- 2022-02-17

* Remove `LogType` type.

## 0.0.7 -- 2022-02-16

* Add `LogType` type.

## 0.0.6 -- 2022-02-01

* Add `LOG_CLASSIFICATION_COUNT` metric key.

## 0.0.5 -- 2022-01-18

* Fixed OrderEvent concat logs.

## 0.0.4 -- 2021-02-26

* Event added
  1. WorkflowEvent
* Added new types for workflow logging.

## 0.0.3 -- 2021-03-03

* Add new metric keys
* Add new types for locker system

## 0.0.1 -- 2020-07-30

Logs events at `stdout` in `json` format.

* Events added
  1. `OrderEvent`
  2. `TxnEvent`
  3. `TxnCardInfoEvent`
* Loggers added
  1. `Stdout`

## 0.0.2 -- 2020-08-10
Adds Prometheus Metrics support

