# ![RealWorld Example App](logo.png)

> ### datomic codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.


### [Demo](https://demo.realworld.io/)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)


This codebase was created to demonstrate a fully fledged fullstack application built with **datomic** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the **datomic** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.


# How it works

Mostly a single namespace: `conduit.server`.

It is short: only ~600LOC.

# Getting started

## Run with docker

;; TODO

## Run on local machine

```shell
clojure -M -m conduit.main
```

## How to test

```shell
clojure -M:dev:test
```
