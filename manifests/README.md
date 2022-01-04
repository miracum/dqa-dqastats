# Kubernetes Manifest using Argo Workflows

- [Kubernetes Manifest using Argo Workflows](#kubernetes-manifest-using-argo-workflows)
  - [Background](#background)
  - [How to use](#how-to-use)
  - [Thanks](#thanks)

## Background

The manifest [`dqastats-workflow.yaml`](./dqastats-workflow.yaml) uses [Argo Workflows](https://argoproj.github.io/argo-workflows/) to shedule the dockerized version of DQAstats to run a data quality (DQ) analysis on a regular basis.

## How to use

1. Create a local cluster pre-configured with Ingress support for testing using [KinD](https://kind.sigs.k8s.io/):

   ```sh
   kind create cluster
   ```

1. Make changes to the manifest [`dqastats-workflow.yaml`](./dqastats-workflow.yaml) to your needs or run the current one for demo purpose.

## Thanks

:tada: Big thanks to @christian.gulden for all Kubernetes Support! The draft of this "How to ..." section is borrowed from him, originally from here: <https://gitlab.miracum.org/miracum/charts/-/blob/master/README.md>.
