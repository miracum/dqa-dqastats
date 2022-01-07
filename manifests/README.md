# Kubernetes Manifest using Argo Workflows

- [Kubernetes Manifest using Argo Workflows](#kubernetes-manifest-using-argo-workflows)
  - [Background](#background)
  - [How to use](#how-to-use)
  - [Thanks](#thanks)

## Background

The manifest [`dqastats-workflow.yaml`](./dqastats-workflow.yaml) uses [Argo Workflows](https://argoproj.github.io/argo-workflows/) to shedule the dockerized version of DQAstats to run a data quality (DQ) analysis on a regular basis.

## How to use

1. Install [KinD](https://kind.sigs.k8s.io/) (Kubernetes in Docker).
1. Create a local cluster for testing:

   ```sh
   kind create cluster
   ```

1. Install [Argo Workflows](https://argoproj.github.io/argo-workflows/):

   ```sh
   ## Add the HELM repo for Argo:
   helm repo add bitnami https://charts.bitnami.com/bitnami

   ## Install Argo Workflow with own presets:
   helm install argo-wf bitnami/argo-workflows \
       --set server.serviceAccount.name=argo-wf-san
   ```

1. Follow the instructions in the console to obtain the Bearer token, these might be similar to the following:

   ```sh
   ## Note: If you changed the name `arg-wf` of the deployment
   ## in the `helm install ...` command above,
   ## you also need to change it here:
   SECRET=$(kubectl get sa argo-wf-san -o=jsonpath='{.secrets[0].name}')
   ARGO_TOKEN="Bearer $(kubectl get secret $SECRET -o=jsonpath='{.data.token}' | base64 --decode)"
   echo "$ARGO_TOKEN"
   ```

1. Change the manifest [`dqastats-workflow.yaml`](./dqastats-workflow.yaml) to your needs or keep the current one for demo purpose.
1. Send the secret and the workflow to the cluster:

   ```sh
   kubectl apply -f ./manifests/dqastats-secret.yaml
   kubectl apply -f ./manifests/dqastats-workflow.yaml
   ```

## Thanks

:tada: Big thanks to @christian.gulden / [@chgl](https://github.com/chgl/) for all Kubernetes Support! The draft of this "How to ..." section is borrowed from him, originally from here: <https://gitlab.miracum.org/miracum/charts/-/blob/master/README.md>.
