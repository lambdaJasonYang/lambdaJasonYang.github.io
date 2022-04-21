---
title: Kubernetes
tags: tech,mathcs,AI,musings
---

# Setup

* Install minikube which will install kubectl inside
* kubectl is our main function we use 

```bash
minikube kubectl -- verion -- client
#note that MUST be space between -- and flags
```

# Summary

* Container :: Pod :: Node
* $\{Pod...\} \in Node$
* $\{Container...\} \in Pod$
* A Node controls labeled sets of containers called pods.

```bash
kubectl cluster-info
# Kubernetes control plane is running at https://172.17.0.28:8443
# KubeDNS is running at https://172.17.0.28:8443/api/v1/namespaces/kube-system/services/kube-dns:dns/proxy
kubectl get nodes
# NAME       STATUS   ROLES                  AGE    VERSION
# minikube   Ready    control-plane,master   108s   v1.20.2
```

# Deploying and Pods

* Pod = Set of containers that are all under 1 IP address
  * similar to docker-compose in a way


```bash
# deploys blehname to a pod
kubectl create deployment blehname --image=gcr.io/google-samples/kubernetes-bootcamp:v1

```

```bash
kubectl get deployments
```

# Proxy our network into kubectl cluster

```bash
#in another terminal this will run a proxy 
kubectl proxy
```


```bash
curl http://localhost:8001/version
```
