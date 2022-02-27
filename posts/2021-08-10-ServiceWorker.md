---
title: Service Worker and Web Workers
tags: prog, cloud, frontend
---

# Quick summary

* Service Worker = Cache
* Web Workers = memoryless Multithread

# Web workers

## self

* `self` in the main thread means the `window` object
* `self` for webworker thread means the ServiceWorkerGlobalScope
<!--  -->
* Webworkers have no access to DOM elements
<!--  -->
* The main thread is the only thread that can create web workers with `const x = new Worker("henchman.js)`

## How to create

* We need to create separate js file for a webworker
* We create a `henchman.js` file for webworker and `self.addEventListener(..)` allows Webworkers to respond to events

## Compared to multiprocessing

* Webworkers have no mutex, semaphore or shared memory
* They listen to events and send messages

# What are Service Worker?

* it's just a cache
  * it's only useful on the 2nd visit of a website

Browser <---> ServiceWorker/Cache <---> 

```js
/* service-worker.js */

// Install 
self.addEventListener('install', function(event) {
    // ...
});

// Activate 
self.addEventListener('activate', function(event) {
    // ...
});

// Listen for network requests from the main document
self.addEventListener('fetch', function(event) {
    // ...
});

```