* cl-cache is a new Common Lisp library providing the following features:

An underlying cache API as base abstraction for implementing new synchronous caches.

Implementations of some basic caching strategies

* First-in-first-out (FIFOCache)
* Least-recently-used (LRUCache)
* Least-used (LUCache)
* Time-to-live (TTLCache)
* Naive cache (BasicCache)

Implementation of an efficient buffer replacement policy based on the low inter-reference recency set algorithm (LIRSCache) described in the LIRS paper

Factory functions for each existing cache type

