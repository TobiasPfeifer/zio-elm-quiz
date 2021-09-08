(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

// This currentContext variable will only be used if the makeSlotClass
// function is called, which happens only if this is the first copy of the
// @wry/context package to be imported.
var currentContext = null;
// This unique internal object is used to denote the absence of a value
// for a given Slot, and is never exposed to outside code.
var MISSING_VALUE = {};
var idCounter = 1;
// Although we can't do anything about the cost of duplicated code from
// accidentally bundling multiple copies of the @wry/context package, we can
// avoid creating the Slot class more than once using makeSlotClass.
var makeSlotClass = function () { return /** @class */ (function () {
    function Slot() {
        // If you have a Slot object, you can find out its slot.id, but you cannot
        // guess the slot.id of a Slot you don't have access to, thanks to the
        // randomized suffix.
        this.id = [
            "slot",
            idCounter++,
            Date.now(),
            Math.random().toString(36).slice(2),
        ].join(":");
    }
    Slot.prototype.hasValue = function () {
        for (var context_1 = currentContext; context_1; context_1 = context_1.parent) {
            // We use the Slot object iself as a key to its value, which means the
            // value cannot be obtained without a reference to the Slot object.
            if (this.id in context_1.slots) {
                var value = context_1.slots[this.id];
                if (value === MISSING_VALUE)
                    break;
                if (context_1 !== currentContext) {
                    // Cache the value in currentContext.slots so the next lookup will
                    // be faster. This caching is safe because the tree of contexts and
                    // the values of the slots are logically immutable.
                    currentContext.slots[this.id] = value;
                }
                return true;
            }
        }
        if (currentContext) {
            // If a value was not found for this Slot, it's never going to be found
            // no matter how many times we look it up, so we might as well cache
            // the absence of the value, too.
            currentContext.slots[this.id] = MISSING_VALUE;
        }
        return false;
    };
    Slot.prototype.getValue = function () {
        if (this.hasValue()) {
            return currentContext.slots[this.id];
        }
    };
    Slot.prototype.withValue = function (value, callback, 
    // Given the prevalence of arrow functions, specifying arguments is likely
    // to be much more common than specifying `this`, hence this ordering:
    args, thisArg) {
        var _a;
        var slots = (_a = {
                __proto__: null
            },
            _a[this.id] = value,
            _a);
        var parent = currentContext;
        currentContext = { parent: parent, slots: slots };
        try {
            // Function.prototype.apply allows the arguments array argument to be
            // omitted or undefined, so args! is fine here.
            return callback.apply(thisArg, args);
        }
        finally {
            currentContext = parent;
        }
    };
    // Capture the current context and wrap a callback function so that it
    // reestablishes the captured context when called.
    Slot.bind = function (callback) {
        var context = currentContext;
        return function () {
            var saved = currentContext;
            try {
                currentContext = context;
                return callback.apply(this, arguments);
            }
            finally {
                currentContext = saved;
            }
        };
    };
    // Immediately run a callback function without any captured context.
    Slot.noContext = function (callback, 
    // Given the prevalence of arrow functions, specifying arguments is likely
    // to be much more common than specifying `this`, hence this ordering:
    args, thisArg) {
        if (currentContext) {
            var saved = currentContext;
            try {
                currentContext = null;
                // Function.prototype.apply allows the arguments array argument to be
                // omitted or undefined, so args! is fine here.
                return callback.apply(thisArg, args);
            }
            finally {
                currentContext = saved;
            }
        }
        else {
            return callback.apply(thisArg, args);
        }
    };
    return Slot;
}()); };
// We store a single global implementation of the Slot class as a permanent
// non-enumerable symbol property of the Array constructor. This obfuscation
// does nothing to prevent access to the Slot class, but at least it ensures
// the implementation (i.e. currentContext) cannot be tampered with, and all
// copies of the @wry/context package (hopefully just one) will share the
// same Slot implementation. Since the first copy of the @wry/context package
// to be imported wins, this technique imposes a very high cost for any
// future breaking changes to the Slot class.
var globalKey = "@wry/context:Slot";
var host = Array;
var Slot = host[globalKey] || function () {
    var Slot = makeSlotClass();
    try {
        Object.defineProperty(host, globalKey, {
            value: host[globalKey] = Slot,
            enumerable: false,
            writable: false,
            configurable: false,
        });
    }
    finally {
        return Slot;
    }
}();

var bind = Slot.bind, noContext = Slot.noContext;
function setTimeoutWithContext(callback, delay) {
    return setTimeout(bind(callback), delay);
}
// Turn any generator function into an async function (using yield instead
// of await), with context automatically preserved across yields.
function asyncFromGen(genFn) {
    return function () {
        var gen = genFn.apply(this, arguments);
        var boundNext = bind(gen.next);
        var boundThrow = bind(gen.throw);
        return new Promise(function (resolve, reject) {
            function invoke(method, argument) {
                try {
                    var result = method.call(gen, argument);
                }
                catch (error) {
                    return reject(error);
                }
                var next = result.done ? resolve : invokeNext;
                if (isPromiseLike(result.value)) {
                    result.value.then(next, result.done ? reject : invokeThrow);
                }
                else {
                    next(result.value);
                }
            }
            var invokeNext = function (value) { return invoke(boundNext, value); };
            var invokeThrow = function (error) { return invoke(boundThrow, error); };
            invokeNext();
        });
    };
}
function isPromiseLike(value) {
    return value && typeof value.then === "function";
}
// If you use the fibers npm package to implement coroutines in Node.js,
// you should call this function at least once to ensure context management
// remains coherent across any yields.
var wrappedFibers = [];
function wrapYieldingFiberMethods(Fiber) {
    // There can be only one implementation of Fiber per process, so this array
    // should never grow longer than one element.
    if (wrappedFibers.indexOf(Fiber) < 0) {
        var wrap = function (obj, method) {
            var fn = obj[method];
            obj[method] = function () {
                return noContext(fn, arguments, this);
            };
        };
        // These methods can yield, according to
        // https://github.com/laverdet/node-fibers/blob/ddebed9b8ae3883e57f822e2108e6943e5c8d2a8/fibers.js#L97-L100
        wrap(Fiber, "yield");
        wrap(Fiber.prototype, "run");
        wrap(Fiber.prototype, "throwInto");
        wrappedFibers.push(Fiber);
    }
    return Fiber;
}

exports.Slot = Slot;
exports.asyncFromGen = asyncFromGen;
exports.bind = bind;
exports.noContext = noContext;
exports.setTimeout = setTimeoutWithContext;
exports.wrapYieldingFiberMethods = wrapYieldingFiberMethods;

},{}],2:[function(require,module,exports){
'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

var _a = Object.prototype, toString = _a.toString, hasOwnProperty = _a.hasOwnProperty;
var previousComparisons = new Map();
/**
 * Performs a deep equality check on two JavaScript values, tolerating cycles.
 */
function equal(a, b) {
    try {
        return check(a, b);
    }
    finally {
        previousComparisons.clear();
    }
}
function check(a, b) {
    // If the two values are strictly equal, our job is easy.
    if (a === b) {
        return true;
    }
    // Object.prototype.toString returns a representation of the runtime type of
    // the given value that is considerably more precise than typeof.
    var aTag = toString.call(a);
    var bTag = toString.call(b);
    // If the runtime types of a and b are different, they could maybe be equal
    // under some interpretation of equality, but for simplicity and performance
    // we just return false instead.
    if (aTag !== bTag) {
        return false;
    }
    switch (aTag) {
        case '[object Array]':
            // Arrays are a lot like other objects, but we can cheaply compare their
            // lengths as a short-cut before comparing their elements.
            if (a.length !== b.length)
                return false;
        // Fall through to object case...
        case '[object Object]': {
            if (previouslyCompared(a, b))
                return true;
            var aKeys = Object.keys(a);
            var bKeys = Object.keys(b);
            // If `a` and `b` have a different number of enumerable keys, they
            // must be different.
            var keyCount = aKeys.length;
            if (keyCount !== bKeys.length)
                return false;
            // Now make sure they have the same keys.
            for (var k = 0; k < keyCount; ++k) {
                if (!hasOwnProperty.call(b, aKeys[k])) {
                    return false;
                }
            }
            // Finally, check deep equality of all child properties.
            for (var k = 0; k < keyCount; ++k) {
                var key = aKeys[k];
                if (!check(a[key], b[key])) {
                    return false;
                }
            }
            return true;
        }
        case '[object Error]':
            return a.name === b.name && a.message === b.message;
        case '[object Number]':
            // Handle NaN, which is !== itself.
            if (a !== a)
                return b !== b;
        // Fall through to shared +a === +b case...
        case '[object Boolean]':
        case '[object Date]':
            return +a === +b;
        case '[object RegExp]':
        case '[object String]':
            return a == "" + b;
        case '[object Map]':
        case '[object Set]': {
            if (a.size !== b.size)
                return false;
            if (previouslyCompared(a, b))
                return true;
            var aIterator = a.entries();
            var isMap = aTag === '[object Map]';
            while (true) {
                var info = aIterator.next();
                if (info.done)
                    break;
                // If a instanceof Set, aValue === aKey.
                var _a = info.value, aKey = _a[0], aValue = _a[1];
                // So this works the same way for both Set and Map.
                if (!b.has(aKey)) {
                    return false;
                }
                // However, we care about deep equality of values only when dealing
                // with Map structures.
                if (isMap && !check(aValue, b.get(aKey))) {
                    return false;
                }
            }
            return true;
        }
    }
    // Otherwise the values are not equal.
    return false;
}
function previouslyCompared(a, b) {
    // Though cyclic references can make an object graph appear infinite from the
    // perspective of a depth-first traversal, the graph still contains a finite
    // number of distinct object references. We use the previousComparisons cache
    // to avoid comparing the same pair of object references more than once, which
    // guarantees termination (even if we end up comparing every object in one
    // graph to every object in the other graph, which is extremely unlikely),
    // while still allowing weird isomorphic structures (like rings with different
    // lengths) a chance to pass the equality test.
    var bSet = previousComparisons.get(a);
    if (bSet) {
        // Return true here because we can be sure false will be returned somewhere
        // else if the objects are not equivalent.
        if (bSet.has(b))
            return true;
    }
    else {
        previousComparisons.set(a, bSet = new Set);
    }
    bSet.add(b);
    return false;
}

exports.default = equal;
exports.equal = equal;


},{}],3:[function(require,module,exports){
(function (process){(function (){
exports.__esModule = true;
exports.assertIdValue = assertIdValue;
exports.defaultDataIdFromObject = defaultDataIdFromObject;
exports.defaultNormalizedCacheFactory = defaultNormalizedCacheFactory$1;
exports.enhanceErrorWithDocument = enhanceErrorWithDocument;
exports.WriteError = exports.StoreWriter = exports.StoreReader = exports.ObjectCache = exports.IntrospectionFragmentMatcher = exports.InMemoryCache = exports.HeuristicFragmentMatcher = void 0;

var _tslib = require("tslib");

var _apolloCache = require("apollo-cache");

var _apolloUtilities = require("apollo-utilities");

var _optimism = require("optimism");

var _tsInvariant = require("ts-invariant");

var haveWarned = false;

function shouldWarn() {
  var answer = !haveWarned;

  if (!(0, _apolloUtilities.isTest)()) {
    haveWarned = true;
  }

  return answer;
}

var HeuristicFragmentMatcher = function () {
  function HeuristicFragmentMatcher() {}

  HeuristicFragmentMatcher.prototype.ensureReady = function () {
    return Promise.resolve();
  };

  HeuristicFragmentMatcher.prototype.canBypassInit = function () {
    return true;
  };

  HeuristicFragmentMatcher.prototype.match = function (idValue, typeCondition, context) {
    var obj = context.store.get(idValue.id);
    var isRootQuery = idValue.id === 'ROOT_QUERY';

    if (!obj) {
      return isRootQuery;
    }

    var _a = obj.__typename,
        __typename = _a === void 0 ? isRootQuery && 'Query' : _a;

    if (!__typename) {
      if (shouldWarn()) {
        process.env.NODE_ENV === "production" || _tsInvariant.invariant.warn("You're using fragments in your queries, but either don't have the addTypename:\n  true option set in Apollo Client, or you are trying to write a fragment to the store without the __typename.\n   Please turn on the addTypename option and include __typename when writing fragments so that Apollo Client\n   can accurately match fragments.");
        process.env.NODE_ENV === "production" || _tsInvariant.invariant.warn('Could not find __typename on Fragment ', typeCondition, obj);
        process.env.NODE_ENV === "production" || _tsInvariant.invariant.warn("DEPRECATION WARNING: using fragments without __typename is unsupported behavior " + "and will be removed in future versions of Apollo client. You should fix this and set addTypename to true now.");
      }

      return 'heuristic';
    }

    if (__typename === typeCondition) {
      return true;
    }

    if (shouldWarn()) {
      process.env.NODE_ENV === "production" || _tsInvariant.invariant.error('You are using the simple (heuristic) fragment matcher, but your ' + 'queries contain union or interface types. Apollo Client will not be ' + 'able to accurately map fragments. To make this error go away, use ' + 'the `IntrospectionFragmentMatcher` as described in the docs: ' + 'https://www.apollographql.com/docs/react/advanced/fragments.html#fragment-matcher');
    }

    return 'heuristic';
  };

  return HeuristicFragmentMatcher;
}();

exports.HeuristicFragmentMatcher = HeuristicFragmentMatcher;

var IntrospectionFragmentMatcher = function () {
  function IntrospectionFragmentMatcher(options) {
    if (options && options.introspectionQueryResultData) {
      this.possibleTypesMap = this.parseIntrospectionResult(options.introspectionQueryResultData);
      this.isReady = true;
    } else {
      this.isReady = false;
    }

    this.match = this.match.bind(this);
  }

  IntrospectionFragmentMatcher.prototype.match = function (idValue, typeCondition, context) {
    process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(this.isReady, 1) : (0, _tsInvariant.invariant)(this.isReady, 'FragmentMatcher.match() was called before FragmentMatcher.init()');
    var obj = context.store.get(idValue.id);
    var isRootQuery = idValue.id === 'ROOT_QUERY';

    if (!obj) {
      return isRootQuery;
    }

    var _a = obj.__typename,
        __typename = _a === void 0 ? isRootQuery && 'Query' : _a;

    process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(__typename, 2) : (0, _tsInvariant.invariant)(__typename, "Cannot match fragment because __typename property is missing: " + JSON.stringify(obj));

    if (__typename === typeCondition) {
      return true;
    }

    var implementingTypes = this.possibleTypesMap[typeCondition];

    if (__typename && implementingTypes && implementingTypes.indexOf(__typename) > -1) {
      return true;
    }

    return false;
  };

  IntrospectionFragmentMatcher.prototype.parseIntrospectionResult = function (introspectionResultData) {
    var typeMap = {};

    introspectionResultData.__schema.types.forEach(function (type) {
      if (type.kind === 'UNION' || type.kind === 'INTERFACE') {
        typeMap[type.name] = type.possibleTypes.map(function (implementingType) {
          return implementingType.name;
        });
      }
    });

    return typeMap;
  };

  return IntrospectionFragmentMatcher;
}();

exports.IntrospectionFragmentMatcher = IntrospectionFragmentMatcher;
var hasOwn = Object.prototype.hasOwnProperty;

var DepTrackingCache = function () {
  function DepTrackingCache(data) {
    var _this = this;

    if (data === void 0) {
      data = Object.create(null);
    }

    this.data = data;
    this.depend = (0, _optimism.wrap)(function (dataId) {
      return _this.data[dataId];
    }, {
      disposable: true,
      makeCacheKey: function (dataId) {
        return dataId;
      }
    });
  }

  DepTrackingCache.prototype.toObject = function () {
    return this.data;
  };

  DepTrackingCache.prototype.get = function (dataId) {
    this.depend(dataId);
    return this.data[dataId];
  };

  DepTrackingCache.prototype.set = function (dataId, value) {
    var oldValue = this.data[dataId];

    if (value !== oldValue) {
      this.data[dataId] = value;
      this.depend.dirty(dataId);
    }
  };

  DepTrackingCache.prototype.delete = function (dataId) {
    if (hasOwn.call(this.data, dataId)) {
      delete this.data[dataId];
      this.depend.dirty(dataId);
    }
  };

  DepTrackingCache.prototype.clear = function () {
    this.replace(null);
  };

  DepTrackingCache.prototype.replace = function (newData) {
    var _this = this;

    if (newData) {
      Object.keys(newData).forEach(function (dataId) {
        _this.set(dataId, newData[dataId]);
      });
      Object.keys(this.data).forEach(function (dataId) {
        if (!hasOwn.call(newData, dataId)) {
          _this.delete(dataId);
        }
      });
    } else {
      Object.keys(this.data).forEach(function (dataId) {
        _this.delete(dataId);
      });
    }
  };

  return DepTrackingCache;
}();

function defaultNormalizedCacheFactory(seed) {
  return new DepTrackingCache(seed);
}

var StoreReader = function () {
  function StoreReader(_a) {
    var _this = this;

    var _b = _a === void 0 ? {} : _a,
        _c = _b.cacheKeyRoot,
        cacheKeyRoot = _c === void 0 ? new _optimism.KeyTrie(_apolloUtilities.canUseWeakMap) : _c,
        _d = _b.freezeResults,
        freezeResults = _d === void 0 ? false : _d;

    var _e = this,
        executeStoreQuery = _e.executeStoreQuery,
        executeSelectionSet = _e.executeSelectionSet,
        executeSubSelectedArray = _e.executeSubSelectedArray;

    this.freezeResults = freezeResults;
    this.executeStoreQuery = (0, _optimism.wrap)(function (options) {
      return executeStoreQuery.call(_this, options);
    }, {
      makeCacheKey: function (_a) {
        var query = _a.query,
            rootValue = _a.rootValue,
            contextValue = _a.contextValue,
            variableValues = _a.variableValues,
            fragmentMatcher = _a.fragmentMatcher;

        if (contextValue.store instanceof DepTrackingCache) {
          return cacheKeyRoot.lookup(contextValue.store, query, fragmentMatcher, JSON.stringify(variableValues), rootValue.id);
        }
      }
    });
    this.executeSelectionSet = (0, _optimism.wrap)(function (options) {
      return executeSelectionSet.call(_this, options);
    }, {
      makeCacheKey: function (_a) {
        var selectionSet = _a.selectionSet,
            rootValue = _a.rootValue,
            execContext = _a.execContext;

        if (execContext.contextValue.store instanceof DepTrackingCache) {
          return cacheKeyRoot.lookup(execContext.contextValue.store, selectionSet, execContext.fragmentMatcher, JSON.stringify(execContext.variableValues), rootValue.id);
        }
      }
    });
    this.executeSubSelectedArray = (0, _optimism.wrap)(function (options) {
      return executeSubSelectedArray.call(_this, options);
    }, {
      makeCacheKey: function (_a) {
        var field = _a.field,
            array = _a.array,
            execContext = _a.execContext;

        if (execContext.contextValue.store instanceof DepTrackingCache) {
          return cacheKeyRoot.lookup(execContext.contextValue.store, field, array, JSON.stringify(execContext.variableValues));
        }
      }
    });
  }

  StoreReader.prototype.readQueryFromStore = function (options) {
    return this.diffQueryAgainstStore((0, _tslib.__assign)((0, _tslib.__assign)({}, options), {
      returnPartialData: false
    })).result;
  };

  StoreReader.prototype.diffQueryAgainstStore = function (_a) {
    var store = _a.store,
        query = _a.query,
        variables = _a.variables,
        previousResult = _a.previousResult,
        _b = _a.returnPartialData,
        returnPartialData = _b === void 0 ? true : _b,
        _c = _a.rootId,
        rootId = _c === void 0 ? 'ROOT_QUERY' : _c,
        fragmentMatcherFunction = _a.fragmentMatcherFunction,
        config = _a.config;
    var queryDefinition = (0, _apolloUtilities.getQueryDefinition)(query);
    variables = (0, _apolloUtilities.assign)({}, (0, _apolloUtilities.getDefaultValues)(queryDefinition), variables);
    var context = {
      store: store,
      dataIdFromObject: config && config.dataIdFromObject,
      cacheRedirects: config && config.cacheRedirects || {}
    };
    var execResult = this.executeStoreQuery({
      query: query,
      rootValue: {
        type: 'id',
        id: rootId,
        generated: true,
        typename: 'Query'
      },
      contextValue: context,
      variableValues: variables,
      fragmentMatcher: fragmentMatcherFunction
    });
    var hasMissingFields = execResult.missing && execResult.missing.length > 0;

    if (hasMissingFields && !returnPartialData) {
      execResult.missing.forEach(function (info) {
        if (info.tolerable) return;
        throw process.env.NODE_ENV === "production" ? new _tsInvariant.InvariantError(8) : new _tsInvariant.InvariantError("Can't find field " + info.fieldName + " on object " + JSON.stringify(info.object, null, 2) + ".");
      });
    }

    if (previousResult) {
      if ((0, _apolloUtilities.isEqual)(previousResult, execResult.result)) {
        execResult.result = previousResult;
      }
    }

    return {
      result: execResult.result,
      complete: !hasMissingFields
    };
  };

  StoreReader.prototype.executeStoreQuery = function (_a) {
    var query = _a.query,
        rootValue = _a.rootValue,
        contextValue = _a.contextValue,
        variableValues = _a.variableValues,
        _b = _a.fragmentMatcher,
        fragmentMatcher = _b === void 0 ? defaultFragmentMatcher : _b;
    var mainDefinition = (0, _apolloUtilities.getMainDefinition)(query);
    var fragments = (0, _apolloUtilities.getFragmentDefinitions)(query);
    var fragmentMap = (0, _apolloUtilities.createFragmentMap)(fragments);
    var execContext = {
      query: query,
      fragmentMap: fragmentMap,
      contextValue: contextValue,
      variableValues: variableValues,
      fragmentMatcher: fragmentMatcher
    };
    return this.executeSelectionSet({
      selectionSet: mainDefinition.selectionSet,
      rootValue: rootValue,
      execContext: execContext
    });
  };

  StoreReader.prototype.executeSelectionSet = function (_a) {
    var _this = this;

    var selectionSet = _a.selectionSet,
        rootValue = _a.rootValue,
        execContext = _a.execContext;
    var fragmentMap = execContext.fragmentMap,
        contextValue = execContext.contextValue,
        variables = execContext.variableValues;
    var finalResult = {
      result: null
    };
    var objectsToMerge = [];
    var object = contextValue.store.get(rootValue.id);
    var typename = object && object.__typename || rootValue.id === 'ROOT_QUERY' && 'Query' || void 0;

    function handleMissing(result) {
      var _a;

      if (result.missing) {
        finalResult.missing = finalResult.missing || [];

        (_a = finalResult.missing).push.apply(_a, result.missing);
      }

      return result.result;
    }

    selectionSet.selections.forEach(function (selection) {
      var _a;

      if (!(0, _apolloUtilities.shouldInclude)(selection, variables)) {
        return;
      }

      if ((0, _apolloUtilities.isField)(selection)) {
        var fieldResult = handleMissing(_this.executeField(object, typename, selection, execContext));

        if (typeof fieldResult !== 'undefined') {
          objectsToMerge.push((_a = {}, _a[(0, _apolloUtilities.resultKeyNameFromField)(selection)] = fieldResult, _a));
        }
      } else {
        var fragment = void 0;

        if ((0, _apolloUtilities.isInlineFragment)(selection)) {
          fragment = selection;
        } else {
          fragment = fragmentMap[selection.name.value];

          if (!fragment) {
            throw process.env.NODE_ENV === "production" ? new _tsInvariant.InvariantError(9) : new _tsInvariant.InvariantError("No fragment named " + selection.name.value);
          }
        }

        var typeCondition = fragment.typeCondition && fragment.typeCondition.name.value;
        var match = !typeCondition || execContext.fragmentMatcher(rootValue, typeCondition, contextValue);

        if (match) {
          var fragmentExecResult = _this.executeSelectionSet({
            selectionSet: fragment.selectionSet,
            rootValue: rootValue,
            execContext: execContext
          });

          if (match === 'heuristic' && fragmentExecResult.missing) {
            fragmentExecResult = (0, _tslib.__assign)((0, _tslib.__assign)({}, fragmentExecResult), {
              missing: fragmentExecResult.missing.map(function (info) {
                return (0, _tslib.__assign)((0, _tslib.__assign)({}, info), {
                  tolerable: true
                });
              })
            });
          }

          objectsToMerge.push(handleMissing(fragmentExecResult));
        }
      }
    });
    finalResult.result = (0, _apolloUtilities.mergeDeepArray)(objectsToMerge);

    if (this.freezeResults && process.env.NODE_ENV !== 'production') {
      Object.freeze(finalResult.result);
    }

    return finalResult;
  };

  StoreReader.prototype.executeField = function (object, typename, field, execContext) {
    var variables = execContext.variableValues,
        contextValue = execContext.contextValue;
    var fieldName = field.name.value;
    var args = (0, _apolloUtilities.argumentsObjectFromField)(field, variables);
    var info = {
      resultKey: (0, _apolloUtilities.resultKeyNameFromField)(field),
      directives: (0, _apolloUtilities.getDirectiveInfoFromField)(field, variables)
    };
    var readStoreResult = readStoreResolver(object, typename, fieldName, args, contextValue, info);

    if (Array.isArray(readStoreResult.result)) {
      return this.combineExecResults(readStoreResult, this.executeSubSelectedArray({
        field: field,
        array: readStoreResult.result,
        execContext: execContext
      }));
    }

    if (!field.selectionSet) {
      assertSelectionSetForIdValue(field, readStoreResult.result);

      if (this.freezeResults && process.env.NODE_ENV !== 'production') {
        (0, _apolloUtilities.maybeDeepFreeze)(readStoreResult);
      }

      return readStoreResult;
    }

    if (readStoreResult.result == null) {
      return readStoreResult;
    }

    return this.combineExecResults(readStoreResult, this.executeSelectionSet({
      selectionSet: field.selectionSet,
      rootValue: readStoreResult.result,
      execContext: execContext
    }));
  };

  StoreReader.prototype.combineExecResults = function () {
    var execResults = [];

    for (var _i = 0; _i < arguments.length; _i++) {
      execResults[_i] = arguments[_i];
    }

    var missing;
    execResults.forEach(function (execResult) {
      if (execResult.missing) {
        missing = missing || [];
        missing.push.apply(missing, execResult.missing);
      }
    });
    return {
      result: execResults.pop().result,
      missing: missing
    };
  };

  StoreReader.prototype.executeSubSelectedArray = function (_a) {
    var _this = this;

    var field = _a.field,
        array = _a.array,
        execContext = _a.execContext;
    var missing;

    function handleMissing(childResult) {
      if (childResult.missing) {
        missing = missing || [];
        missing.push.apply(missing, childResult.missing);
      }

      return childResult.result;
    }

    array = array.map(function (item) {
      if (item === null) {
        return null;
      }

      if (Array.isArray(item)) {
        return handleMissing(_this.executeSubSelectedArray({
          field: field,
          array: item,
          execContext: execContext
        }));
      }

      if (field.selectionSet) {
        return handleMissing(_this.executeSelectionSet({
          selectionSet: field.selectionSet,
          rootValue: item,
          execContext: execContext
        }));
      }

      assertSelectionSetForIdValue(field, item);
      return item;
    });

    if (this.freezeResults && process.env.NODE_ENV !== 'production') {
      Object.freeze(array);
    }

    return {
      result: array,
      missing: missing
    };
  };

  return StoreReader;
}();

exports.StoreReader = StoreReader;

function assertSelectionSetForIdValue(field, value) {
  if (!field.selectionSet && (0, _apolloUtilities.isIdValue)(value)) {
    throw process.env.NODE_ENV === "production" ? new _tsInvariant.InvariantError(10) : new _tsInvariant.InvariantError("Missing selection set for object of type " + value.typename + " returned for query field " + field.name.value);
  }
}

function defaultFragmentMatcher() {
  return true;
}

function assertIdValue(idValue) {
  process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)((0, _apolloUtilities.isIdValue)(idValue), 11) : (0, _tsInvariant.invariant)((0, _apolloUtilities.isIdValue)(idValue), "Encountered a sub-selection on the query, but the store doesn't have an object reference. This should never happen during normal use unless you have custom code that is directly manipulating the store; please file an issue.");
}

function readStoreResolver(object, typename, fieldName, args, context, _a) {
  var resultKey = _a.resultKey,
      directives = _a.directives;
  var storeKeyName = fieldName;

  if (args || directives) {
    storeKeyName = (0, _apolloUtilities.getStoreKeyName)(storeKeyName, args, directives);
  }

  var fieldValue = void 0;

  if (object) {
    fieldValue = object[storeKeyName];

    if (typeof fieldValue === 'undefined' && context.cacheRedirects && typeof typename === 'string') {
      var type = context.cacheRedirects[typename];

      if (type) {
        var resolver = type[fieldName];

        if (resolver) {
          fieldValue = resolver(object, args, {
            getCacheKey: function (storeObj) {
              var id = context.dataIdFromObject(storeObj);
              return id && (0, _apolloUtilities.toIdValue)({
                id: id,
                typename: storeObj.__typename
              });
            }
          });
        }
      }
    }
  }

  if (typeof fieldValue === 'undefined') {
    return {
      result: fieldValue,
      missing: [{
        object: object,
        fieldName: storeKeyName,
        tolerable: false
      }]
    };
  }

  if ((0, _apolloUtilities.isJsonValue)(fieldValue)) {
    fieldValue = fieldValue.json;
  }

  return {
    result: fieldValue
  };
}

var ObjectCache = function () {
  function ObjectCache(data) {
    if (data === void 0) {
      data = Object.create(null);
    }

    this.data = data;
  }

  ObjectCache.prototype.toObject = function () {
    return this.data;
  };

  ObjectCache.prototype.get = function (dataId) {
    return this.data[dataId];
  };

  ObjectCache.prototype.set = function (dataId, value) {
    this.data[dataId] = value;
  };

  ObjectCache.prototype.delete = function (dataId) {
    this.data[dataId] = void 0;
  };

  ObjectCache.prototype.clear = function () {
    this.data = Object.create(null);
  };

  ObjectCache.prototype.replace = function (newData) {
    this.data = newData || Object.create(null);
  };

  return ObjectCache;
}();

exports.ObjectCache = ObjectCache;

function defaultNormalizedCacheFactory$1(seed) {
  return new ObjectCache(seed);
}

var WriteError = function (_super) {
  (0, _tslib.__extends)(WriteError, _super);

  function WriteError() {
    var _this = _super !== null && _super.apply(this, arguments) || this;

    _this.type = 'WriteError';
    return _this;
  }

  return WriteError;
}(Error);

exports.WriteError = WriteError;

function enhanceErrorWithDocument(error, document) {
  var enhancedError = new WriteError("Error writing result to store for query:\n " + JSON.stringify(document));
  enhancedError.message += '\n' + error.message;
  enhancedError.stack = error.stack;
  return enhancedError;
}

var StoreWriter = function () {
  function StoreWriter() {}

  StoreWriter.prototype.writeQueryToStore = function (_a) {
    var query = _a.query,
        result = _a.result,
        _b = _a.store,
        store = _b === void 0 ? defaultNormalizedCacheFactory() : _b,
        variables = _a.variables,
        dataIdFromObject = _a.dataIdFromObject,
        fragmentMatcherFunction = _a.fragmentMatcherFunction;
    return this.writeResultToStore({
      dataId: 'ROOT_QUERY',
      result: result,
      document: query,
      store: store,
      variables: variables,
      dataIdFromObject: dataIdFromObject,
      fragmentMatcherFunction: fragmentMatcherFunction
    });
  };

  StoreWriter.prototype.writeResultToStore = function (_a) {
    var dataId = _a.dataId,
        result = _a.result,
        document = _a.document,
        _b = _a.store,
        store = _b === void 0 ? defaultNormalizedCacheFactory() : _b,
        variables = _a.variables,
        dataIdFromObject = _a.dataIdFromObject,
        fragmentMatcherFunction = _a.fragmentMatcherFunction;
    var operationDefinition = (0, _apolloUtilities.getOperationDefinition)(document);

    try {
      return this.writeSelectionSetToStore({
        result: result,
        dataId: dataId,
        selectionSet: operationDefinition.selectionSet,
        context: {
          store: store,
          processedData: {},
          variables: (0, _apolloUtilities.assign)({}, (0, _apolloUtilities.getDefaultValues)(operationDefinition), variables),
          dataIdFromObject: dataIdFromObject,
          fragmentMap: (0, _apolloUtilities.createFragmentMap)((0, _apolloUtilities.getFragmentDefinitions)(document)),
          fragmentMatcherFunction: fragmentMatcherFunction
        }
      });
    } catch (e) {
      throw enhanceErrorWithDocument(e, document);
    }
  };

  StoreWriter.prototype.writeSelectionSetToStore = function (_a) {
    var _this = this;

    var result = _a.result,
        dataId = _a.dataId,
        selectionSet = _a.selectionSet,
        context = _a.context;
    var variables = context.variables,
        store = context.store,
        fragmentMap = context.fragmentMap;
    selectionSet.selections.forEach(function (selection) {
      var _a;

      if (!(0, _apolloUtilities.shouldInclude)(selection, variables)) {
        return;
      }

      if ((0, _apolloUtilities.isField)(selection)) {
        var resultFieldKey = (0, _apolloUtilities.resultKeyNameFromField)(selection);
        var value = result[resultFieldKey];

        if (typeof value !== 'undefined') {
          _this.writeFieldToStore({
            dataId: dataId,
            value: value,
            field: selection,
            context: context
          });
        } else {
          var isDefered = false;
          var isClient = false;

          if (selection.directives && selection.directives.length) {
            isDefered = selection.directives.some(function (directive) {
              return directive.name && directive.name.value === 'defer';
            });
            isClient = selection.directives.some(function (directive) {
              return directive.name && directive.name.value === 'client';
            });
          }

          if (!isDefered && !isClient && context.fragmentMatcherFunction) {
            process.env.NODE_ENV === "production" || _tsInvariant.invariant.warn("Missing field " + resultFieldKey + " in " + JSON.stringify(result, null, 2).substring(0, 100));
          }
        }
      } else {
        var fragment = void 0;

        if ((0, _apolloUtilities.isInlineFragment)(selection)) {
          fragment = selection;
        } else {
          fragment = (fragmentMap || {})[selection.name.value];
          process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(fragment, 3) : (0, _tsInvariant.invariant)(fragment, "No fragment named " + selection.name.value + ".");
        }

        var matches = true;

        if (context.fragmentMatcherFunction && fragment.typeCondition) {
          var id = dataId || 'self';
          var idValue = (0, _apolloUtilities.toIdValue)({
            id: id,
            typename: undefined
          });
          var fakeContext = {
            store: new ObjectCache((_a = {}, _a[id] = result, _a)),
            cacheRedirects: {}
          };
          var match = context.fragmentMatcherFunction(idValue, fragment.typeCondition.name.value, fakeContext);

          if (!(0, _apolloUtilities.isProduction)() && match === 'heuristic') {
            process.env.NODE_ENV === "production" || _tsInvariant.invariant.error('WARNING: heuristic fragment matching going on!');
          }

          matches = !!match;
        }

        if (matches) {
          _this.writeSelectionSetToStore({
            result: result,
            selectionSet: fragment.selectionSet,
            dataId: dataId,
            context: context
          });
        }
      }
    });
    return store;
  };

  StoreWriter.prototype.writeFieldToStore = function (_a) {
    var _b;

    var field = _a.field,
        value = _a.value,
        dataId = _a.dataId,
        context = _a.context;
    var variables = context.variables,
        dataIdFromObject = context.dataIdFromObject,
        store = context.store;
    var storeValue;
    var storeObject;
    var storeFieldName = (0, _apolloUtilities.storeKeyNameFromField)(field, variables);

    if (!field.selectionSet || value === null) {
      storeValue = value != null && typeof value === 'object' ? {
        type: 'json',
        json: value
      } : value;
    } else if (Array.isArray(value)) {
      var generatedId = dataId + "." + storeFieldName;
      storeValue = this.processArrayValue(value, generatedId, field.selectionSet, context);
    } else {
      var valueDataId = dataId + "." + storeFieldName;
      var generated = true;

      if (!isGeneratedId(valueDataId)) {
        valueDataId = '$' + valueDataId;
      }

      if (dataIdFromObject) {
        var semanticId = dataIdFromObject(value);
        process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(!semanticId || !isGeneratedId(semanticId), 4) : (0, _tsInvariant.invariant)(!semanticId || !isGeneratedId(semanticId), 'IDs returned by dataIdFromObject cannot begin with the "$" character.');

        if (semanticId || typeof semanticId === 'number' && semanticId === 0) {
          valueDataId = semanticId;
          generated = false;
        }
      }

      if (!isDataProcessed(valueDataId, field, context.processedData)) {
        this.writeSelectionSetToStore({
          dataId: valueDataId,
          result: value,
          selectionSet: field.selectionSet,
          context: context
        });
      }

      var typename = value.__typename;
      storeValue = (0, _apolloUtilities.toIdValue)({
        id: valueDataId,
        typename: typename
      }, generated);
      storeObject = store.get(dataId);
      var escapedId = storeObject && storeObject[storeFieldName];

      if (escapedId !== storeValue && (0, _apolloUtilities.isIdValue)(escapedId)) {
        var hadTypename = escapedId.typename !== undefined;
        var hasTypename = typename !== undefined;
        var typenameChanged = hadTypename && hasTypename && escapedId.typename !== typename;
        process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(!generated || escapedId.generated || typenameChanged, 5) : (0, _tsInvariant.invariant)(!generated || escapedId.generated || typenameChanged, "Store error: the application attempted to write an object with no provided id but the store already contains an id of " + escapedId.id + " for this object. The selectionSet that was trying to be written is:\n" + JSON.stringify(field));
        process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(!hadTypename || hasTypename, 6) : (0, _tsInvariant.invariant)(!hadTypename || hasTypename, "Store error: the application attempted to write an object with no provided typename but the store already contains an object with typename of " + escapedId.typename + " for the object of id " + escapedId.id + ". The selectionSet that was trying to be written is:\n" + JSON.stringify(field));

        if (escapedId.generated) {
          if (typenameChanged) {
            if (!generated) {
              store.delete(escapedId.id);
            }
          } else {
            mergeWithGenerated(escapedId.id, storeValue.id, store);
          }
        }
      }
    }

    storeObject = store.get(dataId);

    if (!storeObject || !(0, _apolloUtilities.isEqual)(storeValue, storeObject[storeFieldName])) {
      store.set(dataId, (0, _tslib.__assign)((0, _tslib.__assign)({}, storeObject), (_b = {}, _b[storeFieldName] = storeValue, _b)));
    }
  };

  StoreWriter.prototype.processArrayValue = function (value, generatedId, selectionSet, context) {
    var _this = this;

    return value.map(function (item, index) {
      if (item === null) {
        return null;
      }

      var itemDataId = generatedId + "." + index;

      if (Array.isArray(item)) {
        return _this.processArrayValue(item, itemDataId, selectionSet, context);
      }

      var generated = true;

      if (context.dataIdFromObject) {
        var semanticId = context.dataIdFromObject(item);

        if (semanticId) {
          itemDataId = semanticId;
          generated = false;
        }
      }

      if (!isDataProcessed(itemDataId, selectionSet, context.processedData)) {
        _this.writeSelectionSetToStore({
          dataId: itemDataId,
          result: item,
          selectionSet: selectionSet,
          context: context
        });
      }

      return (0, _apolloUtilities.toIdValue)({
        id: itemDataId,
        typename: item.__typename
      }, generated);
    });
  };

  return StoreWriter;
}();

exports.StoreWriter = StoreWriter;

function isGeneratedId(id) {
  return id[0] === '$';
}

function mergeWithGenerated(generatedKey, realKey, cache) {
  if (generatedKey === realKey) {
    return false;
  }

  var generated = cache.get(generatedKey);
  var real = cache.get(realKey);
  var madeChanges = false;
  Object.keys(generated).forEach(function (key) {
    var value = generated[key];
    var realValue = real[key];

    if ((0, _apolloUtilities.isIdValue)(value) && isGeneratedId(value.id) && (0, _apolloUtilities.isIdValue)(realValue) && !(0, _apolloUtilities.isEqual)(value, realValue) && mergeWithGenerated(value.id, realValue.id, cache)) {
      madeChanges = true;
    }
  });
  cache.delete(generatedKey);
  var newRealValue = (0, _tslib.__assign)((0, _tslib.__assign)({}, generated), real);

  if ((0, _apolloUtilities.isEqual)(newRealValue, real)) {
    return madeChanges;
  }

  cache.set(realKey, newRealValue);
  return true;
}

function isDataProcessed(dataId, field, processedData) {
  if (!processedData) {
    return false;
  }

  if (processedData[dataId]) {
    if (processedData[dataId].indexOf(field) >= 0) {
      return true;
    } else {
      processedData[dataId].push(field);
    }
  } else {
    processedData[dataId] = [field];
  }

  return false;
}

var defaultConfig = {
  fragmentMatcher: new HeuristicFragmentMatcher(),
  dataIdFromObject: defaultDataIdFromObject,
  addTypename: true,
  resultCaching: true,
  freezeResults: false
};

function defaultDataIdFromObject(result) {
  if (result.__typename) {
    if (result.id !== undefined) {
      return result.__typename + ":" + result.id;
    }

    if (result._id !== undefined) {
      return result.__typename + ":" + result._id;
    }
  }

  return null;
}

var hasOwn$1 = Object.prototype.hasOwnProperty;

var OptimisticCacheLayer = function (_super) {
  (0, _tslib.__extends)(OptimisticCacheLayer, _super);

  function OptimisticCacheLayer(optimisticId, parent, transaction) {
    var _this = _super.call(this, Object.create(null)) || this;

    _this.optimisticId = optimisticId;
    _this.parent = parent;
    _this.transaction = transaction;
    return _this;
  }

  OptimisticCacheLayer.prototype.toObject = function () {
    return (0, _tslib.__assign)((0, _tslib.__assign)({}, this.parent.toObject()), this.data);
  };

  OptimisticCacheLayer.prototype.get = function (dataId) {
    return hasOwn$1.call(this.data, dataId) ? this.data[dataId] : this.parent.get(dataId);
  };

  return OptimisticCacheLayer;
}(ObjectCache);

var InMemoryCache = function (_super) {
  (0, _tslib.__extends)(InMemoryCache, _super);

  function InMemoryCache(config) {
    if (config === void 0) {
      config = {};
    }

    var _this = _super.call(this) || this;

    _this.watches = new Set();
    _this.typenameDocumentCache = new Map();
    _this.cacheKeyRoot = new _optimism.KeyTrie(_apolloUtilities.canUseWeakMap);
    _this.silenceBroadcast = false;
    _this.config = (0, _tslib.__assign)((0, _tslib.__assign)({}, defaultConfig), config);

    if (_this.config.customResolvers) {
      process.env.NODE_ENV === "production" || _tsInvariant.invariant.warn('customResolvers have been renamed to cacheRedirects. Please update your config as we will be deprecating customResolvers in the next major version.');
      _this.config.cacheRedirects = _this.config.customResolvers;
    }

    if (_this.config.cacheResolvers) {
      process.env.NODE_ENV === "production" || _tsInvariant.invariant.warn('cacheResolvers have been renamed to cacheRedirects. Please update your config as we will be deprecating cacheResolvers in the next major version.');
      _this.config.cacheRedirects = _this.config.cacheResolvers;
    }

    _this.addTypename = !!_this.config.addTypename;
    _this.data = _this.config.resultCaching ? new DepTrackingCache() : new ObjectCache();
    _this.optimisticData = _this.data;
    _this.storeWriter = new StoreWriter();
    _this.storeReader = new StoreReader({
      cacheKeyRoot: _this.cacheKeyRoot,
      freezeResults: config.freezeResults
    });
    var cache = _this;
    var maybeBroadcastWatch = cache.maybeBroadcastWatch;
    _this.maybeBroadcastWatch = (0, _optimism.wrap)(function (c) {
      return maybeBroadcastWatch.call(_this, c);
    }, {
      makeCacheKey: function (c) {
        if (c.optimistic) {
          return;
        }

        if (c.previousResult) {
          return;
        }

        if (cache.data instanceof DepTrackingCache) {
          return cache.cacheKeyRoot.lookup(c.query, JSON.stringify(c.variables));
        }
      }
    });
    return _this;
  }

  InMemoryCache.prototype.restore = function (data) {
    if (data) this.data.replace(data);
    return this;
  };

  InMemoryCache.prototype.extract = function (optimistic) {
    if (optimistic === void 0) {
      optimistic = false;
    }

    return (optimistic ? this.optimisticData : this.data).toObject();
  };

  InMemoryCache.prototype.read = function (options) {
    if (typeof options.rootId === 'string' && typeof this.data.get(options.rootId) === 'undefined') {
      return null;
    }

    var fragmentMatcher = this.config.fragmentMatcher;
    var fragmentMatcherFunction = fragmentMatcher && fragmentMatcher.match;
    return this.storeReader.readQueryFromStore({
      store: options.optimistic ? this.optimisticData : this.data,
      query: this.transformDocument(options.query),
      variables: options.variables,
      rootId: options.rootId,
      fragmentMatcherFunction: fragmentMatcherFunction,
      previousResult: options.previousResult,
      config: this.config
    }) || null;
  };

  InMemoryCache.prototype.write = function (write) {
    var fragmentMatcher = this.config.fragmentMatcher;
    var fragmentMatcherFunction = fragmentMatcher && fragmentMatcher.match;
    this.storeWriter.writeResultToStore({
      dataId: write.dataId,
      result: write.result,
      variables: write.variables,
      document: this.transformDocument(write.query),
      store: this.data,
      dataIdFromObject: this.config.dataIdFromObject,
      fragmentMatcherFunction: fragmentMatcherFunction
    });
    this.broadcastWatches();
  };

  InMemoryCache.prototype.diff = function (query) {
    var fragmentMatcher = this.config.fragmentMatcher;
    var fragmentMatcherFunction = fragmentMatcher && fragmentMatcher.match;
    return this.storeReader.diffQueryAgainstStore({
      store: query.optimistic ? this.optimisticData : this.data,
      query: this.transformDocument(query.query),
      variables: query.variables,
      returnPartialData: query.returnPartialData,
      previousResult: query.previousResult,
      fragmentMatcherFunction: fragmentMatcherFunction,
      config: this.config
    });
  };

  InMemoryCache.prototype.watch = function (watch) {
    var _this = this;

    this.watches.add(watch);
    return function () {
      _this.watches.delete(watch);
    };
  };

  InMemoryCache.prototype.evict = function (query) {
    throw process.env.NODE_ENV === "production" ? new _tsInvariant.InvariantError(7) : new _tsInvariant.InvariantError("eviction is not implemented on InMemory Cache");
  };

  InMemoryCache.prototype.reset = function () {
    this.data.clear();
    this.broadcastWatches();
    return Promise.resolve();
  };

  InMemoryCache.prototype.removeOptimistic = function (idToRemove) {
    var toReapply = [];
    var removedCount = 0;
    var layer = this.optimisticData;

    while (layer instanceof OptimisticCacheLayer) {
      if (layer.optimisticId === idToRemove) {
        ++removedCount;
      } else {
        toReapply.push(layer);
      }

      layer = layer.parent;
    }

    if (removedCount > 0) {
      this.optimisticData = layer;

      while (toReapply.length > 0) {
        var layer_1 = toReapply.pop();
        this.performTransaction(layer_1.transaction, layer_1.optimisticId);
      }

      this.broadcastWatches();
    }
  };

  InMemoryCache.prototype.performTransaction = function (transaction, optimisticId) {
    var _a = this,
        data = _a.data,
        silenceBroadcast = _a.silenceBroadcast;

    this.silenceBroadcast = true;

    if (typeof optimisticId === 'string') {
      this.data = this.optimisticData = new OptimisticCacheLayer(optimisticId, this.optimisticData, transaction);
    }

    try {
      transaction(this);
    } finally {
      this.silenceBroadcast = silenceBroadcast;
      this.data = data;
    }

    this.broadcastWatches();
  };

  InMemoryCache.prototype.recordOptimisticTransaction = function (transaction, id) {
    return this.performTransaction(transaction, id);
  };

  InMemoryCache.prototype.transformDocument = function (document) {
    if (this.addTypename) {
      var result = this.typenameDocumentCache.get(document);

      if (!result) {
        result = (0, _apolloUtilities.addTypenameToDocument)(document);
        this.typenameDocumentCache.set(document, result);
        this.typenameDocumentCache.set(result, result);
      }

      return result;
    }

    return document;
  };

  InMemoryCache.prototype.broadcastWatches = function () {
    var _this = this;

    if (!this.silenceBroadcast) {
      this.watches.forEach(function (c) {
        return _this.maybeBroadcastWatch(c);
      });
    }
  };

  InMemoryCache.prototype.maybeBroadcastWatch = function (c) {
    c.callback(this.diff({
      query: c.query,
      variables: c.variables,
      previousResult: c.previousResult && c.previousResult(),
      optimistic: c.optimistic
    }));
  };

  return InMemoryCache;
}(_apolloCache.ApolloCache); 


exports.InMemoryCache = InMemoryCache;

}).call(this)}).call(this,require('_process'))
},{"_process":146,"apollo-cache":4,"apollo-utilities":11,"optimism":145,"ts-invariant":155,"tslib":156}],4:[function(require,module,exports){
exports.__esModule = true;
exports.Cache = exports.ApolloCache = void 0;

var _apolloUtilities = require("apollo-utilities");

function queryFromPojo(obj) {
  var op = {
    kind: 'OperationDefinition',
    operation: 'query',
    name: {
      kind: 'Name',
      value: 'GeneratedClientQuery'
    },
    selectionSet: selectionSetFromObj(obj)
  };
  var out = {
    kind: 'Document',
    definitions: [op]
  };
  return out;
}

function fragmentFromPojo(obj, typename) {
  var frag = {
    kind: 'FragmentDefinition',
    typeCondition: {
      kind: 'NamedType',
      name: {
        kind: 'Name',
        value: typename || '__FakeType'
      }
    },
    name: {
      kind: 'Name',
      value: 'GeneratedClientQuery'
    },
    selectionSet: selectionSetFromObj(obj)
  };
  var out = {
    kind: 'Document',
    definitions: [frag]
  };
  return out;
}

function selectionSetFromObj(obj) {
  if (typeof obj === 'number' || typeof obj === 'boolean' || typeof obj === 'string' || typeof obj === 'undefined' || obj === null) {
    return null;
  }

  if (Array.isArray(obj)) {
    return selectionSetFromObj(obj[0]);
  }

  var selections = [];
  Object.keys(obj).forEach(function (key) {
    var nestedSelSet = selectionSetFromObj(obj[key]);
    var field = {
      kind: 'Field',
      name: {
        kind: 'Name',
        value: key
      },
      selectionSet: nestedSelSet || undefined
    };
    selections.push(field);
  });
  var selectionSet = {
    kind: 'SelectionSet',
    selections: selections
  };
  return selectionSet;
}

var justTypenameQuery = {
  kind: 'Document',
  definitions: [{
    kind: 'OperationDefinition',
    operation: 'query',
    name: null,
    variableDefinitions: null,
    directives: [],
    selectionSet: {
      kind: 'SelectionSet',
      selections: [{
        kind: 'Field',
        alias: null,
        name: {
          kind: 'Name',
          value: '__typename'
        },
        arguments: [],
        directives: [],
        selectionSet: null
      }]
    }
  }]
};

var ApolloCache = function () {
  function ApolloCache() {}

  ApolloCache.prototype.transformDocument = function (document) {
    return document;
  };

  ApolloCache.prototype.transformForLink = function (document) {
    return document;
  };

  ApolloCache.prototype.readQuery = function (options, optimistic) {
    if (optimistic === void 0) {
      optimistic = false;
    }

    return this.read({
      query: options.query,
      variables: options.variables,
      optimistic: optimistic
    });
  };

  ApolloCache.prototype.readFragment = function (options, optimistic) {
    if (optimistic === void 0) {
      optimistic = false;
    }

    return this.read({
      query: (0, _apolloUtilities.getFragmentQueryDocument)(options.fragment, options.fragmentName),
      variables: options.variables,
      rootId: options.id,
      optimistic: optimistic
    });
  };

  ApolloCache.prototype.writeQuery = function (options) {
    this.write({
      dataId: 'ROOT_QUERY',
      result: options.data,
      query: options.query,
      variables: options.variables
    });
  };

  ApolloCache.prototype.writeFragment = function (options) {
    this.write({
      dataId: options.id,
      result: options.data,
      variables: options.variables,
      query: (0, _apolloUtilities.getFragmentQueryDocument)(options.fragment, options.fragmentName)
    });
  };

  ApolloCache.prototype.writeData = function (_a) {
    var id = _a.id,
        data = _a.data;

    if (typeof id !== 'undefined') {
      var typenameResult = null;

      try {
        typenameResult = this.read({
          rootId: id,
          optimistic: false,
          query: justTypenameQuery
        });
      } catch (e) {}

      var __typename = typenameResult && typenameResult.__typename || '__ClientData';

      var dataToWrite = Object.assign({
        __typename: __typename
      }, data);
      this.writeFragment({
        id: id,
        fragment: fragmentFromPojo(dataToWrite, __typename),
        data: dataToWrite
      });
    } else {
      this.writeQuery({
        query: queryFromPojo(data),
        data: data
      });
    }
  };

  return ApolloCache;
}();

exports.ApolloCache = ApolloCache;
var Cache;
exports.Cache = Cache;

(function (Cache) {})(Cache || (exports.Cache = Cache = {}));

},{"apollo-utilities":11}],5:[function(require,module,exports){
(function (process){(function (){
(function (global, factory) {
  if (typeof define === "function" && define.amd) {
    define(["exports", "tslib", "apollo-utilities", "apollo-link", "symbol-observable", "ts-invariant", "graphql/language/visitor"], factory);
  } else if (typeof exports !== "undefined") {
    factory(exports, require("tslib"), require("apollo-utilities"), require("apollo-link"), require("symbol-observable"), require("ts-invariant"), require("graphql/language/visitor"));
  } else {
    var mod = {
      exports: {}
    };
    factory(mod.exports, global.tslib, global.apolloUtilities, global.apolloLink, global.symbolObservable, global.tsInvariant, global.visitor);
    global.unknown = mod.exports;
  }
})(typeof globalThis !== "undefined" ? globalThis : typeof self !== "undefined" ? self : this, function (_exports, _tslib, _apolloUtilities, _apolloLink, _symbolObservable, _tsInvariant, _visitor) {

  _exports.__esModule = true;
  _exports.isApolloError = isApolloError;
  _exports.ObservableQuery = _exports.NetworkStatus = _exports.FetchType = _exports.ApolloError = _exports.ApolloClient = _exports.default = void 0;
  _symbolObservable = _interopRequireDefault(_symbolObservable);

  function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

  var NetworkStatus;
  _exports.NetworkStatus = NetworkStatus;

  (function (NetworkStatus) {
    NetworkStatus[NetworkStatus["loading"] = 1] = "loading";
    NetworkStatus[NetworkStatus["setVariables"] = 2] = "setVariables";
    NetworkStatus[NetworkStatus["fetchMore"] = 3] = "fetchMore";
    NetworkStatus[NetworkStatus["refetch"] = 4] = "refetch";
    NetworkStatus[NetworkStatus["poll"] = 6] = "poll";
    NetworkStatus[NetworkStatus["ready"] = 7] = "ready";
    NetworkStatus[NetworkStatus["error"] = 8] = "error";
  })(NetworkStatus || (_exports.NetworkStatus = NetworkStatus = {}));

  function isNetworkRequestInFlight(networkStatus) {
    return networkStatus < 7;
  }

  var Observable = function (_super) {
    (0, _tslib.__extends)(Observable, _super);

    function Observable() {
      return _super !== null && _super.apply(this, arguments) || this;
    }

    Observable.prototype[_symbolObservable.default] = function () {
      return this;
    };

    Observable.prototype['@@observable'] = function () {
      return this;
    };

    return Observable;
  }(_apolloLink.Observable);

  function isNonEmptyArray(value) {
    return Array.isArray(value) && value.length > 0;
  }

  function isApolloError(err) {
    return err.hasOwnProperty('graphQLErrors');
  }

  var generateErrorMessage = function (err) {
    var message = '';

    if (isNonEmptyArray(err.graphQLErrors)) {
      err.graphQLErrors.forEach(function (graphQLError) {
        var errorMessage = graphQLError ? graphQLError.message : 'Error message not found.';
        message += "GraphQL error: " + errorMessage + "\n";
      });
    }

    if (err.networkError) {
      message += 'Network error: ' + err.networkError.message + '\n';
    }

    message = message.replace(/\n$/, '');
    return message;
  };

  var ApolloError = function (_super) {
    (0, _tslib.__extends)(ApolloError, _super);

    function ApolloError(_a) {
      var graphQLErrors = _a.graphQLErrors,
          networkError = _a.networkError,
          errorMessage = _a.errorMessage,
          extraInfo = _a.extraInfo;

      var _this = _super.call(this, errorMessage) || this;

      _this.graphQLErrors = graphQLErrors || [];
      _this.networkError = networkError || null;

      if (!errorMessage) {
        _this.message = generateErrorMessage(_this);
      } else {
        _this.message = errorMessage;
      }

      _this.extraInfo = extraInfo;
      _this.__proto__ = ApolloError.prototype;
      return _this;
    }

    return ApolloError;
  }(Error);

  _exports.ApolloError = ApolloError;
  var FetchType;
  _exports.FetchType = FetchType;

  (function (FetchType) {
    FetchType[FetchType["normal"] = 1] = "normal";
    FetchType[FetchType["refetch"] = 2] = "refetch";
    FetchType[FetchType["poll"] = 3] = "poll";
  })(FetchType || (_exports.FetchType = FetchType = {}));

  var hasError = function (storeValue, policy) {
    if (policy === void 0) {
      policy = 'none';
    }

    return storeValue && (storeValue.networkError || policy === 'none' && isNonEmptyArray(storeValue.graphQLErrors));
  };

  var ObservableQuery = function (_super) {
    (0, _tslib.__extends)(ObservableQuery, _super);

    function ObservableQuery(_a) {
      var queryManager = _a.queryManager,
          options = _a.options,
          _b = _a.shouldSubscribe,
          shouldSubscribe = _b === void 0 ? true : _b;

      var _this = _super.call(this, function (observer) {
        return _this.onSubscribe(observer);
      }) || this;

      _this.observers = new Set();
      _this.subscriptions = new Set();
      _this.isTornDown = false;
      _this.options = options;
      _this.variables = options.variables || {};
      _this.queryId = queryManager.generateQueryId();
      _this.shouldSubscribe = shouldSubscribe;
      var opDef = (0, _apolloUtilities.getOperationDefinition)(options.query);
      _this.queryName = opDef && opDef.name && opDef.name.value;
      _this.queryManager = queryManager;
      return _this;
    }

    ObservableQuery.prototype.result = function () {
      var _this = this;

      return new Promise(function (resolve, reject) {
        var observer = {
          next: function (result) {
            resolve(result);

            _this.observers.delete(observer);

            if (!_this.observers.size) {
              _this.queryManager.removeQuery(_this.queryId);
            }

            setTimeout(function () {
              subscription.unsubscribe();
            }, 0);
          },
          error: reject
        };

        var subscription = _this.subscribe(observer);
      });
    };

    ObservableQuery.prototype.currentResult = function () {
      var result = this.getCurrentResult();

      if (result.data === undefined) {
        result.data = {};
      }

      return result;
    };

    ObservableQuery.prototype.getCurrentResult = function () {
      if (this.isTornDown) {
        var lastResult = this.lastResult;
        return {
          data: !this.lastError && lastResult && lastResult.data || void 0,
          error: this.lastError,
          loading: false,
          networkStatus: NetworkStatus.error
        };
      }

      var _a = this.queryManager.getCurrentQueryResult(this),
          data = _a.data,
          partial = _a.partial;

      var queryStoreValue = this.queryManager.queryStore.get(this.queryId);
      var result;
      var fetchPolicy = this.options.fetchPolicy;
      var isNetworkFetchPolicy = fetchPolicy === 'network-only' || fetchPolicy === 'no-cache';

      if (queryStoreValue) {
        var networkStatus = queryStoreValue.networkStatus;

        if (hasError(queryStoreValue, this.options.errorPolicy)) {
          return {
            data: void 0,
            loading: false,
            networkStatus: networkStatus,
            error: new ApolloError({
              graphQLErrors: queryStoreValue.graphQLErrors,
              networkError: queryStoreValue.networkError
            })
          };
        }

        if (queryStoreValue.variables) {
          this.options.variables = (0, _tslib.__assign)((0, _tslib.__assign)({}, this.options.variables), queryStoreValue.variables);
          this.variables = this.options.variables;
        }

        result = {
          data: data,
          loading: isNetworkRequestInFlight(networkStatus),
          networkStatus: networkStatus
        };

        if (queryStoreValue.graphQLErrors && this.options.errorPolicy === 'all') {
          result.errors = queryStoreValue.graphQLErrors;
        }
      } else {
        var loading = isNetworkFetchPolicy || partial && fetchPolicy !== 'cache-only';
        result = {
          data: data,
          loading: loading,
          networkStatus: loading ? NetworkStatus.loading : NetworkStatus.ready
        };
      }

      if (!partial) {
        this.updateLastResult((0, _tslib.__assign)((0, _tslib.__assign)({}, result), {
          stale: false
        }));
      }

      return (0, _tslib.__assign)((0, _tslib.__assign)({}, result), {
        partial: partial
      });
    };

    ObservableQuery.prototype.isDifferentFromLastResult = function (newResult) {
      var snapshot = this.lastResultSnapshot;
      return !(snapshot && newResult && snapshot.networkStatus === newResult.networkStatus && snapshot.stale === newResult.stale && (0, _apolloUtilities.isEqual)(snapshot.data, newResult.data));
    };

    ObservableQuery.prototype.getLastResult = function () {
      return this.lastResult;
    };

    ObservableQuery.prototype.getLastError = function () {
      return this.lastError;
    };

    ObservableQuery.prototype.resetLastResults = function () {
      delete this.lastResult;
      delete this.lastResultSnapshot;
      delete this.lastError;
      this.isTornDown = false;
    };

    ObservableQuery.prototype.resetQueryStoreErrors = function () {
      var queryStore = this.queryManager.queryStore.get(this.queryId);

      if (queryStore) {
        queryStore.networkError = null;
        queryStore.graphQLErrors = [];
      }
    };

    ObservableQuery.prototype.refetch = function (variables) {
      var fetchPolicy = this.options.fetchPolicy;

      if (fetchPolicy === 'cache-only') {
        return Promise.reject(process.env.NODE_ENV === "production" ? new _tsInvariant.InvariantError(1) : new _tsInvariant.InvariantError('cache-only fetchPolicy option should not be used together with query refetch.'));
      }

      if (fetchPolicy !== 'no-cache' && fetchPolicy !== 'cache-and-network') {
        fetchPolicy = 'network-only';
      }

      if (!(0, _apolloUtilities.isEqual)(this.variables, variables)) {
        this.variables = (0, _tslib.__assign)((0, _tslib.__assign)({}, this.variables), variables);
      }

      if (!(0, _apolloUtilities.isEqual)(this.options.variables, this.variables)) {
        this.options.variables = (0, _tslib.__assign)((0, _tslib.__assign)({}, this.options.variables), this.variables);
      }

      return this.queryManager.fetchQuery(this.queryId, (0, _tslib.__assign)((0, _tslib.__assign)({}, this.options), {
        fetchPolicy: fetchPolicy
      }), FetchType.refetch);
    };

    ObservableQuery.prototype.fetchMore = function (fetchMoreOptions) {
      var _this = this;

      process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(fetchMoreOptions.updateQuery, 2) : (0, _tsInvariant.invariant)(fetchMoreOptions.updateQuery, 'updateQuery option is required. This function defines how to update the query data with the new results.');
      var combinedOptions = (0, _tslib.__assign)((0, _tslib.__assign)({}, fetchMoreOptions.query ? fetchMoreOptions : (0, _tslib.__assign)((0, _tslib.__assign)((0, _tslib.__assign)({}, this.options), fetchMoreOptions), {
        variables: (0, _tslib.__assign)((0, _tslib.__assign)({}, this.variables), fetchMoreOptions.variables)
      })), {
        fetchPolicy: 'network-only'
      });
      var qid = this.queryManager.generateQueryId();
      return this.queryManager.fetchQuery(qid, combinedOptions, FetchType.normal, this.queryId).then(function (fetchMoreResult) {
        _this.updateQuery(function (previousResult) {
          return fetchMoreOptions.updateQuery(previousResult, {
            fetchMoreResult: fetchMoreResult.data,
            variables: combinedOptions.variables
          });
        });

        _this.queryManager.stopQuery(qid);

        return fetchMoreResult;
      }, function (error) {
        _this.queryManager.stopQuery(qid);

        throw error;
      });
    };

    ObservableQuery.prototype.subscribeToMore = function (options) {
      var _this = this;

      var subscription = this.queryManager.startGraphQLSubscription({
        query: options.document,
        variables: options.variables
      }).subscribe({
        next: function (subscriptionData) {
          var updateQuery = options.updateQuery;

          if (updateQuery) {
            _this.updateQuery(function (previous, _a) {
              var variables = _a.variables;
              return updateQuery(previous, {
                subscriptionData: subscriptionData,
                variables: variables
              });
            });
          }
        },
        error: function (err) {
          if (options.onError) {
            options.onError(err);
            return;
          }

          process.env.NODE_ENV === "production" || _tsInvariant.invariant.error('Unhandled GraphQL subscription error', err);
        }
      });
      this.subscriptions.add(subscription);
      return function () {
        if (_this.subscriptions.delete(subscription)) {
          subscription.unsubscribe();
        }
      };
    };

    ObservableQuery.prototype.setOptions = function (opts) {
      var oldFetchPolicy = this.options.fetchPolicy;
      this.options = (0, _tslib.__assign)((0, _tslib.__assign)({}, this.options), opts);

      if (opts.pollInterval) {
        this.startPolling(opts.pollInterval);
      } else if (opts.pollInterval === 0) {
        this.stopPolling();
      }

      var fetchPolicy = opts.fetchPolicy;
      return this.setVariables(this.options.variables, oldFetchPolicy !== fetchPolicy && (oldFetchPolicy === 'cache-only' || oldFetchPolicy === 'standby' || fetchPolicy === 'network-only'), opts.fetchResults);
    };

    ObservableQuery.prototype.setVariables = function (variables, tryFetch, fetchResults) {
      if (tryFetch === void 0) {
        tryFetch = false;
      }

      if (fetchResults === void 0) {
        fetchResults = true;
      }

      this.isTornDown = false;
      variables = variables || this.variables;

      if (!tryFetch && (0, _apolloUtilities.isEqual)(variables, this.variables)) {
        return this.observers.size && fetchResults ? this.result() : Promise.resolve();
      }

      this.variables = this.options.variables = variables;

      if (!this.observers.size) {
        return Promise.resolve();
      }

      return this.queryManager.fetchQuery(this.queryId, this.options);
    };

    ObservableQuery.prototype.updateQuery = function (mapFn) {
      var queryManager = this.queryManager;

      var _a = queryManager.getQueryWithPreviousResult(this.queryId),
          previousResult = _a.previousResult,
          variables = _a.variables,
          document = _a.document;

      var newResult = (0, _apolloUtilities.tryFunctionOrLogError)(function () {
        return mapFn(previousResult, {
          variables: variables
        });
      });

      if (newResult) {
        queryManager.dataStore.markUpdateQueryResult(document, variables, newResult);
        queryManager.broadcastQueries();
      }
    };

    ObservableQuery.prototype.stopPolling = function () {
      this.queryManager.stopPollingQuery(this.queryId);
      this.options.pollInterval = undefined;
    };

    ObservableQuery.prototype.startPolling = function (pollInterval) {
      assertNotCacheFirstOrOnly(this);
      this.options.pollInterval = pollInterval;
      this.queryManager.startPollingQuery(this.options, this.queryId);
    };

    ObservableQuery.prototype.updateLastResult = function (newResult) {
      var previousResult = this.lastResult;
      this.lastResult = newResult;
      this.lastResultSnapshot = this.queryManager.assumeImmutableResults ? newResult : (0, _apolloUtilities.cloneDeep)(newResult);
      return previousResult;
    };

    ObservableQuery.prototype.onSubscribe = function (observer) {
      var _this = this;

      try {
        var subObserver = observer._subscription._observer;

        if (subObserver && !subObserver.error) {
          subObserver.error = defaultSubscriptionObserverErrorCallback;
        }
      } catch (_a) {}

      var first = !this.observers.size;
      this.observers.add(observer);
      if (observer.next && this.lastResult) observer.next(this.lastResult);
      if (observer.error && this.lastError) observer.error(this.lastError);

      if (first) {
        this.setUpQuery();
      }

      return function () {
        if (_this.observers.delete(observer) && !_this.observers.size) {
          _this.tearDownQuery();
        }
      };
    };

    ObservableQuery.prototype.setUpQuery = function () {
      var _this = this;

      var _a = this,
          queryManager = _a.queryManager,
          queryId = _a.queryId;

      if (this.shouldSubscribe) {
        queryManager.addObservableQuery(queryId, this);
      }

      if (this.options.pollInterval) {
        assertNotCacheFirstOrOnly(this);
        queryManager.startPollingQuery(this.options, queryId);
      }

      var onError = function (error) {
        _this.updateLastResult((0, _tslib.__assign)((0, _tslib.__assign)({}, _this.lastResult), {
          errors: error.graphQLErrors,
          networkStatus: NetworkStatus.error,
          loading: false
        }));

        iterateObserversSafely(_this.observers, 'error', _this.lastError = error);
      };

      queryManager.observeQuery(queryId, this.options, {
        next: function (result) {
          if (_this.lastError || _this.isDifferentFromLastResult(result)) {
            var previousResult_1 = _this.updateLastResult(result);

            var _a = _this.options,
                query_1 = _a.query,
                variables = _a.variables,
                fetchPolicy_1 = _a.fetchPolicy;

            if (queryManager.transform(query_1).hasClientExports) {
              queryManager.getLocalState().addExportedVariables(query_1, variables).then(function (variables) {
                var previousVariables = _this.variables;
                _this.variables = _this.options.variables = variables;

                if (!result.loading && previousResult_1 && fetchPolicy_1 !== 'cache-only' && queryManager.transform(query_1).serverQuery && !(0, _apolloUtilities.isEqual)(previousVariables, variables)) {
                  _this.refetch();
                } else {
                  iterateObserversSafely(_this.observers, 'next', result);
                }
              });
            } else {
              iterateObserversSafely(_this.observers, 'next', result);
            }
          }
        },
        error: onError
      }).catch(onError);
    };

    ObservableQuery.prototype.tearDownQuery = function () {
      var queryManager = this.queryManager;
      this.isTornDown = true;
      queryManager.stopPollingQuery(this.queryId);
      this.subscriptions.forEach(function (sub) {
        return sub.unsubscribe();
      });
      this.subscriptions.clear();
      queryManager.removeObservableQuery(this.queryId);
      queryManager.stopQuery(this.queryId);
      this.observers.clear();
    };

    return ObservableQuery;
  }(Observable);

  _exports.ObservableQuery = ObservableQuery;

  function defaultSubscriptionObserverErrorCallback(error) {
    process.env.NODE_ENV === "production" || _tsInvariant.invariant.error('Unhandled error', error.message, error.stack);
  }

  function iterateObserversSafely(observers, method, argument) {
    var observersWithMethod = [];
    observers.forEach(function (obs) {
      return obs[method] && observersWithMethod.push(obs);
    });
    observersWithMethod.forEach(function (obs) {
      return obs[method](argument);
    });
  }

  function assertNotCacheFirstOrOnly(obsQuery) {
    var fetchPolicy = obsQuery.options.fetchPolicy;
    process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(fetchPolicy !== 'cache-first' && fetchPolicy !== 'cache-only', 3) : (0, _tsInvariant.invariant)(fetchPolicy !== 'cache-first' && fetchPolicy !== 'cache-only', 'Queries that specify the cache-first and cache-only fetchPolicies cannot also be polling queries.');
  }

  var MutationStore = function () {
    function MutationStore() {
      this.store = {};
    }

    MutationStore.prototype.getStore = function () {
      return this.store;
    };

    MutationStore.prototype.get = function (mutationId) {
      return this.store[mutationId];
    };

    MutationStore.prototype.initMutation = function (mutationId, mutation, variables) {
      this.store[mutationId] = {
        mutation: mutation,
        variables: variables || {},
        loading: true,
        error: null
      };
    };

    MutationStore.prototype.markMutationError = function (mutationId, error) {
      var mutation = this.store[mutationId];

      if (mutation) {
        mutation.loading = false;
        mutation.error = error;
      }
    };

    MutationStore.prototype.markMutationResult = function (mutationId) {
      var mutation = this.store[mutationId];

      if (mutation) {
        mutation.loading = false;
        mutation.error = null;
      }
    };

    MutationStore.prototype.reset = function () {
      this.store = {};
    };

    return MutationStore;
  }();

  var QueryStore = function () {
    function QueryStore() {
      this.store = {};
    }

    QueryStore.prototype.getStore = function () {
      return this.store;
    };

    QueryStore.prototype.get = function (queryId) {
      return this.store[queryId];
    };

    QueryStore.prototype.initQuery = function (query) {
      var previousQuery = this.store[query.queryId];
      process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(!previousQuery || previousQuery.document === query.document || (0, _apolloUtilities.isEqual)(previousQuery.document, query.document), 19) : (0, _tsInvariant.invariant)(!previousQuery || previousQuery.document === query.document || (0, _apolloUtilities.isEqual)(previousQuery.document, query.document), 'Internal Error: may not update existing query string in store');
      var isSetVariables = false;
      var previousVariables = null;

      if (query.storePreviousVariables && previousQuery && previousQuery.networkStatus !== NetworkStatus.loading) {
        if (!(0, _apolloUtilities.isEqual)(previousQuery.variables, query.variables)) {
          isSetVariables = true;
          previousVariables = previousQuery.variables;
        }
      }

      var networkStatus;

      if (isSetVariables) {
        networkStatus = NetworkStatus.setVariables;
      } else if (query.isPoll) {
        networkStatus = NetworkStatus.poll;
      } else if (query.isRefetch) {
        networkStatus = NetworkStatus.refetch;
      } else {
        networkStatus = NetworkStatus.loading;
      }

      var graphQLErrors = [];

      if (previousQuery && previousQuery.graphQLErrors) {
        graphQLErrors = previousQuery.graphQLErrors;
      }

      this.store[query.queryId] = {
        document: query.document,
        variables: query.variables,
        previousVariables: previousVariables,
        networkError: null,
        graphQLErrors: graphQLErrors,
        networkStatus: networkStatus,
        metadata: query.metadata
      };

      if (typeof query.fetchMoreForQueryId === 'string' && this.store[query.fetchMoreForQueryId]) {
        this.store[query.fetchMoreForQueryId].networkStatus = NetworkStatus.fetchMore;
      }
    };

    QueryStore.prototype.markQueryResult = function (queryId, result, fetchMoreForQueryId) {
      if (!this.store || !this.store[queryId]) return;
      this.store[queryId].networkError = null;
      this.store[queryId].graphQLErrors = isNonEmptyArray(result.errors) ? result.errors : [];
      this.store[queryId].previousVariables = null;
      this.store[queryId].networkStatus = NetworkStatus.ready;

      if (typeof fetchMoreForQueryId === 'string' && this.store[fetchMoreForQueryId]) {
        this.store[fetchMoreForQueryId].networkStatus = NetworkStatus.ready;
      }
    };

    QueryStore.prototype.markQueryError = function (queryId, error, fetchMoreForQueryId) {
      if (!this.store || !this.store[queryId]) return;
      this.store[queryId].networkError = error;
      this.store[queryId].networkStatus = NetworkStatus.error;

      if (typeof fetchMoreForQueryId === 'string') {
        this.markQueryResultClient(fetchMoreForQueryId, true);
      }
    };

    QueryStore.prototype.markQueryResultClient = function (queryId, complete) {
      var storeValue = this.store && this.store[queryId];

      if (storeValue) {
        storeValue.networkError = null;
        storeValue.previousVariables = null;

        if (complete) {
          storeValue.networkStatus = NetworkStatus.ready;
        }
      }
    };

    QueryStore.prototype.stopQuery = function (queryId) {
      delete this.store[queryId];
    };

    QueryStore.prototype.reset = function (observableQueryIds) {
      var _this = this;

      Object.keys(this.store).forEach(function (queryId) {
        if (observableQueryIds.indexOf(queryId) < 0) {
          _this.stopQuery(queryId);
        } else {
          _this.store[queryId].networkStatus = NetworkStatus.loading;
        }
      });
    };

    return QueryStore;
  }();

  function capitalizeFirstLetter(str) {
    return str.charAt(0).toUpperCase() + str.slice(1);
  }

  var LocalState = function () {
    function LocalState(_a) {
      var cache = _a.cache,
          client = _a.client,
          resolvers = _a.resolvers,
          fragmentMatcher = _a.fragmentMatcher;
      this.cache = cache;

      if (client) {
        this.client = client;
      }

      if (resolvers) {
        this.addResolvers(resolvers);
      }

      if (fragmentMatcher) {
        this.setFragmentMatcher(fragmentMatcher);
      }
    }

    LocalState.prototype.addResolvers = function (resolvers) {
      var _this = this;

      this.resolvers = this.resolvers || {};

      if (Array.isArray(resolvers)) {
        resolvers.forEach(function (resolverGroup) {
          _this.resolvers = (0, _apolloUtilities.mergeDeep)(_this.resolvers, resolverGroup);
        });
      } else {
        this.resolvers = (0, _apolloUtilities.mergeDeep)(this.resolvers, resolvers);
      }
    };

    LocalState.prototype.setResolvers = function (resolvers) {
      this.resolvers = {};
      this.addResolvers(resolvers);
    };

    LocalState.prototype.getResolvers = function () {
      return this.resolvers || {};
    };

    LocalState.prototype.runResolvers = function (_a) {
      var document = _a.document,
          remoteResult = _a.remoteResult,
          context = _a.context,
          variables = _a.variables,
          _b = _a.onlyRunForcedResolvers,
          onlyRunForcedResolvers = _b === void 0 ? false : _b;
      return (0, _tslib.__awaiter)(this, void 0, void 0, function () {
        return (0, _tslib.__generator)(this, function (_c) {
          if (document) {
            return [2, this.resolveDocument(document, remoteResult.data, context, variables, this.fragmentMatcher, onlyRunForcedResolvers).then(function (localResult) {
              return (0, _tslib.__assign)((0, _tslib.__assign)({}, remoteResult), {
                data: localResult.result
              });
            })];
          }

          return [2, remoteResult];
        });
      });
    };

    LocalState.prototype.setFragmentMatcher = function (fragmentMatcher) {
      this.fragmentMatcher = fragmentMatcher;
    };

    LocalState.prototype.getFragmentMatcher = function () {
      return this.fragmentMatcher;
    };

    LocalState.prototype.clientQuery = function (document) {
      if ((0, _apolloUtilities.hasDirectives)(['client'], document)) {
        if (this.resolvers) {
          return document;
        }

        process.env.NODE_ENV === "production" || _tsInvariant.invariant.warn('Found @client directives in a query but no ApolloClient resolvers ' + 'were specified. This means ApolloClient local resolver handling ' + 'has been disabled, and @client directives will be passed through ' + 'to your link chain.');
      }

      return null;
    };

    LocalState.prototype.serverQuery = function (document) {
      return this.resolvers ? (0, _apolloUtilities.removeClientSetsFromDocument)(document) : document;
    };

    LocalState.prototype.prepareContext = function (context) {
      if (context === void 0) {
        context = {};
      }

      var cache = this.cache;
      var newContext = (0, _tslib.__assign)((0, _tslib.__assign)({}, context), {
        cache: cache,
        getCacheKey: function (obj) {
          if (cache.config) {
            return cache.config.dataIdFromObject(obj);
          } else {
            process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(false, 6) : (0, _tsInvariant.invariant)(false, 'To use context.getCacheKey, you need to use a cache that has ' + 'a configurable dataIdFromObject, like apollo-cache-inmemory.');
          }
        }
      });
      return newContext;
    };

    LocalState.prototype.addExportedVariables = function (document, variables, context) {
      if (variables === void 0) {
        variables = {};
      }

      if (context === void 0) {
        context = {};
      }

      return (0, _tslib.__awaiter)(this, void 0, void 0, function () {
        return (0, _tslib.__generator)(this, function (_a) {
          if (document) {
            return [2, this.resolveDocument(document, this.buildRootValueFromCache(document, variables) || {}, this.prepareContext(context), variables).then(function (data) {
              return (0, _tslib.__assign)((0, _tslib.__assign)({}, variables), data.exportedVariables);
            })];
          }

          return [2, (0, _tslib.__assign)({}, variables)];
        });
      });
    };

    LocalState.prototype.shouldForceResolvers = function (document) {
      var forceResolvers = false;
      (0, _visitor.visit)(document, {
        Directive: {
          enter: function (node) {
            if (node.name.value === 'client' && node.arguments) {
              forceResolvers = node.arguments.some(function (arg) {
                return arg.name.value === 'always' && arg.value.kind === 'BooleanValue' && arg.value.value === true;
              });

              if (forceResolvers) {
                return _visitor.BREAK;
              }
            }
          }
        }
      });
      return forceResolvers;
    };

    LocalState.prototype.buildRootValueFromCache = function (document, variables) {
      return this.cache.diff({
        query: (0, _apolloUtilities.buildQueryFromSelectionSet)(document),
        variables: variables,
        returnPartialData: true,
        optimistic: false
      }).result;
    };

    LocalState.prototype.resolveDocument = function (document, rootValue, context, variables, fragmentMatcher, onlyRunForcedResolvers) {
      if (context === void 0) {
        context = {};
      }

      if (variables === void 0) {
        variables = {};
      }

      if (fragmentMatcher === void 0) {
        fragmentMatcher = function () {
          return true;
        };
      }

      if (onlyRunForcedResolvers === void 0) {
        onlyRunForcedResolvers = false;
      }

      return (0, _tslib.__awaiter)(this, void 0, void 0, function () {
        var mainDefinition, fragments, fragmentMap, definitionOperation, defaultOperationType, _a, cache, client, execContext;

        return (0, _tslib.__generator)(this, function (_b) {
          mainDefinition = (0, _apolloUtilities.getMainDefinition)(document);
          fragments = (0, _apolloUtilities.getFragmentDefinitions)(document);
          fragmentMap = (0, _apolloUtilities.createFragmentMap)(fragments);
          definitionOperation = mainDefinition.operation;
          defaultOperationType = definitionOperation ? capitalizeFirstLetter(definitionOperation) : 'Query';
          _a = this, cache = _a.cache, client = _a.client;
          execContext = {
            fragmentMap: fragmentMap,
            context: (0, _tslib.__assign)((0, _tslib.__assign)({}, context), {
              cache: cache,
              client: client
            }),
            variables: variables,
            fragmentMatcher: fragmentMatcher,
            defaultOperationType: defaultOperationType,
            exportedVariables: {},
            onlyRunForcedResolvers: onlyRunForcedResolvers
          };
          return [2, this.resolveSelectionSet(mainDefinition.selectionSet, rootValue, execContext).then(function (result) {
            return {
              result: result,
              exportedVariables: execContext.exportedVariables
            };
          })];
        });
      });
    };

    LocalState.prototype.resolveSelectionSet = function (selectionSet, rootValue, execContext) {
      return (0, _tslib.__awaiter)(this, void 0, void 0, function () {
        var fragmentMap, context, variables, resultsToMerge, execute;

        var _this = this;

        return (0, _tslib.__generator)(this, function (_a) {
          fragmentMap = execContext.fragmentMap, context = execContext.context, variables = execContext.variables;
          resultsToMerge = [rootValue];

          execute = function (selection) {
            return (0, _tslib.__awaiter)(_this, void 0, void 0, function () {
              var fragment, typeCondition;
              return (0, _tslib.__generator)(this, function (_a) {
                if (!(0, _apolloUtilities.shouldInclude)(selection, variables)) {
                  return [2];
                }

                if ((0, _apolloUtilities.isField)(selection)) {
                  return [2, this.resolveField(selection, rootValue, execContext).then(function (fieldResult) {
                    var _a;

                    if (typeof fieldResult !== 'undefined') {
                      resultsToMerge.push((_a = {}, _a[(0, _apolloUtilities.resultKeyNameFromField)(selection)] = fieldResult, _a));
                    }
                  })];
                }

                if ((0, _apolloUtilities.isInlineFragment)(selection)) {
                  fragment = selection;
                } else {
                  fragment = fragmentMap[selection.name.value];
                  process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(fragment, 7) : (0, _tsInvariant.invariant)(fragment, "No fragment named " + selection.name.value);
                }

                if (fragment && fragment.typeCondition) {
                  typeCondition = fragment.typeCondition.name.value;

                  if (execContext.fragmentMatcher(rootValue, typeCondition, context)) {
                    return [2, this.resolveSelectionSet(fragment.selectionSet, rootValue, execContext).then(function (fragmentResult) {
                      resultsToMerge.push(fragmentResult);
                    })];
                  }
                }

                return [2];
              });
            });
          };

          return [2, Promise.all(selectionSet.selections.map(execute)).then(function () {
            return (0, _apolloUtilities.mergeDeepArray)(resultsToMerge);
          })];
        });
      });
    };

    LocalState.prototype.resolveField = function (field, rootValue, execContext) {
      return (0, _tslib.__awaiter)(this, void 0, void 0, function () {
        var variables, fieldName, aliasedFieldName, aliasUsed, defaultResult, resultPromise, resolverType, resolverMap, resolve;

        var _this = this;

        return (0, _tslib.__generator)(this, function (_a) {
          variables = execContext.variables;
          fieldName = field.name.value;
          aliasedFieldName = (0, _apolloUtilities.resultKeyNameFromField)(field);
          aliasUsed = fieldName !== aliasedFieldName;
          defaultResult = rootValue[aliasedFieldName] || rootValue[fieldName];
          resultPromise = Promise.resolve(defaultResult);

          if (!execContext.onlyRunForcedResolvers || this.shouldForceResolvers(field)) {
            resolverType = rootValue.__typename || execContext.defaultOperationType;
            resolverMap = this.resolvers && this.resolvers[resolverType];

            if (resolverMap) {
              resolve = resolverMap[aliasUsed ? fieldName : aliasedFieldName];

              if (resolve) {
                resultPromise = Promise.resolve(resolve(rootValue, (0, _apolloUtilities.argumentsObjectFromField)(field, variables), execContext.context, {
                  field: field,
                  fragmentMap: execContext.fragmentMap
                }));
              }
            }
          }

          return [2, resultPromise.then(function (result) {
            if (result === void 0) {
              result = defaultResult;
            }

            if (field.directives) {
              field.directives.forEach(function (directive) {
                if (directive.name.value === 'export' && directive.arguments) {
                  directive.arguments.forEach(function (arg) {
                    if (arg.name.value === 'as' && arg.value.kind === 'StringValue') {
                      execContext.exportedVariables[arg.value.value] = result;
                    }
                  });
                }
              });
            }

            if (!field.selectionSet) {
              return result;
            }

            if (result == null) {
              return result;
            }

            if (Array.isArray(result)) {
              return _this.resolveSubSelectedArray(field, result, execContext);
            }

            if (field.selectionSet) {
              return _this.resolveSelectionSet(field.selectionSet, result, execContext);
            }
          })];
        });
      });
    };

    LocalState.prototype.resolveSubSelectedArray = function (field, result, execContext) {
      var _this = this;

      return Promise.all(result.map(function (item) {
        if (item === null) {
          return null;
        }

        if (Array.isArray(item)) {
          return _this.resolveSubSelectedArray(field, item, execContext);
        }

        if (field.selectionSet) {
          return _this.resolveSelectionSet(field.selectionSet, item, execContext);
        }
      }));
    };

    return LocalState;
  }();

  function multiplex(inner) {
    var observers = new Set();
    var sub = null;
    return new Observable(function (observer) {
      observers.add(observer);
      sub = sub || inner.subscribe({
        next: function (value) {
          observers.forEach(function (obs) {
            return obs.next && obs.next(value);
          });
        },
        error: function (error) {
          observers.forEach(function (obs) {
            return obs.error && obs.error(error);
          });
        },
        complete: function () {
          observers.forEach(function (obs) {
            return obs.complete && obs.complete();
          });
        }
      });
      return function () {
        if (observers.delete(observer) && !observers.size && sub) {
          sub.unsubscribe();
          sub = null;
        }
      };
    });
  }

  function asyncMap(observable, mapFn) {
    return new Observable(function (observer) {
      var next = observer.next,
          error = observer.error,
          complete = observer.complete;
      var activeNextCount = 0;
      var completed = false;
      var handler = {
        next: function (value) {
          ++activeNextCount;
          new Promise(function (resolve) {
            resolve(mapFn(value));
          }).then(function (result) {
            --activeNextCount;
            next && next.call(observer, result);
            completed && handler.complete();
          }, function (e) {
            --activeNextCount;
            error && error.call(observer, e);
          });
        },
        error: function (e) {
          error && error.call(observer, e);
        },
        complete: function () {
          completed = true;

          if (!activeNextCount) {
            complete && complete.call(observer);
          }
        }
      };
      var sub = observable.subscribe(handler);
      return function () {
        return sub.unsubscribe();
      };
    });
  }

  var hasOwnProperty = Object.prototype.hasOwnProperty;

  var QueryManager = function () {
    function QueryManager(_a) {
      var link = _a.link,
          _b = _a.queryDeduplication,
          queryDeduplication = _b === void 0 ? false : _b,
          store = _a.store,
          _c = _a.onBroadcast,
          onBroadcast = _c === void 0 ? function () {
        return undefined;
      } : _c,
          _d = _a.ssrMode,
          ssrMode = _d === void 0 ? false : _d,
          _e = _a.clientAwareness,
          clientAwareness = _e === void 0 ? {} : _e,
          localState = _a.localState,
          assumeImmutableResults = _a.assumeImmutableResults;
      this.mutationStore = new MutationStore();
      this.queryStore = new QueryStore();
      this.clientAwareness = {};
      this.idCounter = 1;
      this.queries = new Map();
      this.fetchQueryRejectFns = new Map();
      this.transformCache = new (_apolloUtilities.canUseWeakMap ? WeakMap : Map)();
      this.inFlightLinkObservables = new Map();
      this.pollingInfoByQueryId = new Map();
      this.link = link;
      this.queryDeduplication = queryDeduplication;
      this.dataStore = store;
      this.onBroadcast = onBroadcast;
      this.clientAwareness = clientAwareness;
      this.localState = localState || new LocalState({
        cache: store.getCache()
      });
      this.ssrMode = ssrMode;
      this.assumeImmutableResults = !!assumeImmutableResults;
    }

    QueryManager.prototype.stop = function () {
      var _this = this;

      this.queries.forEach(function (_info, queryId) {
        _this.stopQueryNoBroadcast(queryId);
      });
      this.fetchQueryRejectFns.forEach(function (reject) {
        reject(process.env.NODE_ENV === "production" ? new _tsInvariant.InvariantError(8) : new _tsInvariant.InvariantError('QueryManager stopped while query was in flight'));
      });
    };

    QueryManager.prototype.mutate = function (_a) {
      var mutation = _a.mutation,
          variables = _a.variables,
          optimisticResponse = _a.optimisticResponse,
          updateQueriesByName = _a.updateQueries,
          _b = _a.refetchQueries,
          refetchQueries = _b === void 0 ? [] : _b,
          _c = _a.awaitRefetchQueries,
          awaitRefetchQueries = _c === void 0 ? false : _c,
          updateWithProxyFn = _a.update,
          _d = _a.errorPolicy,
          errorPolicy = _d === void 0 ? 'none' : _d,
          fetchPolicy = _a.fetchPolicy,
          _e = _a.context,
          context = _e === void 0 ? {} : _e;
      return (0, _tslib.__awaiter)(this, void 0, void 0, function () {
        var mutationId, generateUpdateQueriesInfo, self;

        var _this = this;

        return (0, _tslib.__generator)(this, function (_f) {
          switch (_f.label) {
            case 0:
              process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(mutation, 9) : (0, _tsInvariant.invariant)(mutation, 'mutation option is required. You must specify your GraphQL document in the mutation option.');
              process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(!fetchPolicy || fetchPolicy === 'no-cache', 10) : (0, _tsInvariant.invariant)(!fetchPolicy || fetchPolicy === 'no-cache', "Mutations only support a 'no-cache' fetchPolicy. If you don't want to disable the cache, remove your fetchPolicy setting to proceed with the default mutation behavior.");
              mutationId = this.generateQueryId();
              mutation = this.transform(mutation).document;
              this.setQuery(mutationId, function () {
                return {
                  document: mutation
                };
              });
              variables = this.getVariables(mutation, variables);
              if (!this.transform(mutation).hasClientExports) return [3, 2];
              return [4, this.localState.addExportedVariables(mutation, variables, context)];

            case 1:
              variables = _f.sent();
              _f.label = 2;

            case 2:
              generateUpdateQueriesInfo = function () {
                var ret = {};

                if (updateQueriesByName) {
                  _this.queries.forEach(function (_a, queryId) {
                    var observableQuery = _a.observableQuery;

                    if (observableQuery) {
                      var queryName = observableQuery.queryName;

                      if (queryName && hasOwnProperty.call(updateQueriesByName, queryName)) {
                        ret[queryId] = {
                          updater: updateQueriesByName[queryName],
                          query: _this.queryStore.get(queryId)
                        };
                      }
                    }
                  });
                }

                return ret;
              };

              this.mutationStore.initMutation(mutationId, mutation, variables);
              this.dataStore.markMutationInit({
                mutationId: mutationId,
                document: mutation,
                variables: variables,
                updateQueries: generateUpdateQueriesInfo(),
                update: updateWithProxyFn,
                optimisticResponse: optimisticResponse
              });
              this.broadcastQueries();
              self = this;
              return [2, new Promise(function (resolve, reject) {
                var storeResult;
                var error;
                self.getObservableFromLink(mutation, (0, _tslib.__assign)((0, _tslib.__assign)({}, context), {
                  optimisticResponse: optimisticResponse
                }), variables, false).subscribe({
                  next: function (result) {
                    if ((0, _apolloUtilities.graphQLResultHasError)(result) && errorPolicy === 'none') {
                      error = new ApolloError({
                        graphQLErrors: result.errors
                      });
                      return;
                    }

                    self.mutationStore.markMutationResult(mutationId);

                    if (fetchPolicy !== 'no-cache') {
                      self.dataStore.markMutationResult({
                        mutationId: mutationId,
                        result: result,
                        document: mutation,
                        variables: variables,
                        updateQueries: generateUpdateQueriesInfo(),
                        update: updateWithProxyFn
                      });
                    }

                    storeResult = result;
                  },
                  error: function (err) {
                    self.mutationStore.markMutationError(mutationId, err);
                    self.dataStore.markMutationComplete({
                      mutationId: mutationId,
                      optimisticResponse: optimisticResponse
                    });
                    self.broadcastQueries();
                    self.setQuery(mutationId, function () {
                      return {
                        document: null
                      };
                    });
                    reject(new ApolloError({
                      networkError: err
                    }));
                  },
                  complete: function () {
                    if (error) {
                      self.mutationStore.markMutationError(mutationId, error);
                    }

                    self.dataStore.markMutationComplete({
                      mutationId: mutationId,
                      optimisticResponse: optimisticResponse
                    });
                    self.broadcastQueries();

                    if (error) {
                      reject(error);
                      return;
                    }

                    if (typeof refetchQueries === 'function') {
                      refetchQueries = refetchQueries(storeResult);
                    }

                    var refetchQueryPromises = [];

                    if (isNonEmptyArray(refetchQueries)) {
                      refetchQueries.forEach(function (refetchQuery) {
                        if (typeof refetchQuery === 'string') {
                          self.queries.forEach(function (_a) {
                            var observableQuery = _a.observableQuery;

                            if (observableQuery && observableQuery.queryName === refetchQuery) {
                              refetchQueryPromises.push(observableQuery.refetch());
                            }
                          });
                        } else {
                          var queryOptions = {
                            query: refetchQuery.query,
                            variables: refetchQuery.variables,
                            fetchPolicy: 'network-only'
                          };

                          if (refetchQuery.context) {
                            queryOptions.context = refetchQuery.context;
                          }

                          refetchQueryPromises.push(self.query(queryOptions));
                        }
                      });
                    }

                    Promise.all(awaitRefetchQueries ? refetchQueryPromises : []).then(function () {
                      self.setQuery(mutationId, function () {
                        return {
                          document: null
                        };
                      });

                      if (errorPolicy === 'ignore' && storeResult && (0, _apolloUtilities.graphQLResultHasError)(storeResult)) {
                        delete storeResult.errors;
                      }

                      resolve(storeResult);
                    });
                  }
                });
              })];
          }
        });
      });
    };

    QueryManager.prototype.fetchQuery = function (queryId, options, fetchType, fetchMoreForQueryId) {
      return (0, _tslib.__awaiter)(this, void 0, void 0, function () {
        var _a, metadata, _b, fetchPolicy, _c, context, query, variables, storeResult, isNetworkOnly, needToFetch, _d, complete, result, shouldFetch, requestId, cancel, networkResult;

        var _this = this;

        return (0, _tslib.__generator)(this, function (_e) {
          switch (_e.label) {
            case 0:
              _a = options.metadata, metadata = _a === void 0 ? null : _a, _b = options.fetchPolicy, fetchPolicy = _b === void 0 ? 'cache-first' : _b, _c = options.context, context = _c === void 0 ? {} : _c;
              query = this.transform(options.query).document;
              variables = this.getVariables(query, options.variables);
              if (!this.transform(query).hasClientExports) return [3, 2];
              return [4, this.localState.addExportedVariables(query, variables, context)];

            case 1:
              variables = _e.sent();
              _e.label = 2;

            case 2:
              options = (0, _tslib.__assign)((0, _tslib.__assign)({}, options), {
                variables: variables
              });
              isNetworkOnly = fetchPolicy === 'network-only' || fetchPolicy === 'no-cache';
              needToFetch = isNetworkOnly;

              if (!isNetworkOnly) {
                _d = this.dataStore.getCache().diff({
                  query: query,
                  variables: variables,
                  returnPartialData: true,
                  optimistic: false
                }), complete = _d.complete, result = _d.result;
                needToFetch = !complete || fetchPolicy === 'cache-and-network';
                storeResult = result;
              }

              shouldFetch = needToFetch && fetchPolicy !== 'cache-only' && fetchPolicy !== 'standby';
              if ((0, _apolloUtilities.hasDirectives)(['live'], query)) shouldFetch = true;
              requestId = this.idCounter++;
              cancel = fetchPolicy !== 'no-cache' ? this.updateQueryWatch(queryId, query, options) : undefined;
              this.setQuery(queryId, function () {
                return {
                  document: query,
                  lastRequestId: requestId,
                  invalidated: true,
                  cancel: cancel
                };
              });
              this.invalidate(fetchMoreForQueryId);
              this.queryStore.initQuery({
                queryId: queryId,
                document: query,
                storePreviousVariables: shouldFetch,
                variables: variables,
                isPoll: fetchType === FetchType.poll,
                isRefetch: fetchType === FetchType.refetch,
                metadata: metadata,
                fetchMoreForQueryId: fetchMoreForQueryId
              });
              this.broadcastQueries();

              if (shouldFetch) {
                networkResult = this.fetchRequest({
                  requestId: requestId,
                  queryId: queryId,
                  document: query,
                  options: options,
                  fetchMoreForQueryId: fetchMoreForQueryId
                }).catch(function (error) {
                  if (isApolloError(error)) {
                    throw error;
                  } else {
                    if (requestId >= _this.getQuery(queryId).lastRequestId) {
                      _this.queryStore.markQueryError(queryId, error, fetchMoreForQueryId);

                      _this.invalidate(queryId);

                      _this.invalidate(fetchMoreForQueryId);

                      _this.broadcastQueries();
                    }

                    throw new ApolloError({
                      networkError: error
                    });
                  }
                });

                if (fetchPolicy !== 'cache-and-network') {
                  return [2, networkResult];
                }

                networkResult.catch(function () {});
              }

              this.queryStore.markQueryResultClient(queryId, !shouldFetch);
              this.invalidate(queryId);
              this.invalidate(fetchMoreForQueryId);

              if (this.transform(query).hasForcedResolvers) {
                return [2, this.localState.runResolvers({
                  document: query,
                  remoteResult: {
                    data: storeResult
                  },
                  context: context,
                  variables: variables,
                  onlyRunForcedResolvers: true
                }).then(function (result) {
                  _this.markQueryResult(queryId, result, options, fetchMoreForQueryId);

                  _this.broadcastQueries();

                  return result;
                })];
              }

              this.broadcastQueries();
              return [2, {
                data: storeResult
              }];
          }
        });
      });
    };

    QueryManager.prototype.markQueryResult = function (queryId, result, _a, fetchMoreForQueryId) {
      var fetchPolicy = _a.fetchPolicy,
          variables = _a.variables,
          errorPolicy = _a.errorPolicy;

      if (fetchPolicy === 'no-cache') {
        this.setQuery(queryId, function () {
          return {
            newData: {
              result: result.data,
              complete: true
            }
          };
        });
      } else {
        this.dataStore.markQueryResult(result, this.getQuery(queryId).document, variables, fetchMoreForQueryId, errorPolicy === 'ignore' || errorPolicy === 'all');
      }
    };

    QueryManager.prototype.queryListenerForObserver = function (queryId, options, observer) {
      var _this = this;

      function invoke(method, argument) {
        if (observer[method]) {
          try {
            observer[method](argument);
          } catch (e) {
            process.env.NODE_ENV === "production" || _tsInvariant.invariant.error(e);
          }
        } else if (method === 'error') {
          process.env.NODE_ENV === "production" || _tsInvariant.invariant.error(argument);
        }
      }

      return function (queryStoreValue, newData) {
        _this.invalidate(queryId, false);

        if (!queryStoreValue) return;

        var _a = _this.getQuery(queryId),
            observableQuery = _a.observableQuery,
            document = _a.document;

        var fetchPolicy = observableQuery ? observableQuery.options.fetchPolicy : options.fetchPolicy;
        if (fetchPolicy === 'standby') return;
        var loading = isNetworkRequestInFlight(queryStoreValue.networkStatus);
        var lastResult = observableQuery && observableQuery.getLastResult();
        var networkStatusChanged = !!(lastResult && lastResult.networkStatus !== queryStoreValue.networkStatus);
        var shouldNotifyIfLoading = options.returnPartialData || !newData && queryStoreValue.previousVariables || networkStatusChanged && options.notifyOnNetworkStatusChange || fetchPolicy === 'cache-only' || fetchPolicy === 'cache-and-network';

        if (loading && !shouldNotifyIfLoading) {
          return;
        }

        var hasGraphQLErrors = isNonEmptyArray(queryStoreValue.graphQLErrors);
        var errorPolicy = observableQuery && observableQuery.options.errorPolicy || options.errorPolicy || 'none';

        if (errorPolicy === 'none' && hasGraphQLErrors || queryStoreValue.networkError) {
          return invoke('error', new ApolloError({
            graphQLErrors: queryStoreValue.graphQLErrors,
            networkError: queryStoreValue.networkError
          }));
        }

        try {
          var data = void 0;
          var isMissing = void 0;

          if (newData) {
            if (fetchPolicy !== 'no-cache' && fetchPolicy !== 'network-only') {
              _this.setQuery(queryId, function () {
                return {
                  newData: null
                };
              });
            }

            data = newData.result;
            isMissing = !newData.complete;
          } else {
            var lastError = observableQuery && observableQuery.getLastError();
            var errorStatusChanged = errorPolicy !== 'none' && (lastError && lastError.graphQLErrors) !== queryStoreValue.graphQLErrors;

            if (lastResult && lastResult.data && !errorStatusChanged) {
              data = lastResult.data;
              isMissing = false;
            } else {
              var diffResult = _this.dataStore.getCache().diff({
                query: document,
                variables: queryStoreValue.previousVariables || queryStoreValue.variables,
                returnPartialData: true,
                optimistic: true
              });

              data = diffResult.result;
              isMissing = !diffResult.complete;
            }
          }

          var stale = isMissing && !(options.returnPartialData || fetchPolicy === 'cache-only');
          var resultFromStore = {
            data: stale ? lastResult && lastResult.data : data,
            loading: loading,
            networkStatus: queryStoreValue.networkStatus,
            stale: stale
          };

          if (errorPolicy === 'all' && hasGraphQLErrors) {
            resultFromStore.errors = queryStoreValue.graphQLErrors;
          }

          invoke('next', resultFromStore);
        } catch (networkError) {
          invoke('error', new ApolloError({
            networkError: networkError
          }));
        }
      };
    };

    QueryManager.prototype.transform = function (document) {
      var transformCache = this.transformCache;

      if (!transformCache.has(document)) {
        var cache = this.dataStore.getCache();
        var transformed = cache.transformDocument(document);
        var forLink = (0, _apolloUtilities.removeConnectionDirectiveFromDocument)(cache.transformForLink(transformed));
        var clientQuery = this.localState.clientQuery(transformed);
        var serverQuery = this.localState.serverQuery(forLink);
        var cacheEntry_1 = {
          document: transformed,
          hasClientExports: (0, _apolloUtilities.hasClientExports)(transformed),
          hasForcedResolvers: this.localState.shouldForceResolvers(transformed),
          clientQuery: clientQuery,
          serverQuery: serverQuery,
          defaultVars: (0, _apolloUtilities.getDefaultValues)((0, _apolloUtilities.getOperationDefinition)(transformed))
        };

        var add = function (doc) {
          if (doc && !transformCache.has(doc)) {
            transformCache.set(doc, cacheEntry_1);
          }
        };

        add(document);
        add(transformed);
        add(clientQuery);
        add(serverQuery);
      }

      return transformCache.get(document);
    };

    QueryManager.prototype.getVariables = function (document, variables) {
      return (0, _tslib.__assign)((0, _tslib.__assign)({}, this.transform(document).defaultVars), variables);
    };

    QueryManager.prototype.watchQuery = function (options, shouldSubscribe) {
      if (shouldSubscribe === void 0) {
        shouldSubscribe = true;
      }

      process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(options.fetchPolicy !== 'standby', 11) : (0, _tsInvariant.invariant)(options.fetchPolicy !== 'standby', 'client.watchQuery cannot be called with fetchPolicy set to "standby"');
      options.variables = this.getVariables(options.query, options.variables);

      if (typeof options.notifyOnNetworkStatusChange === 'undefined') {
        options.notifyOnNetworkStatusChange = false;
      }

      var transformedOptions = (0, _tslib.__assign)({}, options);
      return new ObservableQuery({
        queryManager: this,
        options: transformedOptions,
        shouldSubscribe: shouldSubscribe
      });
    };

    QueryManager.prototype.query = function (options) {
      var _this = this;

      process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(options.query, 12) : (0, _tsInvariant.invariant)(options.query, 'query option is required. You must specify your GraphQL document ' + 'in the query option.');
      process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(options.query.kind === 'Document', 13) : (0, _tsInvariant.invariant)(options.query.kind === 'Document', 'You must wrap the query string in a "gql" tag.');
      process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(!options.returnPartialData, 14) : (0, _tsInvariant.invariant)(!options.returnPartialData, 'returnPartialData option only supported on watchQuery.');
      process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(!options.pollInterval, 15) : (0, _tsInvariant.invariant)(!options.pollInterval, 'pollInterval option only supported on watchQuery.');
      return new Promise(function (resolve, reject) {
        var watchedQuery = _this.watchQuery(options, false);

        _this.fetchQueryRejectFns.set("query:" + watchedQuery.queryId, reject);

        watchedQuery.result().then(resolve, reject).then(function () {
          return _this.fetchQueryRejectFns.delete("query:" + watchedQuery.queryId);
        });
      });
    };

    QueryManager.prototype.generateQueryId = function () {
      return String(this.idCounter++);
    };

    QueryManager.prototype.stopQueryInStore = function (queryId) {
      this.stopQueryInStoreNoBroadcast(queryId);
      this.broadcastQueries();
    };

    QueryManager.prototype.stopQueryInStoreNoBroadcast = function (queryId) {
      this.stopPollingQuery(queryId);
      this.queryStore.stopQuery(queryId);
      this.invalidate(queryId);
    };

    QueryManager.prototype.addQueryListener = function (queryId, listener) {
      this.setQuery(queryId, function (_a) {
        var listeners = _a.listeners;
        listeners.add(listener);
        return {
          invalidated: false
        };
      });
    };

    QueryManager.prototype.updateQueryWatch = function (queryId, document, options) {
      var _this = this;

      var cancel = this.getQuery(queryId).cancel;
      if (cancel) cancel();

      var previousResult = function () {
        var previousResult = null;

        var observableQuery = _this.getQuery(queryId).observableQuery;

        if (observableQuery) {
          var lastResult = observableQuery.getLastResult();

          if (lastResult) {
            previousResult = lastResult.data;
          }
        }

        return previousResult;
      };

      return this.dataStore.getCache().watch({
        query: document,
        variables: options.variables,
        optimistic: true,
        previousResult: previousResult,
        callback: function (newData) {
          _this.setQuery(queryId, function () {
            return {
              invalidated: true,
              newData: newData
            };
          });
        }
      });
    };

    QueryManager.prototype.addObservableQuery = function (queryId, observableQuery) {
      this.setQuery(queryId, function () {
        return {
          observableQuery: observableQuery
        };
      });
    };

    QueryManager.prototype.removeObservableQuery = function (queryId) {
      var cancel = this.getQuery(queryId).cancel;
      this.setQuery(queryId, function () {
        return {
          observableQuery: null
        };
      });
      if (cancel) cancel();
    };

    QueryManager.prototype.clearStore = function () {
      this.fetchQueryRejectFns.forEach(function (reject) {
        reject(process.env.NODE_ENV === "production" ? new _tsInvariant.InvariantError(16) : new _tsInvariant.InvariantError('Store reset while query was in flight (not completed in link chain)'));
      });
      var resetIds = [];
      this.queries.forEach(function (_a, queryId) {
        var observableQuery = _a.observableQuery;
        if (observableQuery) resetIds.push(queryId);
      });
      this.queryStore.reset(resetIds);
      this.mutationStore.reset();
      return this.dataStore.reset();
    };

    QueryManager.prototype.resetStore = function () {
      var _this = this;

      return this.clearStore().then(function () {
        return _this.reFetchObservableQueries();
      });
    };

    QueryManager.prototype.reFetchObservableQueries = function (includeStandby) {
      var _this = this;

      if (includeStandby === void 0) {
        includeStandby = false;
      }

      var observableQueryPromises = [];
      this.queries.forEach(function (_a, queryId) {
        var observableQuery = _a.observableQuery;

        if (observableQuery) {
          var fetchPolicy = observableQuery.options.fetchPolicy;
          observableQuery.resetLastResults();

          if (fetchPolicy !== 'cache-only' && (includeStandby || fetchPolicy !== 'standby')) {
            observableQueryPromises.push(observableQuery.refetch());
          }

          _this.setQuery(queryId, function () {
            return {
              newData: null
            };
          });

          _this.invalidate(queryId);
        }
      });
      this.broadcastQueries();
      return Promise.all(observableQueryPromises);
    };

    QueryManager.prototype.observeQuery = function (queryId, options, observer) {
      this.addQueryListener(queryId, this.queryListenerForObserver(queryId, options, observer));
      return this.fetchQuery(queryId, options);
    };

    QueryManager.prototype.startQuery = function (queryId, options, listener) {
      process.env.NODE_ENV === "production" || _tsInvariant.invariant.warn("The QueryManager.startQuery method has been deprecated");
      this.addQueryListener(queryId, listener);
      this.fetchQuery(queryId, options).catch(function () {
        return undefined;
      });
      return queryId;
    };

    QueryManager.prototype.startGraphQLSubscription = function (_a) {
      var _this = this;

      var query = _a.query,
          fetchPolicy = _a.fetchPolicy,
          variables = _a.variables;
      query = this.transform(query).document;
      variables = this.getVariables(query, variables);

      var makeObservable = function (variables) {
        return _this.getObservableFromLink(query, {}, variables, false).map(function (result) {
          if (!fetchPolicy || fetchPolicy !== 'no-cache') {
            _this.dataStore.markSubscriptionResult(result, query, variables);

            _this.broadcastQueries();
          }

          if ((0, _apolloUtilities.graphQLResultHasError)(result)) {
            throw new ApolloError({
              graphQLErrors: result.errors
            });
          }

          return result;
        });
      };

      if (this.transform(query).hasClientExports) {
        var observablePromise_1 = this.localState.addExportedVariables(query, variables).then(makeObservable);
        return new Observable(function (observer) {
          var sub = null;
          observablePromise_1.then(function (observable) {
            return sub = observable.subscribe(observer);
          }, observer.error);
          return function () {
            return sub && sub.unsubscribe();
          };
        });
      }

      return makeObservable(variables);
    };

    QueryManager.prototype.stopQuery = function (queryId) {
      this.stopQueryNoBroadcast(queryId);
      this.broadcastQueries();
    };

    QueryManager.prototype.stopQueryNoBroadcast = function (queryId) {
      this.stopQueryInStoreNoBroadcast(queryId);
      this.removeQuery(queryId);
    };

    QueryManager.prototype.removeQuery = function (queryId) {
      this.fetchQueryRejectFns.delete("query:" + queryId);
      this.fetchQueryRejectFns.delete("fetchRequest:" + queryId);
      this.getQuery(queryId).subscriptions.forEach(function (x) {
        return x.unsubscribe();
      });
      this.queries.delete(queryId);
    };

    QueryManager.prototype.getCurrentQueryResult = function (observableQuery, optimistic) {
      if (optimistic === void 0) {
        optimistic = true;
      }

      var _a = observableQuery.options,
          variables = _a.variables,
          query = _a.query,
          fetchPolicy = _a.fetchPolicy,
          returnPartialData = _a.returnPartialData;
      var lastResult = observableQuery.getLastResult();
      var newData = this.getQuery(observableQuery.queryId).newData;

      if (newData && newData.complete) {
        return {
          data: newData.result,
          partial: false
        };
      }

      if (fetchPolicy === 'no-cache' || fetchPolicy === 'network-only') {
        return {
          data: undefined,
          partial: false
        };
      }

      var _b = this.dataStore.getCache().diff({
        query: query,
        variables: variables,
        previousResult: lastResult ? lastResult.data : undefined,
        returnPartialData: true,
        optimistic: optimistic
      }),
          result = _b.result,
          complete = _b.complete;

      return {
        data: complete || returnPartialData ? result : void 0,
        partial: !complete
      };
    };

    QueryManager.prototype.getQueryWithPreviousResult = function (queryIdOrObservable) {
      var observableQuery;

      if (typeof queryIdOrObservable === 'string') {
        var foundObserveableQuery = this.getQuery(queryIdOrObservable).observableQuery;
        process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(foundObserveableQuery, 17) : (0, _tsInvariant.invariant)(foundObserveableQuery, "ObservableQuery with this id doesn't exist: " + queryIdOrObservable);
        observableQuery = foundObserveableQuery;
      } else {
        observableQuery = queryIdOrObservable;
      }

      var _a = observableQuery.options,
          variables = _a.variables,
          query = _a.query;
      return {
        previousResult: this.getCurrentQueryResult(observableQuery, false).data,
        variables: variables,
        document: query
      };
    };

    QueryManager.prototype.broadcastQueries = function () {
      var _this = this;

      this.onBroadcast();
      this.queries.forEach(function (info, id) {
        if (info.invalidated) {
          info.listeners.forEach(function (listener) {
            if (listener) {
              listener(_this.queryStore.get(id), info.newData);
            }
          });
        }
      });
    };

    QueryManager.prototype.getLocalState = function () {
      return this.localState;
    };

    QueryManager.prototype.getObservableFromLink = function (query, context, variables, deduplication) {
      var _this = this;

      if (deduplication === void 0) {
        deduplication = this.queryDeduplication;
      }

      var observable;
      var serverQuery = this.transform(query).serverQuery;

      if (serverQuery) {
        var _a = this,
            inFlightLinkObservables_1 = _a.inFlightLinkObservables,
            link = _a.link;

        var operation = {
          query: serverQuery,
          variables: variables,
          operationName: (0, _apolloUtilities.getOperationName)(serverQuery) || void 0,
          context: this.prepareContext((0, _tslib.__assign)((0, _tslib.__assign)({}, context), {
            forceFetch: !deduplication
          }))
        };
        context = operation.context;

        if (deduplication) {
          var byVariables_1 = inFlightLinkObservables_1.get(serverQuery) || new Map();
          inFlightLinkObservables_1.set(serverQuery, byVariables_1);
          var varJson_1 = JSON.stringify(variables);
          observable = byVariables_1.get(varJson_1);

          if (!observable) {
            byVariables_1.set(varJson_1, observable = multiplex((0, _apolloLink.execute)(link, operation)));

            var cleanup = function () {
              byVariables_1.delete(varJson_1);
              if (!byVariables_1.size) inFlightLinkObservables_1.delete(serverQuery);
              cleanupSub_1.unsubscribe();
            };

            var cleanupSub_1 = observable.subscribe({
              next: cleanup,
              error: cleanup,
              complete: cleanup
            });
          }
        } else {
          observable = multiplex((0, _apolloLink.execute)(link, operation));
        }
      } else {
        observable = Observable.of({
          data: {}
        });
        context = this.prepareContext(context);
      }

      var clientQuery = this.transform(query).clientQuery;

      if (clientQuery) {
        observable = asyncMap(observable, function (result) {
          return _this.localState.runResolvers({
            document: clientQuery,
            remoteResult: result,
            context: context,
            variables: variables
          });
        });
      }

      return observable;
    };

    QueryManager.prototype.fetchRequest = function (_a) {
      var _this = this;

      var requestId = _a.requestId,
          queryId = _a.queryId,
          document = _a.document,
          options = _a.options,
          fetchMoreForQueryId = _a.fetchMoreForQueryId;
      var variables = options.variables,
          _b = options.errorPolicy,
          errorPolicy = _b === void 0 ? 'none' : _b,
          fetchPolicy = options.fetchPolicy;
      var resultFromStore;
      var errorsFromStore;
      return new Promise(function (resolve, reject) {
        var observable = _this.getObservableFromLink(document, options.context, variables);

        var fqrfId = "fetchRequest:" + queryId;

        _this.fetchQueryRejectFns.set(fqrfId, reject);

        var cleanup = function () {
          _this.fetchQueryRejectFns.delete(fqrfId);

          _this.setQuery(queryId, function (_a) {
            var subscriptions = _a.subscriptions;
            subscriptions.delete(subscription);
          });
        };

        var subscription = observable.map(function (result) {
          if (requestId >= _this.getQuery(queryId).lastRequestId) {
            _this.markQueryResult(queryId, result, options, fetchMoreForQueryId);

            _this.queryStore.markQueryResult(queryId, result, fetchMoreForQueryId);

            _this.invalidate(queryId);

            _this.invalidate(fetchMoreForQueryId);

            _this.broadcastQueries();
          }

          if (errorPolicy === 'none' && isNonEmptyArray(result.errors)) {
            return reject(new ApolloError({
              graphQLErrors: result.errors
            }));
          }

          if (errorPolicy === 'all') {
            errorsFromStore = result.errors;
          }

          if (fetchMoreForQueryId || fetchPolicy === 'no-cache') {
            resultFromStore = result.data;
          } else {
            var _a = _this.dataStore.getCache().diff({
              variables: variables,
              query: document,
              optimistic: false,
              returnPartialData: true
            }),
                result_1 = _a.result,
                complete = _a.complete;

            if (complete || options.returnPartialData) {
              resultFromStore = result_1;
            }
          }
        }).subscribe({
          error: function (error) {
            cleanup();
            reject(error);
          },
          complete: function () {
            cleanup();
            resolve({
              data: resultFromStore,
              errors: errorsFromStore,
              loading: false,
              networkStatus: NetworkStatus.ready,
              stale: false
            });
          }
        });

        _this.setQuery(queryId, function (_a) {
          var subscriptions = _a.subscriptions;
          subscriptions.add(subscription);
        });
      });
    };

    QueryManager.prototype.getQuery = function (queryId) {
      return this.queries.get(queryId) || {
        listeners: new Set(),
        invalidated: false,
        document: null,
        newData: null,
        lastRequestId: 1,
        observableQuery: null,
        subscriptions: new Set()
      };
    };

    QueryManager.prototype.setQuery = function (queryId, updater) {
      var prev = this.getQuery(queryId);
      var newInfo = (0, _tslib.__assign)((0, _tslib.__assign)({}, prev), updater(prev));
      this.queries.set(queryId, newInfo);
    };

    QueryManager.prototype.invalidate = function (queryId, invalidated) {
      if (invalidated === void 0) {
        invalidated = true;
      }

      if (queryId) {
        this.setQuery(queryId, function () {
          return {
            invalidated: invalidated
          };
        });
      }
    };

    QueryManager.prototype.prepareContext = function (context) {
      if (context === void 0) {
        context = {};
      }

      var newContext = this.localState.prepareContext(context);
      return (0, _tslib.__assign)((0, _tslib.__assign)({}, newContext), {
        clientAwareness: this.clientAwareness
      });
    };

    QueryManager.prototype.checkInFlight = function (queryId) {
      var query = this.queryStore.get(queryId);
      return query && query.networkStatus !== NetworkStatus.ready && query.networkStatus !== NetworkStatus.error;
    };

    QueryManager.prototype.startPollingQuery = function (options, queryId, listener) {
      var _this = this;

      var pollInterval = options.pollInterval;
      process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(pollInterval, 18) : (0, _tsInvariant.invariant)(pollInterval, 'Attempted to start a polling query without a polling interval.');

      if (!this.ssrMode) {
        var info = this.pollingInfoByQueryId.get(queryId);

        if (!info) {
          this.pollingInfoByQueryId.set(queryId, info = {});
        }

        info.interval = pollInterval;
        info.options = (0, _tslib.__assign)((0, _tslib.__assign)({}, options), {
          fetchPolicy: 'network-only'
        });

        var maybeFetch_1 = function () {
          var info = _this.pollingInfoByQueryId.get(queryId);

          if (info) {
            if (_this.checkInFlight(queryId)) {
              poll_1();
            } else {
              _this.fetchQuery(queryId, info.options, FetchType.poll).then(poll_1, poll_1);
            }
          }
        };

        var poll_1 = function () {
          var info = _this.pollingInfoByQueryId.get(queryId);

          if (info) {
            clearTimeout(info.timeout);
            info.timeout = setTimeout(maybeFetch_1, info.interval);
          }
        };

        if (listener) {
          this.addQueryListener(queryId, listener);
        }

        poll_1();
      }

      return queryId;
    };

    QueryManager.prototype.stopPollingQuery = function (queryId) {
      this.pollingInfoByQueryId.delete(queryId);
    };

    return QueryManager;
  }();

  var DataStore = function () {
    function DataStore(initialCache) {
      this.cache = initialCache;
    }

    DataStore.prototype.getCache = function () {
      return this.cache;
    };

    DataStore.prototype.markQueryResult = function (result, document, variables, fetchMoreForQueryId, ignoreErrors) {
      if (ignoreErrors === void 0) {
        ignoreErrors = false;
      }

      var writeWithErrors = !(0, _apolloUtilities.graphQLResultHasError)(result);

      if (ignoreErrors && (0, _apolloUtilities.graphQLResultHasError)(result) && result.data) {
        writeWithErrors = true;
      }

      if (!fetchMoreForQueryId && writeWithErrors) {
        this.cache.write({
          result: result.data,
          dataId: 'ROOT_QUERY',
          query: document,
          variables: variables
        });
      }
    };

    DataStore.prototype.markSubscriptionResult = function (result, document, variables) {
      if (!(0, _apolloUtilities.graphQLResultHasError)(result)) {
        this.cache.write({
          result: result.data,
          dataId: 'ROOT_SUBSCRIPTION',
          query: document,
          variables: variables
        });
      }
    };

    DataStore.prototype.markMutationInit = function (mutation) {
      var _this = this;

      if (mutation.optimisticResponse) {
        var optimistic_1;

        if (typeof mutation.optimisticResponse === 'function') {
          optimistic_1 = mutation.optimisticResponse(mutation.variables);
        } else {
          optimistic_1 = mutation.optimisticResponse;
        }

        this.cache.recordOptimisticTransaction(function (c) {
          var orig = _this.cache;
          _this.cache = c;

          try {
            _this.markMutationResult({
              mutationId: mutation.mutationId,
              result: {
                data: optimistic_1
              },
              document: mutation.document,
              variables: mutation.variables,
              updateQueries: mutation.updateQueries,
              update: mutation.update
            });
          } finally {
            _this.cache = orig;
          }
        }, mutation.mutationId);
      }
    };

    DataStore.prototype.markMutationResult = function (mutation) {
      var _this = this;

      if (!(0, _apolloUtilities.graphQLResultHasError)(mutation.result)) {
        var cacheWrites_1 = [{
          result: mutation.result.data,
          dataId: 'ROOT_MUTATION',
          query: mutation.document,
          variables: mutation.variables
        }];
        var updateQueries_1 = mutation.updateQueries;

        if (updateQueries_1) {
          Object.keys(updateQueries_1).forEach(function (id) {
            var _a = updateQueries_1[id],
                query = _a.query,
                updater = _a.updater;

            var _b = _this.cache.diff({
              query: query.document,
              variables: query.variables,
              returnPartialData: true,
              optimistic: false
            }),
                currentQueryResult = _b.result,
                complete = _b.complete;

            if (complete) {
              var nextQueryResult = (0, _apolloUtilities.tryFunctionOrLogError)(function () {
                return updater(currentQueryResult, {
                  mutationResult: mutation.result,
                  queryName: (0, _apolloUtilities.getOperationName)(query.document) || undefined,
                  queryVariables: query.variables
                });
              });

              if (nextQueryResult) {
                cacheWrites_1.push({
                  result: nextQueryResult,
                  dataId: 'ROOT_QUERY',
                  query: query.document,
                  variables: query.variables
                });
              }
            }
          });
        }

        this.cache.performTransaction(function (c) {
          cacheWrites_1.forEach(function (write) {
            return c.write(write);
          });
          var update = mutation.update;

          if (update) {
            (0, _apolloUtilities.tryFunctionOrLogError)(function () {
              return update(c, mutation.result);
            });
          }
        });
      }
    };

    DataStore.prototype.markMutationComplete = function (_a) {
      var mutationId = _a.mutationId,
          optimisticResponse = _a.optimisticResponse;

      if (optimisticResponse) {
        this.cache.removeOptimistic(mutationId);
      }
    };

    DataStore.prototype.markUpdateQueryResult = function (document, variables, newResult) {
      this.cache.write({
        result: newResult,
        dataId: 'ROOT_QUERY',
        variables: variables,
        query: document
      });
    };

    DataStore.prototype.reset = function () {
      return this.cache.reset();
    };

    return DataStore;
  }();

  var version = "2.6.10";
  var hasSuggestedDevtools = false;

  var ApolloClient = function () {
    function ApolloClient(options) {
      var _this = this;

      this.defaultOptions = {};
      this.resetStoreCallbacks = [];
      this.clearStoreCallbacks = [];
      var cache = options.cache,
          _a = options.ssrMode,
          ssrMode = _a === void 0 ? false : _a,
          _b = options.ssrForceFetchDelay,
          ssrForceFetchDelay = _b === void 0 ? 0 : _b,
          connectToDevTools = options.connectToDevTools,
          _c = options.queryDeduplication,
          queryDeduplication = _c === void 0 ? true : _c,
          defaultOptions = options.defaultOptions,
          _d = options.assumeImmutableResults,
          assumeImmutableResults = _d === void 0 ? false : _d,
          resolvers = options.resolvers,
          typeDefs = options.typeDefs,
          fragmentMatcher = options.fragmentMatcher,
          clientAwarenessName = options.name,
          clientAwarenessVersion = options.version;
      var link = options.link;

      if (!link && resolvers) {
        link = _apolloLink.ApolloLink.empty();
      }

      if (!link || !cache) {
        throw process.env.NODE_ENV === "production" ? new _tsInvariant.InvariantError(4) : new _tsInvariant.InvariantError("In order to initialize Apollo Client, you must specify 'link' and 'cache' properties in the options object.\n" + "These options are part of the upgrade requirements when migrating from Apollo Client 1.x to Apollo Client 2.x.\n" + "For more information, please visit: https://www.apollographql.com/docs/tutorial/client.html#apollo-client-setup");
      }

      this.link = link;
      this.cache = cache;
      this.store = new DataStore(cache);
      this.disableNetworkFetches = ssrMode || ssrForceFetchDelay > 0;
      this.queryDeduplication = queryDeduplication;
      this.defaultOptions = defaultOptions || {};
      this.typeDefs = typeDefs;

      if (ssrForceFetchDelay) {
        setTimeout(function () {
          return _this.disableNetworkFetches = false;
        }, ssrForceFetchDelay);
      }

      this.watchQuery = this.watchQuery.bind(this);
      this.query = this.query.bind(this);
      this.mutate = this.mutate.bind(this);
      this.resetStore = this.resetStore.bind(this);
      this.reFetchObservableQueries = this.reFetchObservableQueries.bind(this);
      var defaultConnectToDevTools = process.env.NODE_ENV !== 'production' && typeof window !== 'undefined' && !window.__APOLLO_CLIENT__;

      if (typeof connectToDevTools === 'undefined' ? defaultConnectToDevTools : connectToDevTools && typeof window !== 'undefined') {
        window.__APOLLO_CLIENT__ = this;
      }

      if (!hasSuggestedDevtools && process.env.NODE_ENV !== 'production') {
        hasSuggestedDevtools = true;

        if (typeof window !== 'undefined' && window.document && window.top === window.self) {
          if (typeof window.__APOLLO_DEVTOOLS_GLOBAL_HOOK__ === 'undefined') {
            if (window.navigator && window.navigator.userAgent && window.navigator.userAgent.indexOf('Chrome') > -1) {
              console.debug('Download the Apollo DevTools ' + 'for a better development experience: ' + 'https://chrome.google.com/webstore/detail/apollo-client-developer-t/jdkknkkbebbapilgoeccciglkfbmbnfm');
            }
          }
        }
      }

      this.version = version;
      this.localState = new LocalState({
        cache: cache,
        client: this,
        resolvers: resolvers,
        fragmentMatcher: fragmentMatcher
      });
      this.queryManager = new QueryManager({
        link: this.link,
        store: this.store,
        queryDeduplication: queryDeduplication,
        ssrMode: ssrMode,
        clientAwareness: {
          name: clientAwarenessName,
          version: clientAwarenessVersion
        },
        localState: this.localState,
        assumeImmutableResults: assumeImmutableResults,
        onBroadcast: function () {
          if (_this.devToolsHookCb) {
            _this.devToolsHookCb({
              action: {},
              state: {
                queries: _this.queryManager.queryStore.getStore(),
                mutations: _this.queryManager.mutationStore.getStore()
              },
              dataWithOptimisticResults: _this.cache.extract(true)
            });
          }
        }
      });
    }

    ApolloClient.prototype.stop = function () {
      this.queryManager.stop();
    };

    ApolloClient.prototype.watchQuery = function (options) {
      if (this.defaultOptions.watchQuery) {
        options = (0, _tslib.__assign)((0, _tslib.__assign)({}, this.defaultOptions.watchQuery), options);
      }

      if (this.disableNetworkFetches && (options.fetchPolicy === 'network-only' || options.fetchPolicy === 'cache-and-network')) {
        options = (0, _tslib.__assign)((0, _tslib.__assign)({}, options), {
          fetchPolicy: 'cache-first'
        });
      }

      return this.queryManager.watchQuery(options);
    };

    ApolloClient.prototype.query = function (options) {
      if (this.defaultOptions.query) {
        options = (0, _tslib.__assign)((0, _tslib.__assign)({}, this.defaultOptions.query), options);
      }

      process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(options.fetchPolicy !== 'cache-and-network', 5) : (0, _tsInvariant.invariant)(options.fetchPolicy !== 'cache-and-network', 'The cache-and-network fetchPolicy does not work with client.query, because ' + 'client.query can only return a single result. Please use client.watchQuery ' + 'to receive multiple results from the cache and the network, or consider ' + 'using a different fetchPolicy, such as cache-first or network-only.');

      if (this.disableNetworkFetches && options.fetchPolicy === 'network-only') {
        options = (0, _tslib.__assign)((0, _tslib.__assign)({}, options), {
          fetchPolicy: 'cache-first'
        });
      }

      return this.queryManager.query(options);
    };

    ApolloClient.prototype.mutate = function (options) {
      if (this.defaultOptions.mutate) {
        options = (0, _tslib.__assign)((0, _tslib.__assign)({}, this.defaultOptions.mutate), options);
      }

      return this.queryManager.mutate(options);
    };

    ApolloClient.prototype.subscribe = function (options) {
      return this.queryManager.startGraphQLSubscription(options);
    };

    ApolloClient.prototype.readQuery = function (options, optimistic) {
      if (optimistic === void 0) {
        optimistic = false;
      }

      return this.cache.readQuery(options, optimistic);
    };

    ApolloClient.prototype.readFragment = function (options, optimistic) {
      if (optimistic === void 0) {
        optimistic = false;
      }

      return this.cache.readFragment(options, optimistic);
    };

    ApolloClient.prototype.writeQuery = function (options) {
      var result = this.cache.writeQuery(options);
      this.queryManager.broadcastQueries();
      return result;
    };

    ApolloClient.prototype.writeFragment = function (options) {
      var result = this.cache.writeFragment(options);
      this.queryManager.broadcastQueries();
      return result;
    };

    ApolloClient.prototype.writeData = function (options) {
      var result = this.cache.writeData(options);
      this.queryManager.broadcastQueries();
      return result;
    };

    ApolloClient.prototype.__actionHookForDevTools = function (cb) {
      this.devToolsHookCb = cb;
    };

    ApolloClient.prototype.__requestRaw = function (payload) {
      return (0, _apolloLink.execute)(this.link, payload);
    };

    ApolloClient.prototype.initQueryManager = function () {
      process.env.NODE_ENV === "production" || _tsInvariant.invariant.warn('Calling the initQueryManager method is no longer necessary, ' + 'and it will be removed from ApolloClient in version 3.0.');
      return this.queryManager;
    };

    ApolloClient.prototype.resetStore = function () {
      var _this = this;

      return Promise.resolve().then(function () {
        return _this.queryManager.clearStore();
      }).then(function () {
        return Promise.all(_this.resetStoreCallbacks.map(function (fn) {
          return fn();
        }));
      }).then(function () {
        return _this.reFetchObservableQueries();
      });
    };

    ApolloClient.prototype.clearStore = function () {
      var _this = this;

      return Promise.resolve().then(function () {
        return _this.queryManager.clearStore();
      }).then(function () {
        return Promise.all(_this.clearStoreCallbacks.map(function (fn) {
          return fn();
        }));
      });
    };

    ApolloClient.prototype.onResetStore = function (cb) {
      var _this = this;

      this.resetStoreCallbacks.push(cb);
      return function () {
        _this.resetStoreCallbacks = _this.resetStoreCallbacks.filter(function (c) {
          return c !== cb;
        });
      };
    };

    ApolloClient.prototype.onClearStore = function (cb) {
      var _this = this;

      this.clearStoreCallbacks.push(cb);
      return function () {
        _this.clearStoreCallbacks = _this.clearStoreCallbacks.filter(function (c) {
          return c !== cb;
        });
      };
    };

    ApolloClient.prototype.reFetchObservableQueries = function (includeStandby) {
      return this.queryManager.reFetchObservableQueries(includeStandby);
    };

    ApolloClient.prototype.extract = function (optimistic) {
      return this.cache.extract(optimistic);
    };

    ApolloClient.prototype.restore = function (serializedState) {
      return this.cache.restore(serializedState);
    };

    ApolloClient.prototype.addResolvers = function (resolvers) {
      this.localState.addResolvers(resolvers);
    };

    ApolloClient.prototype.setResolvers = function (resolvers) {
      this.localState.setResolvers(resolvers);
    };

    ApolloClient.prototype.getResolvers = function () {
      return this.localState.getResolvers();
    };

    ApolloClient.prototype.setLocalStateFragmentMatcher = function (fragmentMatcher) {
      this.localState.setFragmentMatcher(fragmentMatcher);
    };

    return ApolloClient;
  }();

  _exports.ApolloClient = ApolloClient;
  var _default = ApolloClient; 

  _exports.default = _default;
});

}).call(this)}).call(this,require('_process'))
},{"_process":146,"apollo-link":8,"apollo-utilities":11,"graphql/language/visitor":64,"symbol-observable":153,"ts-invariant":155,"tslib":156}],6:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var tslib_1 = require("tslib");
tslib_1.__exportStar(require("./webSocketLink"), exports);

},{"./webSocketLink":7,"tslib":156}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var tslib_1 = require("tslib");
var apollo_link_1 = require("apollo-link");
var subscriptions_transport_ws_1 = require("subscriptions-transport-ws");
var WebSocketLink = (function (_super) {
    tslib_1.__extends(WebSocketLink, _super);
    function WebSocketLink(paramsOrClient) {
        var _this = _super.call(this) || this;
        if (paramsOrClient instanceof subscriptions_transport_ws_1.SubscriptionClient) {
            _this.subscriptionClient = paramsOrClient;
        }
        else {
            _this.subscriptionClient = new subscriptions_transport_ws_1.SubscriptionClient(paramsOrClient.uri, paramsOrClient.options, paramsOrClient.webSocketImpl);
        }
        return _this;
    }
    WebSocketLink.prototype.request = function (operation) {
        return this.subscriptionClient.request(operation);
    };
    return WebSocketLink;
}(apollo_link_1.ApolloLink));
exports.WebSocketLink = WebSocketLink;

},{"apollo-link":8,"subscriptions-transport-ws":147,"tslib":156}],8:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var tslib_1 = require("tslib");
tslib_1.__exportStar(require("./link"), exports);
var linkUtils_1 = require("./linkUtils");
exports.createOperation = linkUtils_1.createOperation;
exports.makePromise = linkUtils_1.makePromise;
exports.toPromise = linkUtils_1.toPromise;
exports.fromPromise = linkUtils_1.fromPromise;
exports.fromError = linkUtils_1.fromError;
exports.getOperationName = linkUtils_1.getOperationName;
var zen_observable_ts_1 = tslib_1.__importDefault(require("zen-observable-ts"));
exports.Observable = zen_observable_ts_1.default;

},{"./link":9,"./linkUtils":10,"tslib":156,"zen-observable-ts":157}],9:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var tslib_1 = require("tslib");
var zen_observable_ts_1 = tslib_1.__importDefault(require("zen-observable-ts"));
var ts_invariant_1 = require("ts-invariant");
var linkUtils_1 = require("./linkUtils");
function passthrough(op, forward) {
    return forward ? forward(op) : zen_observable_ts_1.default.of();
}
function toLink(handler) {
    return typeof handler === 'function' ? new ApolloLink(handler) : handler;
}
function empty() {
    return new ApolloLink(function () { return zen_observable_ts_1.default.of(); });
}
exports.empty = empty;
function from(links) {
    if (links.length === 0)
        return empty();
    return links.map(toLink).reduce(function (x, y) { return x.concat(y); });
}
exports.from = from;
function split(test, left, right) {
    var leftLink = toLink(left);
    var rightLink = toLink(right || new ApolloLink(passthrough));
    if (linkUtils_1.isTerminating(leftLink) && linkUtils_1.isTerminating(rightLink)) {
        return new ApolloLink(function (operation) {
            return test(operation)
                ? leftLink.request(operation) || zen_observable_ts_1.default.of()
                : rightLink.request(operation) || zen_observable_ts_1.default.of();
        });
    }
    else {
        return new ApolloLink(function (operation, forward) {
            return test(operation)
                ? leftLink.request(operation, forward) || zen_observable_ts_1.default.of()
                : rightLink.request(operation, forward) || zen_observable_ts_1.default.of();
        });
    }
}
exports.split = split;
exports.concat = function (first, second) {
    var firstLink = toLink(first);
    if (linkUtils_1.isTerminating(firstLink)) {
        ts_invariant_1.invariant.warn(new linkUtils_1.LinkError("You are calling concat on a terminating link, which will have no effect", firstLink));
        return firstLink;
    }
    var nextLink = toLink(second);
    if (linkUtils_1.isTerminating(nextLink)) {
        return new ApolloLink(function (operation) {
            return firstLink.request(operation, function (op) { return nextLink.request(op) || zen_observable_ts_1.default.of(); }) || zen_observable_ts_1.default.of();
        });
    }
    else {
        return new ApolloLink(function (operation, forward) {
            return (firstLink.request(operation, function (op) {
                return nextLink.request(op, forward) || zen_observable_ts_1.default.of();
            }) || zen_observable_ts_1.default.of());
        });
    }
};
var ApolloLink = (function () {
    function ApolloLink(request) {
        if (request)
            this.request = request;
    }
    ApolloLink.prototype.split = function (test, left, right) {
        return this.concat(split(test, left, right || new ApolloLink(passthrough)));
    };
    ApolloLink.prototype.concat = function (next) {
        return exports.concat(this, next);
    };
    ApolloLink.prototype.request = function (operation, forward) {
        throw new ts_invariant_1.InvariantError('request is not implemented');
    };
    ApolloLink.empty = empty;
    ApolloLink.from = from;
    ApolloLink.split = split;
    ApolloLink.execute = execute;
    return ApolloLink;
}());
exports.ApolloLink = ApolloLink;
function execute(link, operation) {
    return (link.request(linkUtils_1.createOperation(operation.context, linkUtils_1.transformOperation(linkUtils_1.validateOperation(operation)))) || zen_observable_ts_1.default.of());
}
exports.execute = execute;

},{"./linkUtils":10,"ts-invariant":155,"tslib":156,"zen-observable-ts":157}],10:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var tslib_1 = require("tslib");
var zen_observable_ts_1 = tslib_1.__importDefault(require("zen-observable-ts"));
var apollo_utilities_1 = require("apollo-utilities");
exports.getOperationName = apollo_utilities_1.getOperationName;
var ts_invariant_1 = require("ts-invariant");
function validateOperation(operation) {
    var OPERATION_FIELDS = [
        'query',
        'operationName',
        'variables',
        'extensions',
        'context',
    ];
    for (var _i = 0, _a = Object.keys(operation); _i < _a.length; _i++) {
        var key = _a[_i];
        if (OPERATION_FIELDS.indexOf(key) < 0) {
            throw new ts_invariant_1.InvariantError("illegal argument: " + key);
        }
    }
    return operation;
}
exports.validateOperation = validateOperation;
var LinkError = (function (_super) {
    tslib_1.__extends(LinkError, _super);
    function LinkError(message, link) {
        var _this = _super.call(this, message) || this;
        _this.link = link;
        return _this;
    }
    return LinkError;
}(Error));
exports.LinkError = LinkError;
function isTerminating(link) {
    return link.request.length <= 1;
}
exports.isTerminating = isTerminating;
function toPromise(observable) {
    var completed = false;
    return new Promise(function (resolve, reject) {
        observable.subscribe({
            next: function (data) {
                if (completed) {
                    ts_invariant_1.invariant.warn("Promise Wrapper does not support multiple results from Observable");
                }
                else {
                    completed = true;
                    resolve(data);
                }
            },
            error: reject,
        });
    });
}
exports.toPromise = toPromise;
exports.makePromise = toPromise;
function fromPromise(promise) {
    return new zen_observable_ts_1.default(function (observer) {
        promise
            .then(function (value) {
            observer.next(value);
            observer.complete();
        })
            .catch(observer.error.bind(observer));
    });
}
exports.fromPromise = fromPromise;
function fromError(errorValue) {
    return new zen_observable_ts_1.default(function (observer) {
        observer.error(errorValue);
    });
}
exports.fromError = fromError;
function transformOperation(operation) {
    var transformedOperation = {
        variables: operation.variables || {},
        extensions: operation.extensions || {},
        operationName: operation.operationName,
        query: operation.query,
    };
    if (!transformedOperation.operationName) {
        transformedOperation.operationName =
            typeof transformedOperation.query !== 'string'
                ? apollo_utilities_1.getOperationName(transformedOperation.query)
                : '';
    }
    return transformedOperation;
}
exports.transformOperation = transformOperation;
function createOperation(starting, operation) {
    var context = tslib_1.__assign({}, starting);
    var setContext = function (next) {
        if (typeof next === 'function') {
            context = tslib_1.__assign({}, context, next(context));
        }
        else {
            context = tslib_1.__assign({}, context, next);
        }
    };
    var getContext = function () { return (tslib_1.__assign({}, context)); };
    Object.defineProperty(operation, 'setContext', {
        enumerable: false,
        value: setContext,
    });
    Object.defineProperty(operation, 'getContext', {
        enumerable: false,
        value: getContext,
    });
    Object.defineProperty(operation, 'toKey', {
        enumerable: false,
        value: function () { return getKey(operation); },
    });
    return operation;
}
exports.createOperation = createOperation;
function getKey(operation) {
    var query = operation.query, variables = operation.variables, operationName = operation.operationName;
    return JSON.stringify([operationName, query, variables]);
}
exports.getKey = getKey;

},{"apollo-utilities":11,"ts-invariant":155,"tslib":156,"zen-observable-ts":157}],11:[function(require,module,exports){
(function (process){(function (){
exports.__esModule = true;
exports.addTypenameToDocument = addTypenameToDocument;
exports.argumentsObjectFromField = argumentsObjectFromField;
exports.assign = assign;
exports.buildQueryFromSelectionSet = buildQueryFromSelectionSet;
exports.checkDocument = checkDocument;
exports.cloneDeep = cloneDeep;
exports.createFragmentMap = createFragmentMap;
exports.getDefaultValues = getDefaultValues;
exports.getDirectiveInfoFromField = getDirectiveInfoFromField;
exports.getDirectiveNames = getDirectiveNames;
exports.getDirectivesFromDocument = getDirectivesFromDocument;
exports.getEnv = getEnv;
exports.getFragmentDefinition = getFragmentDefinition;
exports.getFragmentDefinitions = getFragmentDefinitions;
exports.getFragmentQueryDocument = getFragmentQueryDocument;
exports.getInclusionDirectives = getInclusionDirectives;
exports.getMainDefinition = getMainDefinition;
exports.getMutationDefinition = getMutationDefinition;
exports.getOperationDefinition = getOperationDefinition;
exports.getOperationDefinitionOrDie = getOperationDefinitionOrDie;
exports.getOperationName = getOperationName;
exports.getQueryDefinition = getQueryDefinition;
exports.getStoreKeyName = getStoreKeyName;
exports.graphQLResultHasError = graphQLResultHasError;
exports.hasClientExports = hasClientExports;
exports.hasDirectives = hasDirectives;
exports.isDevelopment = isDevelopment;
exports.isEnv = isEnv;
exports.isField = isField;
exports.isIdValue = isIdValue;
exports.isInlineFragment = isInlineFragment;
exports.isJsonValue = isJsonValue;
exports.isNumberValue = isNumberValue;
exports.isProduction = isProduction;
exports.isScalarValue = isScalarValue;
exports.isTest = isTest;
exports.maybeDeepFreeze = maybeDeepFreeze;
exports.mergeDeep = mergeDeep;
exports.mergeDeepArray = mergeDeepArray;
exports.removeArgumentsFromDocument = removeArgumentsFromDocument;
exports.removeClientSetsFromDocument = removeClientSetsFromDocument;
exports.removeConnectionDirectiveFromDocument = removeConnectionDirectiveFromDocument;
exports.removeDirectivesFromDocument = removeDirectivesFromDocument;
exports.removeFragmentSpreadFromDocument = removeFragmentSpreadFromDocument;
exports.resultKeyNameFromField = resultKeyNameFromField;
exports.shouldInclude = shouldInclude;
exports.storeKeyNameFromField = storeKeyNameFromField;
exports.stripSymbols = stripSymbols;
exports.toIdValue = toIdValue;
exports.tryFunctionOrLogError = tryFunctionOrLogError;
exports.valueFromNode = valueFromNode;
exports.valueToObjectRepresentation = valueToObjectRepresentation;
exports.variablesInOperation = variablesInOperation;
exports.warnOnceInDevelopment = warnOnceInDevelopment;
exports.canUseWeakMap = exports.isEqual = void 0;

var _visitor = require("graphql/language/visitor");

var _tsInvariant = require("ts-invariant");

var _tslib = require("tslib");

var _fastJsonStableStringify = _interopRequireDefault(require("fast-json-stable-stringify"));

var _equality = require("@wry/equality");

exports.isEqual = _equality.equal;

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function isScalarValue(value) {
  return ['StringValue', 'BooleanValue', 'EnumValue'].indexOf(value.kind) > -1;
}

function isNumberValue(value) {
  return ['IntValue', 'FloatValue'].indexOf(value.kind) > -1;
}

function isStringValue(value) {
  return value.kind === 'StringValue';
}

function isBooleanValue(value) {
  return value.kind === 'BooleanValue';
}

function isIntValue(value) {
  return value.kind === 'IntValue';
}

function isFloatValue(value) {
  return value.kind === 'FloatValue';
}

function isVariable(value) {
  return value.kind === 'Variable';
}

function isObjectValue(value) {
  return value.kind === 'ObjectValue';
}

function isListValue(value) {
  return value.kind === 'ListValue';
}

function isEnumValue(value) {
  return value.kind === 'EnumValue';
}

function isNullValue(value) {
  return value.kind === 'NullValue';
}

function valueToObjectRepresentation(argObj, name, value, variables) {
  if (isIntValue(value) || isFloatValue(value)) {
    argObj[name.value] = Number(value.value);
  } else if (isBooleanValue(value) || isStringValue(value)) {
    argObj[name.value] = value.value;
  } else if (isObjectValue(value)) {
    var nestedArgObj_1 = {};
    value.fields.map(function (obj) {
      return valueToObjectRepresentation(nestedArgObj_1, obj.name, obj.value, variables);
    });
    argObj[name.value] = nestedArgObj_1;
  } else if (isVariable(value)) {
    var variableValue = (variables || {})[value.name.value];
    argObj[name.value] = variableValue;
  } else if (isListValue(value)) {
    argObj[name.value] = value.values.map(function (listValue) {
      var nestedArgArrayObj = {};
      valueToObjectRepresentation(nestedArgArrayObj, name, listValue, variables);
      return nestedArgArrayObj[name.value];
    });
  } else if (isEnumValue(value)) {
    argObj[name.value] = value.value;
  } else if (isNullValue(value)) {
    argObj[name.value] = null;
  } else {
    throw process.env.NODE_ENV === "production" ? new _tsInvariant.InvariantError(17) : new _tsInvariant.InvariantError("The inline argument \"" + name.value + "\" of kind \"" + value.kind + "\"" + 'is not supported. Use variables instead of inline arguments to ' + 'overcome this limitation.');
  }
}

function storeKeyNameFromField(field, variables) {
  var directivesObj = null;

  if (field.directives) {
    directivesObj = {};
    field.directives.forEach(function (directive) {
      directivesObj[directive.name.value] = {};

      if (directive.arguments) {
        directive.arguments.forEach(function (_a) {
          var name = _a.name,
              value = _a.value;
          return valueToObjectRepresentation(directivesObj[directive.name.value], name, value, variables);
        });
      }
    });
  }

  var argObj = null;

  if (field.arguments && field.arguments.length) {
    argObj = {};
    field.arguments.forEach(function (_a) {
      var name = _a.name,
          value = _a.value;
      return valueToObjectRepresentation(argObj, name, value, variables);
    });
  }

  return getStoreKeyName(field.name.value, argObj, directivesObj);
}

var KNOWN_DIRECTIVES = ['connection', 'include', 'skip', 'client', 'rest', 'export'];

function getStoreKeyName(fieldName, args, directives) {
  if (directives && directives['connection'] && directives['connection']['key']) {
    if (directives['connection']['filter'] && directives['connection']['filter'].length > 0) {
      var filterKeys = directives['connection']['filter'] ? directives['connection']['filter'] : [];
      filterKeys.sort();
      var queryArgs_1 = args;
      var filteredArgs_1 = {};
      filterKeys.forEach(function (key) {
        filteredArgs_1[key] = queryArgs_1[key];
      });
      return directives['connection']['key'] + "(" + JSON.stringify(filteredArgs_1) + ")";
    } else {
      return directives['connection']['key'];
    }
  }

  var completeFieldName = fieldName;

  if (args) {
    var stringifiedArgs = (0, _fastJsonStableStringify.default)(args);
    completeFieldName += "(" + stringifiedArgs + ")";
  }

  if (directives) {
    Object.keys(directives).forEach(function (key) {
      if (KNOWN_DIRECTIVES.indexOf(key) !== -1) return;

      if (directives[key] && Object.keys(directives[key]).length) {
        completeFieldName += "@" + key + "(" + JSON.stringify(directives[key]) + ")";
      } else {
        completeFieldName += "@" + key;
      }
    });
  }

  return completeFieldName;
}

function argumentsObjectFromField(field, variables) {
  if (field.arguments && field.arguments.length) {
    var argObj_1 = {};
    field.arguments.forEach(function (_a) {
      var name = _a.name,
          value = _a.value;
      return valueToObjectRepresentation(argObj_1, name, value, variables);
    });
    return argObj_1;
  }

  return null;
}

function resultKeyNameFromField(field) {
  return field.alias ? field.alias.value : field.name.value;
}

function isField(selection) {
  return selection.kind === 'Field';
}

function isInlineFragment(selection) {
  return selection.kind === 'InlineFragment';
}

function isIdValue(idObject) {
  return idObject && idObject.type === 'id' && typeof idObject.generated === 'boolean';
}

function toIdValue(idConfig, generated) {
  if (generated === void 0) {
    generated = false;
  }

  return (0, _tslib.__assign)({
    type: 'id',
    generated: generated
  }, typeof idConfig === 'string' ? {
    id: idConfig,
    typename: undefined
  } : idConfig);
}

function isJsonValue(jsonObject) {
  return jsonObject != null && typeof jsonObject === 'object' && jsonObject.type === 'json';
}

function defaultValueFromVariable(node) {
  throw process.env.NODE_ENV === "production" ? new _tsInvariant.InvariantError(18) : new _tsInvariant.InvariantError("Variable nodes are not supported by valueFromNode");
}

function valueFromNode(node, onVariable) {
  if (onVariable === void 0) {
    onVariable = defaultValueFromVariable;
  }

  switch (node.kind) {
    case 'Variable':
      return onVariable(node);

    case 'NullValue':
      return null;

    case 'IntValue':
      return parseInt(node.value, 10);

    case 'FloatValue':
      return parseFloat(node.value);

    case 'ListValue':
      return node.values.map(function (v) {
        return valueFromNode(v, onVariable);
      });

    case 'ObjectValue':
      {
        var value = {};

        for (var _i = 0, _a = node.fields; _i < _a.length; _i++) {
          var field = _a[_i];
          value[field.name.value] = valueFromNode(field.value, onVariable);
        }

        return value;
      }

    default:
      return node.value;
  }
}

function getDirectiveInfoFromField(field, variables) {
  if (field.directives && field.directives.length) {
    var directiveObj_1 = {};
    field.directives.forEach(function (directive) {
      directiveObj_1[directive.name.value] = argumentsObjectFromField(directive, variables);
    });
    return directiveObj_1;
  }

  return null;
}

function shouldInclude(selection, variables) {
  if (variables === void 0) {
    variables = {};
  }

  return getInclusionDirectives(selection.directives).every(function (_a) {
    var directive = _a.directive,
        ifArgument = _a.ifArgument;
    var evaledValue = false;

    if (ifArgument.value.kind === 'Variable') {
      evaledValue = variables[ifArgument.value.name.value];
      process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(evaledValue !== void 0, 13) : (0, _tsInvariant.invariant)(evaledValue !== void 0, "Invalid variable referenced in @" + directive.name.value + " directive.");
    } else {
      evaledValue = ifArgument.value.value;
    }

    return directive.name.value === 'skip' ? !evaledValue : evaledValue;
  });
}

function getDirectiveNames(doc) {
  var names = [];
  (0, _visitor.visit)(doc, {
    Directive: function (node) {
      names.push(node.name.value);
    }
  });
  return names;
}

function hasDirectives(names, doc) {
  return getDirectiveNames(doc).some(function (name) {
    return names.indexOf(name) > -1;
  });
}

function hasClientExports(document) {
  return document && hasDirectives(['client'], document) && hasDirectives(['export'], document);
}

function isInclusionDirective(_a) {
  var value = _a.name.value;
  return value === 'skip' || value === 'include';
}

function getInclusionDirectives(directives) {
  return directives ? directives.filter(isInclusionDirective).map(function (directive) {
    var directiveArguments = directive.arguments;
    var directiveName = directive.name.value;
    process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(directiveArguments && directiveArguments.length === 1, 14) : (0, _tsInvariant.invariant)(directiveArguments && directiveArguments.length === 1, "Incorrect number of arguments for the @" + directiveName + " directive.");
    var ifArgument = directiveArguments[0];
    process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(ifArgument.name && ifArgument.name.value === 'if', 15) : (0, _tsInvariant.invariant)(ifArgument.name && ifArgument.name.value === 'if', "Invalid argument for the @" + directiveName + " directive.");
    var ifValue = ifArgument.value;
    process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(ifValue && (ifValue.kind === 'Variable' || ifValue.kind === 'BooleanValue'), 16) : (0, _tsInvariant.invariant)(ifValue && (ifValue.kind === 'Variable' || ifValue.kind === 'BooleanValue'), "Argument for the @" + directiveName + " directive must be a variable or a boolean value.");
    return {
      directive: directive,
      ifArgument: ifArgument
    };
  }) : [];
}

function getFragmentQueryDocument(document, fragmentName) {
  var actualFragmentName = fragmentName;
  var fragments = [];
  document.definitions.forEach(function (definition) {
    if (definition.kind === 'OperationDefinition') {
      throw process.env.NODE_ENV === "production" ? new _tsInvariant.InvariantError(11) : new _tsInvariant.InvariantError("Found a " + definition.operation + " operation" + (definition.name ? " named '" + definition.name.value + "'" : '') + ". " + 'No operations are allowed when using a fragment as a query. Only fragments are allowed.');
    }

    if (definition.kind === 'FragmentDefinition') {
      fragments.push(definition);
    }
  });

  if (typeof actualFragmentName === 'undefined') {
    process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(fragments.length === 1, 12) : (0, _tsInvariant.invariant)(fragments.length === 1, "Found " + fragments.length + " fragments. `fragmentName` must be provided when there is not exactly 1 fragment.");
    actualFragmentName = fragments[0].name.value;
  }

  var query = (0, _tslib.__assign)((0, _tslib.__assign)({}, document), {
    definitions: (0, _tslib.__spreadArrays)([{
      kind: 'OperationDefinition',
      operation: 'query',
      selectionSet: {
        kind: 'SelectionSet',
        selections: [{
          kind: 'FragmentSpread',
          name: {
            kind: 'Name',
            value: actualFragmentName
          }
        }]
      }
    }], document.definitions)
  });
  return query;
}

function assign(target) {
  var sources = [];

  for (var _i = 1; _i < arguments.length; _i++) {
    sources[_i - 1] = arguments[_i];
  }

  sources.forEach(function (source) {
    if (typeof source === 'undefined' || source === null) {
      return;
    }

    Object.keys(source).forEach(function (key) {
      target[key] = source[key];
    });
  });
  return target;
}

function getMutationDefinition(doc) {
  checkDocument(doc);
  var mutationDef = doc.definitions.filter(function (definition) {
    return definition.kind === 'OperationDefinition' && definition.operation === 'mutation';
  })[0];
  process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(mutationDef, 1) : (0, _tsInvariant.invariant)(mutationDef, 'Must contain a mutation definition.');
  return mutationDef;
}

function checkDocument(doc) {
  process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(doc && doc.kind === 'Document', 2) : (0, _tsInvariant.invariant)(doc && doc.kind === 'Document', "Expecting a parsed GraphQL document. Perhaps you need to wrap the query string in a \"gql\" tag? http://docs.apollostack.com/apollo-client/core.html#gql");
  var operations = doc.definitions.filter(function (d) {
    return d.kind !== 'FragmentDefinition';
  }).map(function (definition) {
    if (definition.kind !== 'OperationDefinition') {
      throw process.env.NODE_ENV === "production" ? new _tsInvariant.InvariantError(3) : new _tsInvariant.InvariantError("Schema type definitions not allowed in queries. Found: \"" + definition.kind + "\"");
    }

    return definition;
  });
  process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(operations.length <= 1, 4) : (0, _tsInvariant.invariant)(operations.length <= 1, "Ambiguous GraphQL document: contains " + operations.length + " operations");
  return doc;
}

function getOperationDefinition(doc) {
  checkDocument(doc);
  return doc.definitions.filter(function (definition) {
    return definition.kind === 'OperationDefinition';
  })[0];
}

function getOperationDefinitionOrDie(document) {
  var def = getOperationDefinition(document);
  process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(def, 5) : (0, _tsInvariant.invariant)(def, "GraphQL document is missing an operation");
  return def;
}

function getOperationName(doc) {
  return doc.definitions.filter(function (definition) {
    return definition.kind === 'OperationDefinition' && definition.name;
  }).map(function (x) {
    return x.name.value;
  })[0] || null;
}

function getFragmentDefinitions(doc) {
  return doc.definitions.filter(function (definition) {
    return definition.kind === 'FragmentDefinition';
  });
}

function getQueryDefinition(doc) {
  var queryDef = getOperationDefinition(doc);
  process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(queryDef && queryDef.operation === 'query', 6) : (0, _tsInvariant.invariant)(queryDef && queryDef.operation === 'query', 'Must contain a query definition.');
  return queryDef;
}

function getFragmentDefinition(doc) {
  process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(doc.kind === 'Document', 7) : (0, _tsInvariant.invariant)(doc.kind === 'Document', "Expecting a parsed GraphQL document. Perhaps you need to wrap the query string in a \"gql\" tag? http://docs.apollostack.com/apollo-client/core.html#gql");
  process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(doc.definitions.length <= 1, 8) : (0, _tsInvariant.invariant)(doc.definitions.length <= 1, 'Fragment must have exactly one definition.');
  var fragmentDef = doc.definitions[0];
  process.env.NODE_ENV === "production" ? (0, _tsInvariant.invariant)(fragmentDef.kind === 'FragmentDefinition', 9) : (0, _tsInvariant.invariant)(fragmentDef.kind === 'FragmentDefinition', 'Must be a fragment definition.');
  return fragmentDef;
}

function getMainDefinition(queryDoc) {
  checkDocument(queryDoc);
  var fragmentDefinition;

  for (var _i = 0, _a = queryDoc.definitions; _i < _a.length; _i++) {
    var definition = _a[_i];

    if (definition.kind === 'OperationDefinition') {
      var operation = definition.operation;

      if (operation === 'query' || operation === 'mutation' || operation === 'subscription') {
        return definition;
      }
    }

    if (definition.kind === 'FragmentDefinition' && !fragmentDefinition) {
      fragmentDefinition = definition;
    }
  }

  if (fragmentDefinition) {
    return fragmentDefinition;
  }

  throw process.env.NODE_ENV === "production" ? new _tsInvariant.InvariantError(10) : new _tsInvariant.InvariantError('Expected a parsed GraphQL query with a query, mutation, subscription, or a fragment.');
}

function createFragmentMap(fragments) {
  if (fragments === void 0) {
    fragments = [];
  }

  var symTable = {};
  fragments.forEach(function (fragment) {
    symTable[fragment.name.value] = fragment;
  });
  return symTable;
}

function getDefaultValues(definition) {
  if (definition && definition.variableDefinitions && definition.variableDefinitions.length) {
    var defaultValues = definition.variableDefinitions.filter(function (_a) {
      var defaultValue = _a.defaultValue;
      return defaultValue;
    }).map(function (_a) {
      var variable = _a.variable,
          defaultValue = _a.defaultValue;
      var defaultValueObj = {};
      valueToObjectRepresentation(defaultValueObj, variable.name, defaultValue);
      return defaultValueObj;
    });
    return assign.apply(void 0, (0, _tslib.__spreadArrays)([{}], defaultValues));
  }

  return {};
}

function variablesInOperation(operation) {
  var names = new Set();

  if (operation.variableDefinitions) {
    for (var _i = 0, _a = operation.variableDefinitions; _i < _a.length; _i++) {
      var definition = _a[_i];
      names.add(definition.variable.name.value);
    }
  }

  return names;
}

function filterInPlace(array, test, context) {
  var target = 0;
  array.forEach(function (elem, i) {
    if (test.call(this, elem, i, array)) {
      array[target++] = elem;
    }
  }, context);
  array.length = target;
  return array;
}

var TYPENAME_FIELD = {
  kind: 'Field',
  name: {
    kind: 'Name',
    value: '__typename'
  }
};

function isEmpty(op, fragments) {
  return op.selectionSet.selections.every(function (selection) {
    return selection.kind === 'FragmentSpread' && isEmpty(fragments[selection.name.value], fragments);
  });
}

function nullIfDocIsEmpty(doc) {
  return isEmpty(getOperationDefinition(doc) || getFragmentDefinition(doc), createFragmentMap(getFragmentDefinitions(doc))) ? null : doc;
}

function getDirectiveMatcher(directives) {
  return function directiveMatcher(directive) {
    return directives.some(function (dir) {
      return dir.name && dir.name === directive.name.value || dir.test && dir.test(directive);
    });
  };
}

function removeDirectivesFromDocument(directives, doc) {
  var variablesInUse = Object.create(null);
  var variablesToRemove = [];
  var fragmentSpreadsInUse = Object.create(null);
  var fragmentSpreadsToRemove = [];
  var modifiedDoc = nullIfDocIsEmpty((0, _visitor.visit)(doc, {
    Variable: {
      enter: function (node, _key, parent) {
        if (parent.kind !== 'VariableDefinition') {
          variablesInUse[node.name.value] = true;
        }
      }
    },
    Field: {
      enter: function (node) {
        if (directives && node.directives) {
          var shouldRemoveField = directives.some(function (directive) {
            return directive.remove;
          });

          if (shouldRemoveField && node.directives && node.directives.some(getDirectiveMatcher(directives))) {
            if (node.arguments) {
              node.arguments.forEach(function (arg) {
                if (arg.value.kind === 'Variable') {
                  variablesToRemove.push({
                    name: arg.value.name.value
                  });
                }
              });
            }

            if (node.selectionSet) {
              getAllFragmentSpreadsFromSelectionSet(node.selectionSet).forEach(function (frag) {
                fragmentSpreadsToRemove.push({
                  name: frag.name.value
                });
              });
            }

            return null;
          }
        }
      }
    },
    FragmentSpread: {
      enter: function (node) {
        fragmentSpreadsInUse[node.name.value] = true;
      }
    },
    Directive: {
      enter: function (node) {
        if (getDirectiveMatcher(directives)(node)) {
          return null;
        }
      }
    }
  }));

  if (modifiedDoc && filterInPlace(variablesToRemove, function (v) {
    return !variablesInUse[v.name];
  }).length) {
    modifiedDoc = removeArgumentsFromDocument(variablesToRemove, modifiedDoc);
  }

  if (modifiedDoc && filterInPlace(fragmentSpreadsToRemove, function (fs) {
    return !fragmentSpreadsInUse[fs.name];
  }).length) {
    modifiedDoc = removeFragmentSpreadFromDocument(fragmentSpreadsToRemove, modifiedDoc);
  }

  return modifiedDoc;
}

function addTypenameToDocument(doc) {
  return (0, _visitor.visit)(checkDocument(doc), {
    SelectionSet: {
      enter: function (node, _key, parent) {
        if (parent && parent.kind === 'OperationDefinition') {
          return;
        }

        var selections = node.selections;

        if (!selections) {
          return;
        }

        var skip = selections.some(function (selection) {
          return isField(selection) && (selection.name.value === '__typename' || selection.name.value.lastIndexOf('__', 0) === 0);
        });

        if (skip) {
          return;
        }

        var field = parent;

        if (isField(field) && field.directives && field.directives.some(function (d) {
          return d.name.value === 'export';
        })) {
          return;
        }

        return (0, _tslib.__assign)((0, _tslib.__assign)({}, node), {
          selections: (0, _tslib.__spreadArrays)(selections, [TYPENAME_FIELD])
        });
      }
    }
  });
}

var connectionRemoveConfig = {
  test: function (directive) {
    var willRemove = directive.name.value === 'connection';

    if (willRemove) {
      if (!directive.arguments || !directive.arguments.some(function (arg) {
        return arg.name.value === 'key';
      })) {
        process.env.NODE_ENV === "production" || _tsInvariant.invariant.warn('Removing an @connection directive even though it does not have a key. ' + 'You may want to use the key parameter to specify a store key.');
      }
    }

    return willRemove;
  }
};

function removeConnectionDirectiveFromDocument(doc) {
  return removeDirectivesFromDocument([connectionRemoveConfig], checkDocument(doc));
}

function hasDirectivesInSelectionSet(directives, selectionSet, nestedCheck) {
  if (nestedCheck === void 0) {
    nestedCheck = true;
  }

  return selectionSet && selectionSet.selections && selectionSet.selections.some(function (selection) {
    return hasDirectivesInSelection(directives, selection, nestedCheck);
  });
}

function hasDirectivesInSelection(directives, selection, nestedCheck) {
  if (nestedCheck === void 0) {
    nestedCheck = true;
  }

  if (!isField(selection)) {
    return true;
  }

  if (!selection.directives) {
    return false;
  }

  return selection.directives.some(getDirectiveMatcher(directives)) || nestedCheck && hasDirectivesInSelectionSet(directives, selection.selectionSet, nestedCheck);
}

function getDirectivesFromDocument(directives, doc) {
  checkDocument(doc);
  var parentPath;
  return nullIfDocIsEmpty((0, _visitor.visit)(doc, {
    SelectionSet: {
      enter: function (node, _key, _parent, path) {
        var currentPath = path.join('-');

        if (!parentPath || currentPath === parentPath || !currentPath.startsWith(parentPath)) {
          if (node.selections) {
            var selectionsWithDirectives = node.selections.filter(function (selection) {
              return hasDirectivesInSelection(directives, selection);
            });

            if (hasDirectivesInSelectionSet(directives, node, false)) {
              parentPath = currentPath;
            }

            return (0, _tslib.__assign)((0, _tslib.__assign)({}, node), {
              selections: selectionsWithDirectives
            });
          } else {
            return null;
          }
        }
      }
    }
  }));
}

function getArgumentMatcher(config) {
  return function argumentMatcher(argument) {
    return config.some(function (aConfig) {
      return argument.value && argument.value.kind === 'Variable' && argument.value.name && (aConfig.name === argument.value.name.value || aConfig.test && aConfig.test(argument));
    });
  };
}

function removeArgumentsFromDocument(config, doc) {
  var argMatcher = getArgumentMatcher(config);
  return nullIfDocIsEmpty((0, _visitor.visit)(doc, {
    OperationDefinition: {
      enter: function (node) {
        return (0, _tslib.__assign)((0, _tslib.__assign)({}, node), {
          variableDefinitions: node.variableDefinitions.filter(function (varDef) {
            return !config.some(function (arg) {
              return arg.name === varDef.variable.name.value;
            });
          })
        });
      }
    },
    Field: {
      enter: function (node) {
        var shouldRemoveField = config.some(function (argConfig) {
          return argConfig.remove;
        });

        if (shouldRemoveField) {
          var argMatchCount_1 = 0;
          node.arguments.forEach(function (arg) {
            if (argMatcher(arg)) {
              argMatchCount_1 += 1;
            }
          });

          if (argMatchCount_1 === 1) {
            return null;
          }
        }
      }
    },
    Argument: {
      enter: function (node) {
        if (argMatcher(node)) {
          return null;
        }
      }
    }
  }));
}

function removeFragmentSpreadFromDocument(config, doc) {
  function enter(node) {
    if (config.some(function (def) {
      return def.name === node.name.value;
    })) {
      return null;
    }
  }

  return nullIfDocIsEmpty((0, _visitor.visit)(doc, {
    FragmentSpread: {
      enter: enter
    },
    FragmentDefinition: {
      enter: enter
    }
  }));
}

function getAllFragmentSpreadsFromSelectionSet(selectionSet) {
  var allFragments = [];
  selectionSet.selections.forEach(function (selection) {
    if ((isField(selection) || isInlineFragment(selection)) && selection.selectionSet) {
      getAllFragmentSpreadsFromSelectionSet(selection.selectionSet).forEach(function (frag) {
        return allFragments.push(frag);
      });
    } else if (selection.kind === 'FragmentSpread') {
      allFragments.push(selection);
    }
  });
  return allFragments;
}

function buildQueryFromSelectionSet(document) {
  var definition = getMainDefinition(document);
  var definitionOperation = definition.operation;

  if (definitionOperation === 'query') {
    return document;
  }

  var modifiedDoc = (0, _visitor.visit)(document, {
    OperationDefinition: {
      enter: function (node) {
        return (0, _tslib.__assign)((0, _tslib.__assign)({}, node), {
          operation: 'query'
        });
      }
    }
  });
  return modifiedDoc;
}

function removeClientSetsFromDocument(document) {
  checkDocument(document);
  var modifiedDoc = removeDirectivesFromDocument([{
    test: function (directive) {
      return directive.name.value === 'client';
    },
    remove: true
  }], document);

  if (modifiedDoc) {
    modifiedDoc = (0, _visitor.visit)(modifiedDoc, {
      FragmentDefinition: {
        enter: function (node) {
          if (node.selectionSet) {
            var isTypenameOnly = node.selectionSet.selections.every(function (selection) {
              return isField(selection) && selection.name.value === '__typename';
            });

            if (isTypenameOnly) {
              return null;
            }
          }
        }
      }
    });
  }

  return modifiedDoc;
}

var canUseWeakMap = typeof WeakMap === 'function' && !(typeof navigator === 'object' && navigator.product === 'ReactNative');
exports.canUseWeakMap = canUseWeakMap;
var toString = Object.prototype.toString;

function cloneDeep(value) {
  return cloneDeepHelper(value, new Map());
}

function cloneDeepHelper(val, seen) {
  switch (toString.call(val)) {
    case "[object Array]":
      {
        if (seen.has(val)) return seen.get(val);
        var copy_1 = val.slice(0);
        seen.set(val, copy_1);
        copy_1.forEach(function (child, i) {
          copy_1[i] = cloneDeepHelper(child, seen);
        });
        return copy_1;
      }

    case "[object Object]":
      {
        if (seen.has(val)) return seen.get(val);
        var copy_2 = Object.create(Object.getPrototypeOf(val));
        seen.set(val, copy_2);
        Object.keys(val).forEach(function (key) {
          copy_2[key] = cloneDeepHelper(val[key], seen);
        });
        return copy_2;
      }

    default:
      return val;
  }
}

function getEnv() {
  if (typeof process !== 'undefined' && process.env.NODE_ENV) {
    return process.env.NODE_ENV;
  }

  return 'development';
}

function isEnv(env) {
  return getEnv() === env;
}

function isProduction() {
  return isEnv('production') === true;
}

function isDevelopment() {
  return isEnv('development') === true;
}

function isTest() {
  return isEnv('test') === true;
}

function tryFunctionOrLogError(f) {
  try {
    return f();
  } catch (e) {
    if (console.error) {
      console.error(e);
    }
  }
}

function graphQLResultHasError(result) {
  return result.errors && result.errors.length;
}

function deepFreeze(o) {
  Object.freeze(o);
  Object.getOwnPropertyNames(o).forEach(function (prop) {
    if (o[prop] !== null && (typeof o[prop] === 'object' || typeof o[prop] === 'function') && !Object.isFrozen(o[prop])) {
      deepFreeze(o[prop]);
    }
  });
  return o;
}

function maybeDeepFreeze(obj) {
  if (isDevelopment() || isTest()) {
    var symbolIsPolyfilled = typeof Symbol === 'function' && typeof Symbol('') === 'string';

    if (!symbolIsPolyfilled) {
      return deepFreeze(obj);
    }
  }

  return obj;
}

var hasOwnProperty = Object.prototype.hasOwnProperty;

function mergeDeep() {
  var sources = [];

  for (var _i = 0; _i < arguments.length; _i++) {
    sources[_i] = arguments[_i];
  }

  return mergeDeepArray(sources);
}

function mergeDeepArray(sources) {
  var target = sources[0] || {};
  var count = sources.length;

  if (count > 1) {
    var pastCopies = [];
    target = shallowCopyForMerge(target, pastCopies);

    for (var i = 1; i < count; ++i) {
      target = mergeHelper(target, sources[i], pastCopies);
    }
  }

  return target;
}

function isObject(obj) {
  return obj !== null && typeof obj === 'object';
}

function mergeHelper(target, source, pastCopies) {
  if (isObject(source) && isObject(target)) {
    if (Object.isExtensible && !Object.isExtensible(target)) {
      target = shallowCopyForMerge(target, pastCopies);
    }

    Object.keys(source).forEach(function (sourceKey) {
      var sourceValue = source[sourceKey];

      if (hasOwnProperty.call(target, sourceKey)) {
        var targetValue = target[sourceKey];

        if (sourceValue !== targetValue) {
          target[sourceKey] = mergeHelper(shallowCopyForMerge(targetValue, pastCopies), sourceValue, pastCopies);
        }
      } else {
        target[sourceKey] = sourceValue;
      }
    });
    return target;
  }

  return source;
}

function shallowCopyForMerge(value, pastCopies) {
  if (value !== null && typeof value === 'object' && pastCopies.indexOf(value) < 0) {
    if (Array.isArray(value)) {
      value = value.slice(0);
    } else {
      value = (0, _tslib.__assign)({
        __proto__: Object.getPrototypeOf(value)
      }, value);
    }

    pastCopies.push(value);
  }

  return value;
}

var haveWarned = Object.create({});

function warnOnceInDevelopment(msg, type) {
  if (type === void 0) {
    type = 'warn';
  }

  if (!isProduction() && !haveWarned[msg]) {
    if (!isTest()) {
      haveWarned[msg] = true;
    }

    if (type === 'error') {
      console.error(msg);
    } else {
      console.warn(msg);
    }
  }
}

function stripSymbols(data) {
  return JSON.parse(JSON.stringify(data));
}

}).call(this)}).call(this,require('_process'))
},{"@wry/equality":2,"_process":146,"fast-json-stable-stringify":14,"graphql/language/visitor":64,"ts-invariant":155,"tslib":156}],12:[function(require,module,exports){

/**
 * Expose `Backoff`.
 */

module.exports = Backoff;

/**
 * Initialize backoff timer with `opts`.
 *
 * - `min` initial timeout in milliseconds [100]
 * - `max` max timeout [10000]
 * - `jitter` [0]
 * - `factor` [2]
 *
 * @param {Object} opts
 * @api public
 */

function Backoff(opts) {
  opts = opts || {};
  this.ms = opts.min || 100;
  this.max = opts.max || 10000;
  this.factor = opts.factor || 2;
  this.jitter = opts.jitter > 0 && opts.jitter <= 1 ? opts.jitter : 0;
  this.attempts = 0;
}

/**
 * Return the backoff duration.
 *
 * @return {Number}
 * @api public
 */

Backoff.prototype.duration = function(){
  var ms = this.ms * Math.pow(this.factor, this.attempts++);
  if (this.jitter) {
    var rand =  Math.random();
    var deviation = Math.floor(rand * this.jitter * ms);
    ms = (Math.floor(rand * 10) & 1) == 0  ? ms - deviation : ms + deviation;
  }
  return Math.min(ms, this.max) | 0;
};

/**
 * Reset the number of attempts.
 *
 * @api public
 */

Backoff.prototype.reset = function(){
  this.attempts = 0;
};

/**
 * Set the minimum duration
 *
 * @api public
 */

Backoff.prototype.setMin = function(min){
  this.ms = min;
};

/**
 * Set the maximum duration
 *
 * @api public
 */

Backoff.prototype.setMax = function(max){
  this.max = max;
};

/**
 * Set the jitter
 *
 * @api public
 */

Backoff.prototype.setJitter = function(jitter){
  this.jitter = jitter;
};


},{}],13:[function(require,module,exports){
'use strict';

var has = Object.prototype.hasOwnProperty
  , prefix = '~';

/**
 * Constructor to create a storage for our `EE` objects.
 * An `Events` instance is a plain object whose properties are event names.
 *
 * @constructor
 * @private
 */
function Events() {}

//
// We try to not inherit from `Object.prototype`. In some engines creating an
// instance in this way is faster than calling `Object.create(null)` directly.
// If `Object.create(null)` is not supported we prefix the event names with a
// character to make sure that the built-in object properties are not
// overridden or used as an attack vector.
//
if (Object.create) {
  Events.prototype = Object.create(null);

  //
  // This hack is needed because the `__proto__` property is still inherited in
  // some old browsers like Android 4, iPhone 5.1, Opera 11 and Safari 5.
  //
  if (!new Events().__proto__) prefix = false;
}

/**
 * Representation of a single event listener.
 *
 * @param {Function} fn The listener function.
 * @param {*} context The context to invoke the listener with.
 * @param {Boolean} [once=false] Specify if the listener is a one-time listener.
 * @constructor
 * @private
 */
function EE(fn, context, once) {
  this.fn = fn;
  this.context = context;
  this.once = once || false;
}

/**
 * Add a listener for a given event.
 *
 * @param {EventEmitter} emitter Reference to the `EventEmitter` instance.
 * @param {(String|Symbol)} event The event name.
 * @param {Function} fn The listener function.
 * @param {*} context The context to invoke the listener with.
 * @param {Boolean} once Specify if the listener is a one-time listener.
 * @returns {EventEmitter}
 * @private
 */
function addListener(emitter, event, fn, context, once) {
  if (typeof fn !== 'function') {
    throw new TypeError('The listener must be a function');
  }

  var listener = new EE(fn, context || emitter, once)
    , evt = prefix ? prefix + event : event;

  if (!emitter._events[evt]) emitter._events[evt] = listener, emitter._eventsCount++;
  else if (!emitter._events[evt].fn) emitter._events[evt].push(listener);
  else emitter._events[evt] = [emitter._events[evt], listener];

  return emitter;
}

/**
 * Clear event by name.
 *
 * @param {EventEmitter} emitter Reference to the `EventEmitter` instance.
 * @param {(String|Symbol)} evt The Event name.
 * @private
 */
function clearEvent(emitter, evt) {
  if (--emitter._eventsCount === 0) emitter._events = new Events();
  else delete emitter._events[evt];
}

/**
 * Minimal `EventEmitter` interface that is molded against the Node.js
 * `EventEmitter` interface.
 *
 * @constructor
 * @public
 */
function EventEmitter() {
  this._events = new Events();
  this._eventsCount = 0;
}

/**
 * Return an array listing the events for which the emitter has registered
 * listeners.
 *
 * @returns {Array}
 * @public
 */
EventEmitter.prototype.eventNames = function eventNames() {
  var names = []
    , events
    , name;

  if (this._eventsCount === 0) return names;

  for (name in (events = this._events)) {
    if (has.call(events, name)) names.push(prefix ? name.slice(1) : name);
  }

  if (Object.getOwnPropertySymbols) {
    return names.concat(Object.getOwnPropertySymbols(events));
  }

  return names;
};

/**
 * Return the listeners registered for a given event.
 *
 * @param {(String|Symbol)} event The event name.
 * @returns {Array} The registered listeners.
 * @public
 */
EventEmitter.prototype.listeners = function listeners(event) {
  var evt = prefix ? prefix + event : event
    , handlers = this._events[evt];

  if (!handlers) return [];
  if (handlers.fn) return [handlers.fn];

  for (var i = 0, l = handlers.length, ee = new Array(l); i < l; i++) {
    ee[i] = handlers[i].fn;
  }

  return ee;
};

/**
 * Return the number of listeners listening to a given event.
 *
 * @param {(String|Symbol)} event The event name.
 * @returns {Number} The number of listeners.
 * @public
 */
EventEmitter.prototype.listenerCount = function listenerCount(event) {
  var evt = prefix ? prefix + event : event
    , listeners = this._events[evt];

  if (!listeners) return 0;
  if (listeners.fn) return 1;
  return listeners.length;
};

/**
 * Calls each of the listeners registered for a given event.
 *
 * @param {(String|Symbol)} event The event name.
 * @returns {Boolean} `true` if the event had listeners, else `false`.
 * @public
 */
EventEmitter.prototype.emit = function emit(event, a1, a2, a3, a4, a5) {
  var evt = prefix ? prefix + event : event;

  if (!this._events[evt]) return false;

  var listeners = this._events[evt]
    , len = arguments.length
    , args
    , i;

  if (listeners.fn) {
    if (listeners.once) this.removeListener(event, listeners.fn, undefined, true);

    switch (len) {
      case 1: return listeners.fn.call(listeners.context), true;
      case 2: return listeners.fn.call(listeners.context, a1), true;
      case 3: return listeners.fn.call(listeners.context, a1, a2), true;
      case 4: return listeners.fn.call(listeners.context, a1, a2, a3), true;
      case 5: return listeners.fn.call(listeners.context, a1, a2, a3, a4), true;
      case 6: return listeners.fn.call(listeners.context, a1, a2, a3, a4, a5), true;
    }

    for (i = 1, args = new Array(len -1); i < len; i++) {
      args[i - 1] = arguments[i];
    }

    listeners.fn.apply(listeners.context, args);
  } else {
    var length = listeners.length
      , j;

    for (i = 0; i < length; i++) {
      if (listeners[i].once) this.removeListener(event, listeners[i].fn, undefined, true);

      switch (len) {
        case 1: listeners[i].fn.call(listeners[i].context); break;
        case 2: listeners[i].fn.call(listeners[i].context, a1); break;
        case 3: listeners[i].fn.call(listeners[i].context, a1, a2); break;
        case 4: listeners[i].fn.call(listeners[i].context, a1, a2, a3); break;
        default:
          if (!args) for (j = 1, args = new Array(len -1); j < len; j++) {
            args[j - 1] = arguments[j];
          }

          listeners[i].fn.apply(listeners[i].context, args);
      }
    }
  }

  return true;
};

/**
 * Add a listener for a given event.
 *
 * @param {(String|Symbol)} event The event name.
 * @param {Function} fn The listener function.
 * @param {*} [context=this] The context to invoke the listener with.
 * @returns {EventEmitter} `this`.
 * @public
 */
EventEmitter.prototype.on = function on(event, fn, context) {
  return addListener(this, event, fn, context, false);
};

/**
 * Add a one-time listener for a given event.
 *
 * @param {(String|Symbol)} event The event name.
 * @param {Function} fn The listener function.
 * @param {*} [context=this] The context to invoke the listener with.
 * @returns {EventEmitter} `this`.
 * @public
 */
EventEmitter.prototype.once = function once(event, fn, context) {
  return addListener(this, event, fn, context, true);
};

/**
 * Remove the listeners of a given event.
 *
 * @param {(String|Symbol)} event The event name.
 * @param {Function} fn Only remove the listeners that match this function.
 * @param {*} context Only remove the listeners that have this context.
 * @param {Boolean} once Only remove one-time listeners.
 * @returns {EventEmitter} `this`.
 * @public
 */
EventEmitter.prototype.removeListener = function removeListener(event, fn, context, once) {
  var evt = prefix ? prefix + event : event;

  if (!this._events[evt]) return this;
  if (!fn) {
    clearEvent(this, evt);
    return this;
  }

  var listeners = this._events[evt];

  if (listeners.fn) {
    if (
      listeners.fn === fn &&
      (!once || listeners.once) &&
      (!context || listeners.context === context)
    ) {
      clearEvent(this, evt);
    }
  } else {
    for (var i = 0, events = [], length = listeners.length; i < length; i++) {
      if (
        listeners[i].fn !== fn ||
        (once && !listeners[i].once) ||
        (context && listeners[i].context !== context)
      ) {
        events.push(listeners[i]);
      }
    }

    //
    // Reset the array, or remove it completely if we have no more listeners.
    //
    if (events.length) this._events[evt] = events.length === 1 ? events[0] : events;
    else clearEvent(this, evt);
  }

  return this;
};

/**
 * Remove all listeners, or those of the specified event.
 *
 * @param {(String|Symbol)} [event] The event name.
 * @returns {EventEmitter} `this`.
 * @public
 */
EventEmitter.prototype.removeAllListeners = function removeAllListeners(event) {
  var evt;

  if (event) {
    evt = prefix ? prefix + event : event;
    if (this._events[evt]) clearEvent(this, evt);
  } else {
    this._events = new Events();
    this._eventsCount = 0;
  }

  return this;
};

//
// Alias methods names because people roll like that.
//
EventEmitter.prototype.off = EventEmitter.prototype.removeListener;
EventEmitter.prototype.addListener = EventEmitter.prototype.on;

//
// Expose the prefix.
//
EventEmitter.prefixed = prefix;

//
// Allow `EventEmitter` to be imported as module namespace.
//
EventEmitter.EventEmitter = EventEmitter;

//
// Expose the module.
//
if ('undefined' !== typeof module) {
  module.exports = EventEmitter;
}

},{}],14:[function(require,module,exports){
'use strict';

module.exports = function (data, opts) {
    if (!opts) opts = {};
    if (typeof opts === 'function') opts = { cmp: opts };
    var cycles = (typeof opts.cycles === 'boolean') ? opts.cycles : false;

    var cmp = opts.cmp && (function (f) {
        return function (node) {
            return function (a, b) {
                var aobj = { key: a, value: node[a] };
                var bobj = { key: b, value: node[b] };
                return f(aobj, bobj);
            };
        };
    })(opts.cmp);

    var seen = [];
    return (function stringify (node) {
        if (node && node.toJSON && typeof node.toJSON === 'function') {
            node = node.toJSON();
        }

        if (node === undefined) return;
        if (typeof node == 'number') return isFinite(node) ? '' + node : 'null';
        if (typeof node !== 'object') return JSON.stringify(node);

        var i, out;
        if (Array.isArray(node)) {
            out = '[';
            for (i = 0; i < node.length; i++) {
                if (i) out += ',';
                out += stringify(node[i]) || 'null';
            }
            return out + ']';
        }

        if (node === null) return 'null';

        if (seen.indexOf(node) !== -1) {
            if (cycles) return JSON.stringify('__cycle__');
            throw new TypeError('Converting circular structure to JSON');
        }

        var seenIndex = seen.push(node) - 1;
        var keys = Object.keys(node).sort(cmp && cmp(node));
        out = '';
        for (i = 0; i < keys.length; i++) {
            var key = keys[i];
            var value = stringify(node[key]);

            if (!value) continue;
            if (out) out += ',';
            out += JSON.stringify(key) + ':' + value;
        }
        seen.splice(seenIndex, 1);
        return '{' + out + '}';
    })(data);
};

},{}],15:[function(require,module,exports){
(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports, require('tslib'), require('graphql')) :
    typeof define === 'function' && define.amd ? define(['exports', 'tslib', 'graphql'], factory) :
    (global = typeof globalThis !== 'undefined' ? globalThis : global || self, factory(global['graphql-tag'] = {}, global.tslib, global.graphql));
}(this, (function (exports, tslib, graphql) { 'use strict';

    var docCache = new Map();
    var fragmentSourceMap = new Map();
    var printFragmentWarnings = true;
    var experimentalFragmentVariables = false;
    function normalize(string) {
        return string.replace(/[\s,]+/g, ' ').trim();
    }
    function cacheKeyFromLoc(loc) {
        return normalize(loc.source.body.substring(loc.start, loc.end));
    }
    function processFragments(ast) {
        var seenKeys = new Set();
        var definitions = [];
        ast.definitions.forEach(function (fragmentDefinition) {
            if (fragmentDefinition.kind === 'FragmentDefinition') {
                var fragmentName = fragmentDefinition.name.value;
                var sourceKey = cacheKeyFromLoc(fragmentDefinition.loc);
                var sourceKeySet = fragmentSourceMap.get(fragmentName);
                if (sourceKeySet && !sourceKeySet.has(sourceKey)) {
                    if (printFragmentWarnings) {
                        console.warn("Warning: fragment with name " + fragmentName + " already exists.\n"
                            + "graphql-tag enforces all fragment names across your application to be unique; read more about\n"
                            + "this in the docs: http://dev.apollodata.com/core/fragments.html#unique-names");
                    }
                }
                else if (!sourceKeySet) {
                    fragmentSourceMap.set(fragmentName, sourceKeySet = new Set);
                }
                sourceKeySet.add(sourceKey);
                if (!seenKeys.has(sourceKey)) {
                    seenKeys.add(sourceKey);
                    definitions.push(fragmentDefinition);
                }
            }
            else {
                definitions.push(fragmentDefinition);
            }
        });
        return tslib.__assign(tslib.__assign({}, ast), { definitions: definitions });
    }
    function stripLoc(doc) {
        var workSet = new Set(doc.definitions);
        workSet.forEach(function (node) {
            if (node.loc)
                delete node.loc;
            Object.keys(node).forEach(function (key) {
                var value = node[key];
                if (value && typeof value === 'object') {
                    workSet.add(value);
                }
            });
        });
        var loc = doc.loc;
        if (loc) {
            delete loc.startToken;
            delete loc.endToken;
        }
        return doc;
    }
    function parseDocument(source) {
        var cacheKey = normalize(source);
        if (!docCache.has(cacheKey)) {
            var parsed = graphql.parse(source, {
                experimentalFragmentVariables: experimentalFragmentVariables
            });
            if (!parsed || parsed.kind !== 'Document') {
                throw new Error('Not a valid GraphQL document.');
            }
            docCache.set(cacheKey, stripLoc(processFragments(parsed)));
        }
        return docCache.get(cacheKey);
    }
    function gql(literals) {
        var args = [];
        for (var _i = 1; _i < arguments.length; _i++) {
            args[_i - 1] = arguments[_i];
        }
        if (typeof literals === 'string') {
            literals = [literals];
        }
        var result = literals[0];
        args.forEach(function (arg, i) {
            if (arg && arg.kind === 'Document') {
                result += arg.loc.source.body;
            }
            else {
                result += arg;
            }
            result += literals[i + 1];
        });
        return parseDocument(result);
    }
    function resetCaches() {
        docCache.clear();
        fragmentSourceMap.clear();
    }
    function disableFragmentWarnings() {
        printFragmentWarnings = false;
    }
    function enableExperimentalFragmentVariables() {
        experimentalFragmentVariables = true;
    }
    function disableExperimentalFragmentVariables() {
        experimentalFragmentVariables = false;
    }
    var extras = {
        gql: gql,
        resetCaches: resetCaches,
        disableFragmentWarnings: disableFragmentWarnings,
        enableExperimentalFragmentVariables: enableExperimentalFragmentVariables,
        disableExperimentalFragmentVariables: disableExperimentalFragmentVariables
    };
    (function (gql_1) {
        gql_1.gql = extras.gql, gql_1.resetCaches = extras.resetCaches, gql_1.disableFragmentWarnings = extras.disableFragmentWarnings, gql_1.enableExperimentalFragmentVariables = extras.enableExperimentalFragmentVariables, gql_1.disableExperimentalFragmentVariables = extras.disableExperimentalFragmentVariables;
    })(gql || (gql = {}));
    gql["default"] = gql;
    var gql$1 = gql;

    exports.default = gql$1;
    exports.disableExperimentalFragmentVariables = disableExperimentalFragmentVariables;
    exports.disableFragmentWarnings = disableFragmentWarnings;
    exports.enableExperimentalFragmentVariables = enableExperimentalFragmentVariables;
    exports.gql = gql;
    exports.resetCaches = resetCaches;

    Object.defineProperty(exports, '__esModule', { value: true });

})));


},{"graphql":27,"tslib":17}],16:[function(require,module,exports){
// For backwards compatibility, make sure require("graphql-tag") returns
// the gql function, rather than an exports object.
module.exports = require('./lib/graphql-tag.umd.js').gql;

},{"./lib/graphql-tag.umd.js":15}],17:[function(require,module,exports){
(function (global){(function (){
/*! *****************************************************************************
Copyright (c) Microsoft Corporation.

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
***************************************************************************** */
/* global global, define, System, Reflect, Promise */
var __extends;
var __assign;
var __rest;
var __decorate;
var __param;
var __metadata;
var __awaiter;
var __generator;
var __exportStar;
var __values;
var __read;
var __spread;
var __spreadArrays;
var __spreadArray;
var __await;
var __asyncGenerator;
var __asyncDelegator;
var __asyncValues;
var __makeTemplateObject;
var __importStar;
var __importDefault;
var __classPrivateFieldGet;
var __classPrivateFieldSet;
var __createBinding;
(function (factory) {
    var root = typeof global === "object" ? global : typeof self === "object" ? self : typeof this === "object" ? this : {};
    if (typeof define === "function" && define.amd) {
        define("tslib", ["exports"], function (exports) { factory(createExporter(root, createExporter(exports))); });
    }
    else if (typeof module === "object" && typeof module.exports === "object") {
        factory(createExporter(root, createExporter(module.exports)));
    }
    else {
        factory(createExporter(root));
    }
    function createExporter(exports, previous) {
        if (exports !== root) {
            if (typeof Object.create === "function") {
                Object.defineProperty(exports, "__esModule", { value: true });
            }
            else {
                exports.__esModule = true;
            }
        }
        return function (id, v) { return exports[id] = previous ? previous(id, v) : v; };
    }
})
(function (exporter) {
    var extendStatics = Object.setPrototypeOf ||
        ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
        function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };

    __extends = function (d, b) {
        if (typeof b !== "function" && b !== null)
            throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };

    __assign = Object.assign || function (t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p)) t[p] = s[p];
        }
        return t;
    };

    __rest = function (s, e) {
        var t = {};
        for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
            t[p] = s[p];
        if (s != null && typeof Object.getOwnPropertySymbols === "function")
            for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
                if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
                    t[p[i]] = s[p[i]];
            }
        return t;
    };

    __decorate = function (decorators, target, key, desc) {
        var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
        if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
        else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
        return c > 3 && r && Object.defineProperty(target, key, r), r;
    };

    __param = function (paramIndex, decorator) {
        return function (target, key) { decorator(target, key, paramIndex); }
    };

    __metadata = function (metadataKey, metadataValue) {
        if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(metadataKey, metadataValue);
    };

    __awaiter = function (thisArg, _arguments, P, generator) {
        function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
        return new (P || (P = Promise))(function (resolve, reject) {
            function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
            function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
            function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
            step((generator = generator.apply(thisArg, _arguments || [])).next());
        });
    };

    __generator = function (thisArg, body) {
        var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
        return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
        function verb(n) { return function (v) { return step([n, v]); }; }
        function step(op) {
            if (f) throw new TypeError("Generator is already executing.");
            while (_) try {
                if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
                if (y = 0, t) op = [op[0] & 2, t.value];
                switch (op[0]) {
                    case 0: case 1: t = op; break;
                    case 4: _.label++; return { value: op[1], done: false };
                    case 5: _.label++; y = op[1]; op = [0]; continue;
                    case 7: op = _.ops.pop(); _.trys.pop(); continue;
                    default:
                        if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                        if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                        if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                        if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                        if (t[2]) _.ops.pop();
                        _.trys.pop(); continue;
                }
                op = body.call(thisArg, _);
            } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
            if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
        }
    };

    __exportStar = function(m, o) {
        for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(o, p)) __createBinding(o, m, p);
    };

    __createBinding = Object.create ? (function(o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
    }) : (function(o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
    });

    __values = function (o) {
        var s = typeof Symbol === "function" && Symbol.iterator, m = s && o[s], i = 0;
        if (m) return m.call(o);
        if (o && typeof o.length === "number") return {
            next: function () {
                if (o && i >= o.length) o = void 0;
                return { value: o && o[i++], done: !o };
            }
        };
        throw new TypeError(s ? "Object is not iterable." : "Symbol.iterator is not defined.");
    };

    __read = function (o, n) {
        var m = typeof Symbol === "function" && o[Symbol.iterator];
        if (!m) return o;
        var i = m.call(o), r, ar = [], e;
        try {
            while ((n === void 0 || n-- > 0) && !(r = i.next()).done) ar.push(r.value);
        }
        catch (error) { e = { error: error }; }
        finally {
            try {
                if (r && !r.done && (m = i["return"])) m.call(i);
            }
            finally { if (e) throw e.error; }
        }
        return ar;
    };

    /** @deprecated */
    __spread = function () {
        for (var ar = [], i = 0; i < arguments.length; i++)
            ar = ar.concat(__read(arguments[i]));
        return ar;
    };

    /** @deprecated */
    __spreadArrays = function () {
        for (var s = 0, i = 0, il = arguments.length; i < il; i++) s += arguments[i].length;
        for (var r = Array(s), k = 0, i = 0; i < il; i++)
            for (var a = arguments[i], j = 0, jl = a.length; j < jl; j++, k++)
                r[k] = a[j];
        return r;
    };

    __spreadArray = function (to, from) {
        for (var i = 0, il = from.length, j = to.length; i < il; i++, j++)
            to[j] = from[i];
        return to;
    };

    __await = function (v) {
        return this instanceof __await ? (this.v = v, this) : new __await(v);
    };

    __asyncGenerator = function (thisArg, _arguments, generator) {
        if (!Symbol.asyncIterator) throw new TypeError("Symbol.asyncIterator is not defined.");
        var g = generator.apply(thisArg, _arguments || []), i, q = [];
        return i = {}, verb("next"), verb("throw"), verb("return"), i[Symbol.asyncIterator] = function () { return this; }, i;
        function verb(n) { if (g[n]) i[n] = function (v) { return new Promise(function (a, b) { q.push([n, v, a, b]) > 1 || resume(n, v); }); }; }
        function resume(n, v) { try { step(g[n](v)); } catch (e) { settle(q[0][3], e); } }
        function step(r) { r.value instanceof __await ? Promise.resolve(r.value.v).then(fulfill, reject) : settle(q[0][2], r);  }
        function fulfill(value) { resume("next", value); }
        function reject(value) { resume("throw", value); }
        function settle(f, v) { if (f(v), q.shift(), q.length) resume(q[0][0], q[0][1]); }
    };

    __asyncDelegator = function (o) {
        var i, p;
        return i = {}, verb("next"), verb("throw", function (e) { throw e; }), verb("return"), i[Symbol.iterator] = function () { return this; }, i;
        function verb(n, f) { i[n] = o[n] ? function (v) { return (p = !p) ? { value: __await(o[n](v)), done: n === "return" } : f ? f(v) : v; } : f; }
    };

    __asyncValues = function (o) {
        if (!Symbol.asyncIterator) throw new TypeError("Symbol.asyncIterator is not defined.");
        var m = o[Symbol.asyncIterator], i;
        return m ? m.call(o) : (o = typeof __values === "function" ? __values(o) : o[Symbol.iterator](), i = {}, verb("next"), verb("throw"), verb("return"), i[Symbol.asyncIterator] = function () { return this; }, i);
        function verb(n) { i[n] = o[n] && function (v) { return new Promise(function (resolve, reject) { v = o[n](v), settle(resolve, reject, v.done, v.value); }); }; }
        function settle(resolve, reject, d, v) { Promise.resolve(v).then(function(v) { resolve({ value: v, done: d }); }, reject); }
    };

    __makeTemplateObject = function (cooked, raw) {
        if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
        return cooked;
    };

    var __setModuleDefault = Object.create ? (function(o, v) {
        Object.defineProperty(o, "default", { enumerable: true, value: v });
    }) : function(o, v) {
        o["default"] = v;
    };

    __importStar = function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
        __setModuleDefault(result, mod);
        return result;
    };

    __importDefault = function (mod) {
        return (mod && mod.__esModule) ? mod : { "default": mod };
    };

    __classPrivateFieldGet = function (receiver, state, kind, f) {
        if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a getter");
        if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot read private member from an object whose class did not declare it");
        return kind === "m" ? f : kind === "a" ? f.call(receiver) : f ? f.value : state.get(receiver);
    };

    __classPrivateFieldSet = function (receiver, state, value, kind, f) {
        if (kind === "m") throw new TypeError("Private method is not writable");
        if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a setter");
        if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot write private member to an object whose class did not declare it");
        return (kind === "a" ? f.call(receiver, value) : f ? f.value = value : state.set(receiver, value)), value;
    };

    exporter("__extends", __extends);
    exporter("__assign", __assign);
    exporter("__rest", __rest);
    exporter("__decorate", __decorate);
    exporter("__param", __param);
    exporter("__metadata", __metadata);
    exporter("__awaiter", __awaiter);
    exporter("__generator", __generator);
    exporter("__exportStar", __exportStar);
    exporter("__createBinding", __createBinding);
    exporter("__values", __values);
    exporter("__read", __read);
    exporter("__spread", __spread);
    exporter("__spreadArrays", __spreadArrays);
    exporter("__spreadArray", __spreadArray);
    exporter("__await", __await);
    exporter("__asyncGenerator", __asyncGenerator);
    exporter("__asyncDelegator", __asyncDelegator);
    exporter("__asyncValues", __asyncValues);
    exporter("__makeTemplateObject", __makeTemplateObject);
    exporter("__importStar", __importStar);
    exporter("__importDefault", __importDefault);
    exporter("__classPrivateFieldGet", __classPrivateFieldGet);
    exporter("__classPrivateFieldSet", __classPrivateFieldSet);
});

}).call(this)}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],18:[function(require,module,exports){
"use strict";

function _typeof(obj) { "@babel/helpers - typeof"; if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.printError = printError;
exports.GraphQLError = void 0;

var _isObjectLike = _interopRequireDefault(require("../jsutils/isObjectLike.js"));

var _symbols = require("../polyfills/symbols.js");

var _location = require("../language/location.js");

var _printLocation = require("../language/printLocation.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function"); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } }); if (superClass) _setPrototypeOf(subClass, superClass); }

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = _getPrototypeOf(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = _getPrototypeOf(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return _possibleConstructorReturn(this, result); }; }

function _possibleConstructorReturn(self, call) { if (call && (_typeof(call) === "object" || typeof call === "function")) { return call; } return _assertThisInitialized(self); }

function _assertThisInitialized(self) { if (self === void 0) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return self; }

function _wrapNativeSuper(Class) { var _cache = typeof Map === "function" ? new Map() : undefined; _wrapNativeSuper = function _wrapNativeSuper(Class) { if (Class === null || !_isNativeFunction(Class)) return Class; if (typeof Class !== "function") { throw new TypeError("Super expression must either be null or a function"); } if (typeof _cache !== "undefined") { if (_cache.has(Class)) return _cache.get(Class); _cache.set(Class, Wrapper); } function Wrapper() { return _construct(Class, arguments, _getPrototypeOf(this).constructor); } Wrapper.prototype = Object.create(Class.prototype, { constructor: { value: Wrapper, enumerable: false, writable: true, configurable: true } }); return _setPrototypeOf(Wrapper, Class); }; return _wrapNativeSuper(Class); }

function _construct(Parent, args, Class) { if (_isNativeReflectConstruct()) { _construct = Reflect.construct; } else { _construct = function _construct(Parent, args, Class) { var a = [null]; a.push.apply(a, args); var Constructor = Function.bind.apply(Parent, a); var instance = new Constructor(); if (Class) _setPrototypeOf(instance, Class.prototype); return instance; }; } return _construct.apply(null, arguments); }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Date.prototype.toString.call(Reflect.construct(Date, [], function () {})); return true; } catch (e) { return false; } }

function _isNativeFunction(fn) { return Function.toString.call(fn).indexOf("[native code]") !== -1; }

function _setPrototypeOf(o, p) { _setPrototypeOf = Object.setPrototypeOf || function _setPrototypeOf(o, p) { o.__proto__ = p; return o; }; return _setPrototypeOf(o, p); }

function _getPrototypeOf(o) { _getPrototypeOf = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf(o) { return o.__proto__ || Object.getPrototypeOf(o); }; return _getPrototypeOf(o); }

/**
 * A GraphQLError describes an Error found during the parse, validate, or
 * execute phases of performing a GraphQL operation. In addition to a message
 * and stack trace, it also includes information about the locations in a
 * GraphQL document and/or execution result that correspond to the Error.
 */
var GraphQLError = /*#__PURE__*/function (_Error) {
  _inherits(GraphQLError, _Error);

  var _super = _createSuper(GraphQLError);

  /**
   * A message describing the Error for debugging purposes.
   *
   * Enumerable, and appears in the result of JSON.stringify().
   *
   * Note: should be treated as readonly, despite invariant usage.
   */

  /**
   * An array of { line, column } locations within the source GraphQL document
   * which correspond to this error.
   *
   * Errors during validation often contain multiple locations, for example to
   * point out two things with the same name. Errors during execution include a
   * single location, the field which produced the error.
   *
   * Enumerable, and appears in the result of JSON.stringify().
   */

  /**
   * An array describing the JSON-path into the execution response which
   * corresponds to this error. Only included for errors during execution.
   *
   * Enumerable, and appears in the result of JSON.stringify().
   */

  /**
   * An array of GraphQL AST Nodes corresponding to this error.
   */

  /**
   * The source GraphQL document for the first location of this error.
   *
   * Note that if this Error represents more than one node, the source may not
   * represent nodes after the first node.
   */

  /**
   * An array of character offsets within the source GraphQL document
   * which correspond to this error.
   */

  /**
   * The original error thrown from a field resolver during execution.
   */

  /**
   * Extension fields to add to the formatted error.
   */
  function GraphQLError(message, nodes, source, positions, path, originalError, extensions) {
    var _locations2, _source2, _positions2, _extensions2;

    var _this;

    _classCallCheck(this, GraphQLError);

    _this = _super.call(this, message); // Compute list of blame nodes.

    var _nodes = Array.isArray(nodes) ? nodes.length !== 0 ? nodes : undefined : nodes ? [nodes] : undefined; // Compute locations in the source for the given nodes/positions.


    var _source = source;

    if (!_source && _nodes) {
      var _nodes$0$loc;

      _source = (_nodes$0$loc = _nodes[0].loc) === null || _nodes$0$loc === void 0 ? void 0 : _nodes$0$loc.source;
    }

    var _positions = positions;

    if (!_positions && _nodes) {
      _positions = _nodes.reduce(function (list, node) {
        if (node.loc) {
          list.push(node.loc.start);
        }

        return list;
      }, []);
    }

    if (_positions && _positions.length === 0) {
      _positions = undefined;
    }

    var _locations;

    if (positions && source) {
      _locations = positions.map(function (pos) {
        return (0, _location.getLocation)(source, pos);
      });
    } else if (_nodes) {
      _locations = _nodes.reduce(function (list, node) {
        if (node.loc) {
          list.push((0, _location.getLocation)(node.loc.source, node.loc.start));
        }

        return list;
      }, []);
    }

    var _extensions = extensions;

    if (_extensions == null && originalError != null) {
      var originalExtensions = originalError.extensions;

      if ((0, _isObjectLike.default)(originalExtensions)) {
        _extensions = originalExtensions;
      }
    }

    Object.defineProperties(_assertThisInitialized(_this), {
      name: {
        value: 'GraphQLError'
      },
      message: {
        value: message,
        // By being enumerable, JSON.stringify will include `message` in the
        // resulting output. This ensures that the simplest possible GraphQL
        // service adheres to the spec.
        enumerable: true,
        writable: true
      },
      locations: {
        // Coercing falsy values to undefined ensures they will not be included
        // in JSON.stringify() when not provided.
        value: (_locations2 = _locations) !== null && _locations2 !== void 0 ? _locations2 : undefined,
        // By being enumerable, JSON.stringify will include `locations` in the
        // resulting output. This ensures that the simplest possible GraphQL
        // service adheres to the spec.
        enumerable: _locations != null
      },
      path: {
        // Coercing falsy values to undefined ensures they will not be included
        // in JSON.stringify() when not provided.
        value: path !== null && path !== void 0 ? path : undefined,
        // By being enumerable, JSON.stringify will include `path` in the
        // resulting output. This ensures that the simplest possible GraphQL
        // service adheres to the spec.
        enumerable: path != null
      },
      nodes: {
        value: _nodes !== null && _nodes !== void 0 ? _nodes : undefined
      },
      source: {
        value: (_source2 = _source) !== null && _source2 !== void 0 ? _source2 : undefined
      },
      positions: {
        value: (_positions2 = _positions) !== null && _positions2 !== void 0 ? _positions2 : undefined
      },
      originalError: {
        value: originalError
      },
      extensions: {
        // Coercing falsy values to undefined ensures they will not be included
        // in JSON.stringify() when not provided.
        value: (_extensions2 = _extensions) !== null && _extensions2 !== void 0 ? _extensions2 : undefined,
        // By being enumerable, JSON.stringify will include `path` in the
        // resulting output. This ensures that the simplest possible GraphQL
        // service adheres to the spec.
        enumerable: _extensions != null
      }
    }); // Include (non-enumerable) stack trace.

    if (originalError !== null && originalError !== void 0 && originalError.stack) {
      Object.defineProperty(_assertThisInitialized(_this), 'stack', {
        value: originalError.stack,
        writable: true,
        configurable: true
      });
      return _possibleConstructorReturn(_this);
    } // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2317')


    if (Error.captureStackTrace) {
      Error.captureStackTrace(_assertThisInitialized(_this), GraphQLError);
    } else {
      Object.defineProperty(_assertThisInitialized(_this), 'stack', {
        value: Error().stack,
        writable: true,
        configurable: true
      });
    }

    return _this;
  }

  _createClass(GraphQLError, [{
    key: "toString",
    value: function toString() {
      return printError(this);
    } // FIXME: workaround to not break chai comparisons, should be remove in v16
    // $FlowFixMe[unsupported-syntax] Flow doesn't support computed properties yet

  }, {
    key: _symbols.SYMBOL_TO_STRING_TAG,
    get: function get() {
      return 'Object';
    }
  }]);

  return GraphQLError;
}( /*#__PURE__*/_wrapNativeSuper(Error));
/**
 * Prints a GraphQLError to a string, representing useful location information
 * about the error's position in the source.
 */


exports.GraphQLError = GraphQLError;

function printError(error) {
  var output = error.message;

  if (error.nodes) {
    for (var _i2 = 0, _error$nodes2 = error.nodes; _i2 < _error$nodes2.length; _i2++) {
      var node = _error$nodes2[_i2];

      if (node.loc) {
        output += '\n\n' + (0, _printLocation.printLocation)(node.loc);
      }
    }
  } else if (error.source && error.locations) {
    for (var _i4 = 0, _error$locations2 = error.locations; _i4 < _error$locations2.length; _i4++) {
      var location = _error$locations2[_i4];
      output += '\n\n' + (0, _printLocation.printSourceLocation)(error.source, location);
    }
  }

  return output;
}

},{"../jsutils/isObjectLike.js":37,"../language/location.js":57,"../language/printLocation.js":60,"../polyfills/symbols.js":71}],19:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.formatError = formatError;

var _devAssert = _interopRequireDefault(require("../jsutils/devAssert.js"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Given a GraphQLError, format it according to the rules described by the
 * Response Format, Errors section of the GraphQL Specification.
 */
function formatError(error) {
  var _error$message;

  error || (0, _devAssert.default)(0, 'Received null or undefined error.');
  var message = (_error$message = error.message) !== null && _error$message !== void 0 ? _error$message : 'An unknown error occurred.';
  var locations = error.locations;
  var path = error.path;
  var extensions = error.extensions;
  return extensions ? {
    message: message,
    locations: locations,
    path: path,
    extensions: extensions
  } : {
    message: message,
    locations: locations,
    path: path
  };
}
/**
 * @see https://github.com/graphql/graphql-spec/blob/master/spec/Section%207%20--%20Response.md#errors
 */

},{"../jsutils/devAssert.js":30}],20:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "GraphQLError", {
  enumerable: true,
  get: function get() {
    return _GraphQLError.GraphQLError;
  }
});
Object.defineProperty(exports, "printError", {
  enumerable: true,
  get: function get() {
    return _GraphQLError.printError;
  }
});
Object.defineProperty(exports, "syntaxError", {
  enumerable: true,
  get: function get() {
    return _syntaxError.syntaxError;
  }
});
Object.defineProperty(exports, "locatedError", {
  enumerable: true,
  get: function get() {
    return _locatedError.locatedError;
  }
});
Object.defineProperty(exports, "formatError", {
  enumerable: true,
  get: function get() {
    return _formatError.formatError;
  }
});

var _GraphQLError = require("./GraphQLError.js");

var _syntaxError = require("./syntaxError.js");

var _locatedError = require("./locatedError.js");

var _formatError = require("./formatError.js");

},{"./GraphQLError.js":18,"./formatError.js":19,"./locatedError.js":21,"./syntaxError.js":22}],21:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.locatedError = locatedError;

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _GraphQLError = require("./GraphQLError.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Given an arbitrary value, presumably thrown while attempting to execute a
 * GraphQL operation, produce a new GraphQLError aware of the location in the
 * document responsible for the original Error.
 */
function locatedError(rawOriginalError, nodes, path) {
  var _nodes;

  // Sometimes a non-error is thrown, wrap it as an Error instance to ensure a consistent Error interface.
  var originalError = rawOriginalError instanceof Error ? rawOriginalError : new Error('Unexpected error value: ' + (0, _inspect.default)(rawOriginalError)); // Note: this uses a brand-check to support GraphQL errors originating from other contexts.

  if (Array.isArray(originalError.path)) {
    return originalError;
  }

  return new _GraphQLError.GraphQLError(originalError.message, (_nodes = originalError.nodes) !== null && _nodes !== void 0 ? _nodes : nodes, originalError.source, originalError.positions, path, originalError);
}

},{"../jsutils/inspect.js":33,"./GraphQLError.js":18}],22:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.syntaxError = syntaxError;

var _GraphQLError = require("./GraphQLError.js");

/**
 * Produces a GraphQLError representing a syntax error, containing useful
 * descriptive information about the syntax error's position in the source.
 */
function syntaxError(source, position, description) {
  return new _GraphQLError.GraphQLError("Syntax Error: ".concat(description), undefined, source, [position]);
}

},{"./GraphQLError.js":18}],23:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.execute = execute;
exports.executeSync = executeSync;
exports.assertValidExecutionArguments = assertValidExecutionArguments;
exports.buildExecutionContext = buildExecutionContext;
exports.collectFields = collectFields;
exports.buildResolveInfo = buildResolveInfo;
exports.getFieldDef = getFieldDef;
exports.defaultFieldResolver = exports.defaultTypeResolver = void 0;

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _memoize = _interopRequireDefault(require("../jsutils/memoize3.js"));

var _invariant = _interopRequireDefault(require("../jsutils/invariant.js"));

var _devAssert = _interopRequireDefault(require("../jsutils/devAssert.js"));

var _isPromise = _interopRequireDefault(require("../jsutils/isPromise.js"));

var _isObjectLike = _interopRequireDefault(require("../jsutils/isObjectLike.js"));

var _safeArrayFrom = _interopRequireDefault(require("../jsutils/safeArrayFrom.js"));

var _promiseReduce = _interopRequireDefault(require("../jsutils/promiseReduce.js"));

var _promiseForObject = _interopRequireDefault(require("../jsutils/promiseForObject.js"));

var _Path = require("../jsutils/Path.js");

var _GraphQLError = require("../error/GraphQLError.js");

var _locatedError = require("../error/locatedError.js");

var _kinds = require("../language/kinds.js");

var _validate = require("../type/validate.js");

var _introspection = require("../type/introspection.js");

var _directives = require("../type/directives.js");

var _definition = require("../type/definition.js");

var _typeFromAST = require("../utilities/typeFromAST.js");

var _getOperationRootType = require("../utilities/getOperationRootType.js");

var _values = require("./values.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function execute(argsOrSchema, document, rootValue, contextValue, variableValues, operationName, fieldResolver, typeResolver) {
  /* eslint-enable no-redeclare */
  // Extract arguments from object args if provided.
  return arguments.length === 1 ? executeImpl(argsOrSchema) : executeImpl({
    schema: argsOrSchema,
    document: document,
    rootValue: rootValue,
    contextValue: contextValue,
    variableValues: variableValues,
    operationName: operationName,
    fieldResolver: fieldResolver,
    typeResolver: typeResolver
  });
}
/**
 * Also implements the "Evaluating requests" section of the GraphQL specification.
 * However, it guarantees to complete synchronously (or throw an error) assuming
 * that all field resolvers are also synchronous.
 */


function executeSync(args) {
  var result = executeImpl(args); // Assert that the execution was synchronous.

  if ((0, _isPromise.default)(result)) {
    throw new Error('GraphQL execution failed to complete synchronously.');
  }

  return result;
}

function executeImpl(args) {
  var schema = args.schema,
      document = args.document,
      rootValue = args.rootValue,
      contextValue = args.contextValue,
      variableValues = args.variableValues,
      operationName = args.operationName,
      fieldResolver = args.fieldResolver,
      typeResolver = args.typeResolver; // If arguments are missing or incorrect, throw an error.

  assertValidExecutionArguments(schema, document, variableValues); // If a valid execution context cannot be created due to incorrect arguments,
  // a "Response" with only errors is returned.

  var exeContext = buildExecutionContext(schema, document, rootValue, contextValue, variableValues, operationName, fieldResolver, typeResolver); // Return early errors if execution context failed.

  if (Array.isArray(exeContext)) {
    return {
      errors: exeContext
    };
  } // Return a Promise that will eventually resolve to the data described by
  // The "Response" section of the GraphQL specification.
  //
  // If errors are encountered while executing a GraphQL field, only that
  // field and its descendants will be omitted, and sibling fields will still
  // be executed. An execution which encounters errors will still result in a
  // resolved Promise.


  var data = executeOperation(exeContext, exeContext.operation, rootValue);
  return buildResponse(exeContext, data);
}
/**
 * Given a completed execution context and data, build the { errors, data }
 * response defined by the "Response" section of the GraphQL specification.
 */


function buildResponse(exeContext, data) {
  if ((0, _isPromise.default)(data)) {
    return data.then(function (resolved) {
      return buildResponse(exeContext, resolved);
    });
  }

  return exeContext.errors.length === 0 ? {
    data: data
  } : {
    errors: exeContext.errors,
    data: data
  };
}
/**
 * Essential assertions before executing to provide developer feedback for
 * improper use of the GraphQL library.
 *
 * @internal
 */


function assertValidExecutionArguments(schema, document, rawVariableValues) {
  document || (0, _devAssert.default)(0, 'Must provide document.'); // If the schema used for execution is invalid, throw an error.

  (0, _validate.assertValidSchema)(schema); // Variables, if provided, must be an object.

  rawVariableValues == null || (0, _isObjectLike.default)(rawVariableValues) || (0, _devAssert.default)(0, 'Variables must be provided as an Object where each property is a variable value. Perhaps look to see if an unparsed JSON string was provided.');
}
/**
 * Constructs a ExecutionContext object from the arguments passed to
 * execute, which we will pass throughout the other execution methods.
 *
 * Throws a GraphQLError if a valid execution context cannot be created.
 *
 * @internal
 */


function buildExecutionContext(schema, document, rootValue, contextValue, rawVariableValues, operationName, fieldResolver, typeResolver) {
  var _definition$name, _operation$variableDe;

  var operation;
  var fragments = Object.create(null);

  for (var _i2 = 0, _document$definitions2 = document.definitions; _i2 < _document$definitions2.length; _i2++) {
    var definition = _document$definitions2[_i2];

    switch (definition.kind) {
      case _kinds.Kind.OPERATION_DEFINITION:
        if (operationName == null) {
          if (operation !== undefined) {
            return [new _GraphQLError.GraphQLError('Must provide operation name if query contains multiple operations.')];
          }

          operation = definition;
        } else if (((_definition$name = definition.name) === null || _definition$name === void 0 ? void 0 : _definition$name.value) === operationName) {
          operation = definition;
        }

        break;

      case _kinds.Kind.FRAGMENT_DEFINITION:
        fragments[definition.name.value] = definition;
        break;
    }
  }

  if (!operation) {
    if (operationName != null) {
      return [new _GraphQLError.GraphQLError("Unknown operation named \"".concat(operationName, "\"."))];
    }

    return [new _GraphQLError.GraphQLError('Must provide an operation.')];
  } // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')


  var variableDefinitions = (_operation$variableDe = operation.variableDefinitions) !== null && _operation$variableDe !== void 0 ? _operation$variableDe : [];
  var coercedVariableValues = (0, _values.getVariableValues)(schema, variableDefinitions, rawVariableValues !== null && rawVariableValues !== void 0 ? rawVariableValues : {}, {
    maxErrors: 50
  });

  if (coercedVariableValues.errors) {
    return coercedVariableValues.errors;
  }

  return {
    schema: schema,
    fragments: fragments,
    rootValue: rootValue,
    contextValue: contextValue,
    operation: operation,
    variableValues: coercedVariableValues.coerced,
    fieldResolver: fieldResolver !== null && fieldResolver !== void 0 ? fieldResolver : defaultFieldResolver,
    typeResolver: typeResolver !== null && typeResolver !== void 0 ? typeResolver : defaultTypeResolver,
    errors: []
  };
}
/**
 * Implements the "Evaluating operations" section of the spec.
 */


function executeOperation(exeContext, operation, rootValue) {
  var type = (0, _getOperationRootType.getOperationRootType)(exeContext.schema, operation);
  var fields = collectFields(exeContext, type, operation.selectionSet, Object.create(null), Object.create(null));
  var path = undefined; // Errors from sub-fields of a NonNull type may propagate to the top level,
  // at which point we still log the error and null the parent field, which
  // in this case is the entire response.

  try {
    var result = operation.operation === 'mutation' ? executeFieldsSerially(exeContext, type, rootValue, path, fields) : executeFields(exeContext, type, rootValue, path, fields);

    if ((0, _isPromise.default)(result)) {
      return result.then(undefined, function (error) {
        exeContext.errors.push(error);
        return Promise.resolve(null);
      });
    }

    return result;
  } catch (error) {
    exeContext.errors.push(error);
    return null;
  }
}
/**
 * Implements the "Evaluating selection sets" section of the spec
 * for "write" mode.
 */


function executeFieldsSerially(exeContext, parentType, sourceValue, path, fields) {
  return (0, _promiseReduce.default)(Object.keys(fields), function (results, responseName) {
    var fieldNodes = fields[responseName];
    var fieldPath = (0, _Path.addPath)(path, responseName, parentType.name);
    var result = resolveField(exeContext, parentType, sourceValue, fieldNodes, fieldPath);

    if (result === undefined) {
      return results;
    }

    if ((0, _isPromise.default)(result)) {
      return result.then(function (resolvedResult) {
        results[responseName] = resolvedResult;
        return results;
      });
    }

    results[responseName] = result;
    return results;
  }, Object.create(null));
}
/**
 * Implements the "Evaluating selection sets" section of the spec
 * for "read" mode.
 */


function executeFields(exeContext, parentType, sourceValue, path, fields) {
  var results = Object.create(null);
  var containsPromise = false;

  for (var _i4 = 0, _Object$keys2 = Object.keys(fields); _i4 < _Object$keys2.length; _i4++) {
    var responseName = _Object$keys2[_i4];
    var fieldNodes = fields[responseName];
    var fieldPath = (0, _Path.addPath)(path, responseName, parentType.name);
    var result = resolveField(exeContext, parentType, sourceValue, fieldNodes, fieldPath);

    if (result !== undefined) {
      results[responseName] = result;

      if ((0, _isPromise.default)(result)) {
        containsPromise = true;
      }
    }
  } // If there are no promises, we can just return the object


  if (!containsPromise) {
    return results;
  } // Otherwise, results is a map from field name to the result of resolving that
  // field, which is possibly a promise. Return a promise that will return this
  // same map, but with any promises replaced with the values they resolved to.


  return (0, _promiseForObject.default)(results);
}
/**
 * Given a selectionSet, adds all of the fields in that selection to
 * the passed in map of fields, and returns it at the end.
 *
 * CollectFields requires the "runtime type" of an object. For a field which
 * returns an Interface or Union type, the "runtime type" will be the actual
 * Object type returned by that field.
 *
 * @internal
 */


function collectFields(exeContext, runtimeType, selectionSet, fields, visitedFragmentNames) {
  for (var _i6 = 0, _selectionSet$selecti2 = selectionSet.selections; _i6 < _selectionSet$selecti2.length; _i6++) {
    var selection = _selectionSet$selecti2[_i6];

    switch (selection.kind) {
      case _kinds.Kind.FIELD:
        {
          if (!shouldIncludeNode(exeContext, selection)) {
            continue;
          }

          var name = getFieldEntryKey(selection);

          if (!fields[name]) {
            fields[name] = [];
          }

          fields[name].push(selection);
          break;
        }

      case _kinds.Kind.INLINE_FRAGMENT:
        {
          if (!shouldIncludeNode(exeContext, selection) || !doesFragmentConditionMatch(exeContext, selection, runtimeType)) {
            continue;
          }

          collectFields(exeContext, runtimeType, selection.selectionSet, fields, visitedFragmentNames);
          break;
        }

      case _kinds.Kind.FRAGMENT_SPREAD:
        {
          var fragName = selection.name.value;

          if (visitedFragmentNames[fragName] || !shouldIncludeNode(exeContext, selection)) {
            continue;
          }

          visitedFragmentNames[fragName] = true;
          var fragment = exeContext.fragments[fragName];

          if (!fragment || !doesFragmentConditionMatch(exeContext, fragment, runtimeType)) {
            continue;
          }

          collectFields(exeContext, runtimeType, fragment.selectionSet, fields, visitedFragmentNames);
          break;
        }
    }
  }

  return fields;
}
/**
 * Determines if a field should be included based on the @include and @skip
 * directives, where @skip has higher precedence than @include.
 */


function shouldIncludeNode(exeContext, node) {
  var skip = (0, _values.getDirectiveValues)(_directives.GraphQLSkipDirective, node, exeContext.variableValues);

  if ((skip === null || skip === void 0 ? void 0 : skip.if) === true) {
    return false;
  }

  var include = (0, _values.getDirectiveValues)(_directives.GraphQLIncludeDirective, node, exeContext.variableValues);

  if ((include === null || include === void 0 ? void 0 : include.if) === false) {
    return false;
  }

  return true;
}
/**
 * Determines if a fragment is applicable to the given type.
 */


function doesFragmentConditionMatch(exeContext, fragment, type) {
  var typeConditionNode = fragment.typeCondition;

  if (!typeConditionNode) {
    return true;
  }

  var conditionalType = (0, _typeFromAST.typeFromAST)(exeContext.schema, typeConditionNode);

  if (conditionalType === type) {
    return true;
  }

  if ((0, _definition.isAbstractType)(conditionalType)) {
    return exeContext.schema.isSubType(conditionalType, type);
  }

  return false;
}
/**
 * Implements the logic to compute the key of a given field's entry
 */


function getFieldEntryKey(node) {
  return node.alias ? node.alias.value : node.name.value;
}
/**
 * Resolves the field on the given source object. In particular, this
 * figures out the value that the field returns by calling its resolve function,
 * then calls completeValue to complete promises, serialize scalars, or execute
 * the sub-selection-set for objects.
 */


function resolveField(exeContext, parentType, source, fieldNodes, path) {
  var _fieldDef$resolve;

  var fieldNode = fieldNodes[0];
  var fieldName = fieldNode.name.value;
  var fieldDef = getFieldDef(exeContext.schema, parentType, fieldName);

  if (!fieldDef) {
    return;
  }

  var returnType = fieldDef.type;
  var resolveFn = (_fieldDef$resolve = fieldDef.resolve) !== null && _fieldDef$resolve !== void 0 ? _fieldDef$resolve : exeContext.fieldResolver;
  var info = buildResolveInfo(exeContext, fieldDef, fieldNodes, parentType, path); // Get the resolve function, regardless of if its result is normal or abrupt (error).

  try {
    // Build a JS object of arguments from the field.arguments AST, using the
    // variables scope to fulfill any variable references.
    // TODO: find a way to memoize, in case this field is within a List type.
    var args = (0, _values.getArgumentValues)(fieldDef, fieldNodes[0], exeContext.variableValues); // The resolve function's optional third argument is a context value that
    // is provided to every resolve function within an execution. It is commonly
    // used to represent an authenticated user, or request-specific caches.

    var _contextValue = exeContext.contextValue;
    var result = resolveFn(source, args, _contextValue, info);
    var completed;

    if ((0, _isPromise.default)(result)) {
      completed = result.then(function (resolved) {
        return completeValue(exeContext, returnType, fieldNodes, info, path, resolved);
      });
    } else {
      completed = completeValue(exeContext, returnType, fieldNodes, info, path, result);
    }

    if ((0, _isPromise.default)(completed)) {
      // Note: we don't rely on a `catch` method, but we do expect "thenable"
      // to take a second callback for the error case.
      return completed.then(undefined, function (rawError) {
        var error = (0, _locatedError.locatedError)(rawError, fieldNodes, (0, _Path.pathToArray)(path));
        return handleFieldError(error, returnType, exeContext);
      });
    }

    return completed;
  } catch (rawError) {
    var error = (0, _locatedError.locatedError)(rawError, fieldNodes, (0, _Path.pathToArray)(path));
    return handleFieldError(error, returnType, exeContext);
  }
}
/**
 * @internal
 */


function buildResolveInfo(exeContext, fieldDef, fieldNodes, parentType, path) {
  // The resolve function's optional fourth argument is a collection of
  // information about the current execution state.
  return {
    fieldName: fieldDef.name,
    fieldNodes: fieldNodes,
    returnType: fieldDef.type,
    parentType: parentType,
    path: path,
    schema: exeContext.schema,
    fragments: exeContext.fragments,
    rootValue: exeContext.rootValue,
    operation: exeContext.operation,
    variableValues: exeContext.variableValues
  };
}

function handleFieldError(error, returnType, exeContext) {
  // If the field type is non-nullable, then it is resolved without any
  // protection from errors, however it still properly locates the error.
  if ((0, _definition.isNonNullType)(returnType)) {
    throw error;
  } // Otherwise, error protection is applied, logging the error and resolving
  // a null value for this field if one is encountered.


  exeContext.errors.push(error);
  return null;
}
/**
 * Implements the instructions for completeValue as defined in the
 * "Field entries" section of the spec.
 *
 * If the field type is Non-Null, then this recursively completes the value
 * for the inner type. It throws a field error if that completion returns null,
 * as per the "Nullability" section of the spec.
 *
 * If the field type is a List, then this recursively completes the value
 * for the inner type on each item in the list.
 *
 * If the field type is a Scalar or Enum, ensures the completed value is a legal
 * value of the type by calling the `serialize` method of GraphQL type
 * definition.
 *
 * If the field is an abstract type, determine the runtime type of the value
 * and then complete based on that type
 *
 * Otherwise, the field type expects a sub-selection set, and will complete the
 * value by evaluating all sub-selections.
 */


function completeValue(exeContext, returnType, fieldNodes, info, path, result) {
  // If result is an Error, throw a located error.
  if (result instanceof Error) {
    throw result;
  } // If field type is NonNull, complete for inner type, and throw field error
  // if result is null.


  if ((0, _definition.isNonNullType)(returnType)) {
    var completed = completeValue(exeContext, returnType.ofType, fieldNodes, info, path, result);

    if (completed === null) {
      throw new Error("Cannot return null for non-nullable field ".concat(info.parentType.name, ".").concat(info.fieldName, "."));
    }

    return completed;
  } // If result value is null or undefined then return null.


  if (result == null) {
    return null;
  } // If field type is List, complete each item in the list with the inner type


  if ((0, _definition.isListType)(returnType)) {
    return completeListValue(exeContext, returnType, fieldNodes, info, path, result);
  } // If field type is a leaf type, Scalar or Enum, serialize to a valid value,
  // returning null if serialization is not possible.


  if ((0, _definition.isLeafType)(returnType)) {
    return completeLeafValue(returnType, result);
  } // If field type is an abstract type, Interface or Union, determine the
  // runtime Object type and complete for that type.


  if ((0, _definition.isAbstractType)(returnType)) {
    return completeAbstractValue(exeContext, returnType, fieldNodes, info, path, result);
  } // If field type is Object, execute and complete all sub-selections.
  // istanbul ignore else (See: 'https://github.com/graphql/graphql-js/issues/2618')


  if ((0, _definition.isObjectType)(returnType)) {
    return completeObjectValue(exeContext, returnType, fieldNodes, info, path, result);
  } // istanbul ignore next (Not reachable. All possible output types have been considered)


  false || (0, _invariant.default)(0, 'Cannot complete value of unexpected output type: ' + (0, _inspect.default)(returnType));
}
/**
 * Complete a list value by completing each item in the list with the
 * inner type
 */


function completeListValue(exeContext, returnType, fieldNodes, info, path, result) {
  // This is specified as a simple map, however we're optimizing the path
  // where the list contains no Promises by avoiding creating another Promise.
  var itemType = returnType.ofType;
  var containsPromise = false;
  var completedResults = (0, _safeArrayFrom.default)(result, function (item, index) {
    // No need to modify the info object containing the path,
    // since from here on it is not ever accessed by resolver functions.
    var itemPath = (0, _Path.addPath)(path, index, undefined);

    try {
      var completedItem;

      if ((0, _isPromise.default)(item)) {
        completedItem = item.then(function (resolved) {
          return completeValue(exeContext, itemType, fieldNodes, info, itemPath, resolved);
        });
      } else {
        completedItem = completeValue(exeContext, itemType, fieldNodes, info, itemPath, item);
      }

      if ((0, _isPromise.default)(completedItem)) {
        containsPromise = true; // Note: we don't rely on a `catch` method, but we do expect "thenable"
        // to take a second callback for the error case.

        return completedItem.then(undefined, function (rawError) {
          var error = (0, _locatedError.locatedError)(rawError, fieldNodes, (0, _Path.pathToArray)(itemPath));
          return handleFieldError(error, itemType, exeContext);
        });
      }

      return completedItem;
    } catch (rawError) {
      var error = (0, _locatedError.locatedError)(rawError, fieldNodes, (0, _Path.pathToArray)(itemPath));
      return handleFieldError(error, itemType, exeContext);
    }
  });

  if (completedResults == null) {
    throw new _GraphQLError.GraphQLError("Expected Iterable, but did not find one for field \"".concat(info.parentType.name, ".").concat(info.fieldName, "\"."));
  }

  return containsPromise ? Promise.all(completedResults) : completedResults;
}
/**
 * Complete a Scalar or Enum by serializing to a valid value, returning
 * null if serialization is not possible.
 */


function completeLeafValue(returnType, result) {
  var serializedResult = returnType.serialize(result);

  if (serializedResult === undefined) {
    throw new Error("Expected a value of type \"".concat((0, _inspect.default)(returnType), "\" but ") + "received: ".concat((0, _inspect.default)(result)));
  }

  return serializedResult;
}
/**
 * Complete a value of an abstract type by determining the runtime object type
 * of that value, then complete the value for that type.
 */


function completeAbstractValue(exeContext, returnType, fieldNodes, info, path, result) {
  var _returnType$resolveTy;

  var resolveTypeFn = (_returnType$resolveTy = returnType.resolveType) !== null && _returnType$resolveTy !== void 0 ? _returnType$resolveTy : exeContext.typeResolver;
  var contextValue = exeContext.contextValue;
  var runtimeType = resolveTypeFn(result, contextValue, info, returnType);

  if ((0, _isPromise.default)(runtimeType)) {
    return runtimeType.then(function (resolvedRuntimeType) {
      return completeObjectValue(exeContext, ensureValidRuntimeType(resolvedRuntimeType, exeContext, returnType, fieldNodes, info, result), fieldNodes, info, path, result);
    });
  }

  return completeObjectValue(exeContext, ensureValidRuntimeType(runtimeType, exeContext, returnType, fieldNodes, info, result), fieldNodes, info, path, result);
}

function ensureValidRuntimeType(runtimeTypeOrName, exeContext, returnType, fieldNodes, info, result) {
  if (runtimeTypeOrName == null) {
    throw new _GraphQLError.GraphQLError("Abstract type \"".concat(returnType.name, "\" must resolve to an Object type at runtime for field \"").concat(info.parentType.name, ".").concat(info.fieldName, "\". Either the \"").concat(returnType.name, "\" type should provide a \"resolveType\" function or each possible type should provide an \"isTypeOf\" function."), fieldNodes);
  } // FIXME: temporary workaround until support for passing object types would be removed in v16.0.0


  var runtimeTypeName = (0, _definition.isNamedType)(runtimeTypeOrName) ? runtimeTypeOrName.name : runtimeTypeOrName;

  if (typeof runtimeTypeName !== 'string') {
    throw new _GraphQLError.GraphQLError("Abstract type \"".concat(returnType.name, "\" must resolve to an Object type at runtime for field \"").concat(info.parentType.name, ".").concat(info.fieldName, "\" with ") + "value ".concat((0, _inspect.default)(result), ", received \"").concat((0, _inspect.default)(runtimeTypeOrName), "\"."));
  }

  var runtimeType = exeContext.schema.getType(runtimeTypeName);

  if (runtimeType == null) {
    throw new _GraphQLError.GraphQLError("Abstract type \"".concat(returnType.name, "\" was resolve to a type \"").concat(runtimeTypeName, "\" that does not exist inside schema."), fieldNodes);
  }

  if (!(0, _definition.isObjectType)(runtimeType)) {
    throw new _GraphQLError.GraphQLError("Abstract type \"".concat(returnType.name, "\" was resolve to a non-object type \"").concat(runtimeTypeName, "\"."), fieldNodes);
  }

  if (!exeContext.schema.isSubType(returnType, runtimeType)) {
    throw new _GraphQLError.GraphQLError("Runtime Object type \"".concat(runtimeType.name, "\" is not a possible type for \"").concat(returnType.name, "\"."), fieldNodes);
  }

  return runtimeType;
}
/**
 * Complete an Object value by executing all sub-selections.
 */


function completeObjectValue(exeContext, returnType, fieldNodes, info, path, result) {
  // If there is an isTypeOf predicate function, call it with the
  // current result. If isTypeOf returns false, then raise an error rather
  // than continuing execution.
  if (returnType.isTypeOf) {
    var isTypeOf = returnType.isTypeOf(result, exeContext.contextValue, info);

    if ((0, _isPromise.default)(isTypeOf)) {
      return isTypeOf.then(function (resolvedIsTypeOf) {
        if (!resolvedIsTypeOf) {
          throw invalidReturnTypeError(returnType, result, fieldNodes);
        }

        return collectAndExecuteSubfields(exeContext, returnType, fieldNodes, path, result);
      });
    }

    if (!isTypeOf) {
      throw invalidReturnTypeError(returnType, result, fieldNodes);
    }
  }

  return collectAndExecuteSubfields(exeContext, returnType, fieldNodes, path, result);
}

function invalidReturnTypeError(returnType, result, fieldNodes) {
  return new _GraphQLError.GraphQLError("Expected value of type \"".concat(returnType.name, "\" but got: ").concat((0, _inspect.default)(result), "."), fieldNodes);
}

function collectAndExecuteSubfields(exeContext, returnType, fieldNodes, path, result) {
  // Collect sub-fields to execute to complete this value.
  var subFieldNodes = collectSubfields(exeContext, returnType, fieldNodes);
  return executeFields(exeContext, returnType, result, path, subFieldNodes);
}
/**
 * A memoized collection of relevant subfields with regard to the return
 * type. Memoizing ensures the subfields are not repeatedly calculated, which
 * saves overhead when resolving lists of values.
 */


var collectSubfields = (0, _memoize.default)(_collectSubfields);

function _collectSubfields(exeContext, returnType, fieldNodes) {
  var subFieldNodes = Object.create(null);
  var visitedFragmentNames = Object.create(null);

  for (var _i8 = 0; _i8 < fieldNodes.length; _i8++) {
    var node = fieldNodes[_i8];

    if (node.selectionSet) {
      subFieldNodes = collectFields(exeContext, returnType, node.selectionSet, subFieldNodes, visitedFragmentNames);
    }
  }

  return subFieldNodes;
}
/**
 * If a resolveType function is not given, then a default resolve behavior is
 * used which attempts two strategies:
 *
 * First, See if the provided value has a `__typename` field defined, if so, use
 * that value as name of the resolved type.
 *
 * Otherwise, test each possible type for the abstract type by calling
 * isTypeOf for the object being coerced, returning the first type that matches.
 */


var defaultTypeResolver = function defaultTypeResolver(value, contextValue, info, abstractType) {
  // First, look for `__typename`.
  if ((0, _isObjectLike.default)(value) && typeof value.__typename === 'string') {
    return value.__typename;
  } // Otherwise, test each possible type.


  var possibleTypes = info.schema.getPossibleTypes(abstractType);
  var promisedIsTypeOfResults = [];

  for (var i = 0; i < possibleTypes.length; i++) {
    var type = possibleTypes[i];

    if (type.isTypeOf) {
      var isTypeOfResult = type.isTypeOf(value, contextValue, info);

      if ((0, _isPromise.default)(isTypeOfResult)) {
        promisedIsTypeOfResults[i] = isTypeOfResult;
      } else if (isTypeOfResult) {
        return type.name;
      }
    }
  }

  if (promisedIsTypeOfResults.length) {
    return Promise.all(promisedIsTypeOfResults).then(function (isTypeOfResults) {
      for (var _i9 = 0; _i9 < isTypeOfResults.length; _i9++) {
        if (isTypeOfResults[_i9]) {
          return possibleTypes[_i9].name;
        }
      }
    });
  }
};
/**
 * If a resolve function is not given, then a default resolve behavior is used
 * which takes the property of the source object of the same name as the field
 * and returns it as the result, or if it's a function, returns the result
 * of calling that function while passing along args and context value.
 */


exports.defaultTypeResolver = defaultTypeResolver;

var defaultFieldResolver = function defaultFieldResolver(source, args, contextValue, info) {
  // ensure source is a value for which property access is acceptable.
  if ((0, _isObjectLike.default)(source) || typeof source === 'function') {
    var property = source[info.fieldName];

    if (typeof property === 'function') {
      return source[info.fieldName](args, contextValue, info);
    }

    return property;
  }
};
/**
 * This method looks up the field on the given type definition.
 * It has special casing for the three introspection fields,
 * __schema, __type and __typename. __typename is special because
 * it can always be queried as a field, even in situations where no
 * other fields are allowed, like on a Union. __schema and __type
 * could get automatically added to the query type, but that would
 * require mutating type definitions, which would cause issues.
 *
 * @internal
 */


exports.defaultFieldResolver = defaultFieldResolver;

function getFieldDef(schema, parentType, fieldName) {
  if (fieldName === _introspection.SchemaMetaFieldDef.name && schema.getQueryType() === parentType) {
    return _introspection.SchemaMetaFieldDef;
  } else if (fieldName === _introspection.TypeMetaFieldDef.name && schema.getQueryType() === parentType) {
    return _introspection.TypeMetaFieldDef;
  } else if (fieldName === _introspection.TypeNameMetaFieldDef.name) {
    return _introspection.TypeNameMetaFieldDef;
  }

  return parentType.getFields()[fieldName];
}

},{"../error/GraphQLError.js":18,"../error/locatedError.js":21,"../jsutils/Path.js":28,"../jsutils/devAssert.js":30,"../jsutils/inspect.js":33,"../jsutils/invariant.js":35,"../jsutils/isObjectLike.js":37,"../jsutils/isPromise.js":38,"../jsutils/memoize3.js":42,"../jsutils/promiseForObject.js":46,"../jsutils/promiseReduce.js":47,"../jsutils/safeArrayFrom.js":48,"../language/kinds.js":55,"../type/definition.js":75,"../type/directives.js":76,"../type/introspection.js":78,"../type/validate.js":81,"../utilities/getOperationRootType.js":94,"../utilities/typeFromAST.js":102,"./values.js":25}],24:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "responsePathAsArray", {
  enumerable: true,
  get: function get() {
    return _Path.pathToArray;
  }
});
Object.defineProperty(exports, "execute", {
  enumerable: true,
  get: function get() {
    return _execute.execute;
  }
});
Object.defineProperty(exports, "executeSync", {
  enumerable: true,
  get: function get() {
    return _execute.executeSync;
  }
});
Object.defineProperty(exports, "defaultFieldResolver", {
  enumerable: true,
  get: function get() {
    return _execute.defaultFieldResolver;
  }
});
Object.defineProperty(exports, "defaultTypeResolver", {
  enumerable: true,
  get: function get() {
    return _execute.defaultTypeResolver;
  }
});
Object.defineProperty(exports, "getDirectiveValues", {
  enumerable: true,
  get: function get() {
    return _values.getDirectiveValues;
  }
});

var _Path = require("../jsutils/Path.js");

var _execute = require("./execute.js");

var _values = require("./values.js");

},{"../jsutils/Path.js":28,"./execute.js":23,"./values.js":25}],25:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.getVariableValues = getVariableValues;
exports.getArgumentValues = getArgumentValues;
exports.getDirectiveValues = getDirectiveValues;

var _find = _interopRequireDefault(require("../polyfills/find.js"));

var _keyMap = _interopRequireDefault(require("../jsutils/keyMap.js"));

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _printPathArray = _interopRequireDefault(require("../jsutils/printPathArray.js"));

var _GraphQLError = require("../error/GraphQLError.js");

var _kinds = require("../language/kinds.js");

var _printer = require("../language/printer.js");

var _definition = require("../type/definition.js");

var _typeFromAST = require("../utilities/typeFromAST.js");

var _valueFromAST = require("../utilities/valueFromAST.js");

var _coerceInputValue = require("../utilities/coerceInputValue.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Prepares an object map of variableValues of the correct type based on the
 * provided variable definitions and arbitrary input. If the input cannot be
 * parsed to match the variable definitions, a GraphQLError will be thrown.
 *
 * Note: The returned value is a plain Object with a prototype, since it is
 * exposed to user code. Care should be taken to not pull values from the
 * Object prototype.
 *
 * @internal
 */
function getVariableValues(schema, varDefNodes, inputs, options) {
  var errors = [];
  var maxErrors = options === null || options === void 0 ? void 0 : options.maxErrors;

  try {
    var coerced = coerceVariableValues(schema, varDefNodes, inputs, function (error) {
      if (maxErrors != null && errors.length >= maxErrors) {
        throw new _GraphQLError.GraphQLError('Too many errors processing variables, error limit reached. Execution aborted.');
      }

      errors.push(error);
    });

    if (errors.length === 0) {
      return {
        coerced: coerced
      };
    }
  } catch (error) {
    errors.push(error);
  }

  return {
    errors: errors
  };
}

function coerceVariableValues(schema, varDefNodes, inputs, onError) {
  var coercedValues = {};

  var _loop = function _loop(_i2) {
    var varDefNode = varDefNodes[_i2];
    var varName = varDefNode.variable.name.value;
    var varType = (0, _typeFromAST.typeFromAST)(schema, varDefNode.type);

    if (!(0, _definition.isInputType)(varType)) {
      // Must use input types for variables. This should be caught during
      // validation, however is checked again here for safety.
      var varTypeStr = (0, _printer.print)(varDefNode.type);
      onError(new _GraphQLError.GraphQLError("Variable \"$".concat(varName, "\" expected value of type \"").concat(varTypeStr, "\" which cannot be used as an input type."), varDefNode.type));
      return "continue";
    }

    if (!hasOwnProperty(inputs, varName)) {
      if (varDefNode.defaultValue) {
        coercedValues[varName] = (0, _valueFromAST.valueFromAST)(varDefNode.defaultValue, varType);
      } else if ((0, _definition.isNonNullType)(varType)) {
        var _varTypeStr = (0, _inspect.default)(varType);

        onError(new _GraphQLError.GraphQLError("Variable \"$".concat(varName, "\" of required type \"").concat(_varTypeStr, "\" was not provided."), varDefNode));
      }

      return "continue";
    }

    var value = inputs[varName];

    if (value === null && (0, _definition.isNonNullType)(varType)) {
      var _varTypeStr2 = (0, _inspect.default)(varType);

      onError(new _GraphQLError.GraphQLError("Variable \"$".concat(varName, "\" of non-null type \"").concat(_varTypeStr2, "\" must not be null."), varDefNode));
      return "continue";
    }

    coercedValues[varName] = (0, _coerceInputValue.coerceInputValue)(value, varType, function (path, invalidValue, error) {
      var prefix = "Variable \"$".concat(varName, "\" got invalid value ") + (0, _inspect.default)(invalidValue);

      if (path.length > 0) {
        prefix += " at \"".concat(varName).concat((0, _printPathArray.default)(path), "\"");
      }

      onError(new _GraphQLError.GraphQLError(prefix + '; ' + error.message, varDefNode, undefined, undefined, undefined, error.originalError));
    });
  };

  for (var _i2 = 0; _i2 < varDefNodes.length; _i2++) {
    var _ret = _loop(_i2);

    if (_ret === "continue") continue;
  }

  return coercedValues;
}
/**
 * Prepares an object map of argument values given a list of argument
 * definitions and list of argument AST nodes.
 *
 * Note: The returned value is a plain Object with a prototype, since it is
 * exposed to user code. Care should be taken to not pull values from the
 * Object prototype.
 *
 * @internal
 */


function getArgumentValues(def, node, variableValues) {
  var _node$arguments;

  var coercedValues = {}; // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')

  var argumentNodes = (_node$arguments = node.arguments) !== null && _node$arguments !== void 0 ? _node$arguments : [];
  var argNodeMap = (0, _keyMap.default)(argumentNodes, function (arg) {
    return arg.name.value;
  });

  for (var _i4 = 0, _def$args2 = def.args; _i4 < _def$args2.length; _i4++) {
    var argDef = _def$args2[_i4];
    var name = argDef.name;
    var argType = argDef.type;
    var argumentNode = argNodeMap[name];

    if (!argumentNode) {
      if (argDef.defaultValue !== undefined) {
        coercedValues[name] = argDef.defaultValue;
      } else if ((0, _definition.isNonNullType)(argType)) {
        throw new _GraphQLError.GraphQLError("Argument \"".concat(name, "\" of required type \"").concat((0, _inspect.default)(argType), "\" ") + 'was not provided.', node);
      }

      continue;
    }

    var valueNode = argumentNode.value;
    var isNull = valueNode.kind === _kinds.Kind.NULL;

    if (valueNode.kind === _kinds.Kind.VARIABLE) {
      var variableName = valueNode.name.value;

      if (variableValues == null || !hasOwnProperty(variableValues, variableName)) {
        if (argDef.defaultValue !== undefined) {
          coercedValues[name] = argDef.defaultValue;
        } else if ((0, _definition.isNonNullType)(argType)) {
          throw new _GraphQLError.GraphQLError("Argument \"".concat(name, "\" of required type \"").concat((0, _inspect.default)(argType), "\" ") + "was provided the variable \"$".concat(variableName, "\" which was not provided a runtime value."), valueNode);
        }

        continue;
      }

      isNull = variableValues[variableName] == null;
    }

    if (isNull && (0, _definition.isNonNullType)(argType)) {
      throw new _GraphQLError.GraphQLError("Argument \"".concat(name, "\" of non-null type \"").concat((0, _inspect.default)(argType), "\" ") + 'must not be null.', valueNode);
    }

    var coercedValue = (0, _valueFromAST.valueFromAST)(valueNode, argType, variableValues);

    if (coercedValue === undefined) {
      // Note: ValuesOfCorrectTypeRule validation should catch this before
      // execution. This is a runtime check to ensure execution does not
      // continue with an invalid argument value.
      throw new _GraphQLError.GraphQLError("Argument \"".concat(name, "\" has invalid value ").concat((0, _printer.print)(valueNode), "."), valueNode);
    }

    coercedValues[name] = coercedValue;
  }

  return coercedValues;
}
/**
 * Prepares an object map of argument values given a directive definition
 * and a AST node which may contain directives. Optionally also accepts a map
 * of variable values.
 *
 * If the directive does not exist on the node, returns undefined.
 *
 * Note: The returned value is a plain Object with a prototype, since it is
 * exposed to user code. Care should be taken to not pull values from the
 * Object prototype.
 */


function getDirectiveValues(directiveDef, node, variableValues) {
  var directiveNode = node.directives && (0, _find.default)(node.directives, function (directive) {
    return directive.name.value === directiveDef.name;
  });

  if (directiveNode) {
    return getArgumentValues(directiveDef, directiveNode, variableValues);
  }
}

function hasOwnProperty(obj, prop) {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

},{"../error/GraphQLError.js":18,"../jsutils/inspect.js":33,"../jsutils/keyMap.js":39,"../jsutils/printPathArray.js":45,"../language/kinds.js":55,"../language/printer.js":61,"../polyfills/find.js":66,"../type/definition.js":75,"../utilities/coerceInputValue.js":87,"../utilities/typeFromAST.js":102,"../utilities/valueFromAST.js":103}],26:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.graphql = graphql;
exports.graphqlSync = graphqlSync;

var _isPromise = _interopRequireDefault(require("./jsutils/isPromise.js"));

var _parser = require("./language/parser.js");

var _validate = require("./validation/validate.js");

var _validate2 = require("./type/validate.js");

var _execute = require("./execution/execute.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function graphql(argsOrSchema, source, rootValue, contextValue, variableValues, operationName, fieldResolver, typeResolver) {
  var _arguments = arguments;

  /* eslint-enable no-redeclare */
  // Always return a Promise for a consistent API.
  return new Promise(function (resolve) {
    return resolve( // Extract arguments from object args if provided.
    _arguments.length === 1 ? graphqlImpl(argsOrSchema) : graphqlImpl({
      schema: argsOrSchema,
      source: source,
      rootValue: rootValue,
      contextValue: contextValue,
      variableValues: variableValues,
      operationName: operationName,
      fieldResolver: fieldResolver,
      typeResolver: typeResolver
    }));
  });
}
/**
 * The graphqlSync function also fulfills GraphQL operations by parsing,
 * validating, and executing a GraphQL document along side a GraphQL schema.
 * However, it guarantees to complete synchronously (or throw an error) assuming
 * that all field resolvers are also synchronous.
 */


function graphqlSync(argsOrSchema, source, rootValue, contextValue, variableValues, operationName, fieldResolver, typeResolver) {
  /* eslint-enable no-redeclare */
  // Extract arguments from object args if provided.
  var result = arguments.length === 1 ? graphqlImpl(argsOrSchema) : graphqlImpl({
    schema: argsOrSchema,
    source: source,
    rootValue: rootValue,
    contextValue: contextValue,
    variableValues: variableValues,
    operationName: operationName,
    fieldResolver: fieldResolver,
    typeResolver: typeResolver
  }); // Assert that the execution was synchronous.

  if ((0, _isPromise.default)(result)) {
    throw new Error('GraphQL execution failed to complete synchronously.');
  }

  return result;
}

function graphqlImpl(args) {
  var schema = args.schema,
      source = args.source,
      rootValue = args.rootValue,
      contextValue = args.contextValue,
      variableValues = args.variableValues,
      operationName = args.operationName,
      fieldResolver = args.fieldResolver,
      typeResolver = args.typeResolver; // Validate Schema

  var schemaValidationErrors = (0, _validate2.validateSchema)(schema);

  if (schemaValidationErrors.length > 0) {
    return {
      errors: schemaValidationErrors
    };
  } // Parse


  var document;

  try {
    document = (0, _parser.parse)(source);
  } catch (syntaxError) {
    return {
      errors: [syntaxError]
    };
  } // Validate


  var validationErrors = (0, _validate.validate)(schema, document);

  if (validationErrors.length > 0) {
    return {
      errors: validationErrors
    };
  } // Execute


  return (0, _execute.execute)({
    schema: schema,
    document: document,
    rootValue: rootValue,
    contextValue: contextValue,
    variableValues: variableValues,
    operationName: operationName,
    fieldResolver: fieldResolver,
    typeResolver: typeResolver
  });
}

},{"./execution/execute.js":23,"./jsutils/isPromise.js":38,"./language/parser.js":58,"./type/validate.js":81,"./validation/validate.js":143}],27:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "version", {
  enumerable: true,
  get: function get() {
    return _version.version;
  }
});
Object.defineProperty(exports, "versionInfo", {
  enumerable: true,
  get: function get() {
    return _version.versionInfo;
  }
});
Object.defineProperty(exports, "graphql", {
  enumerable: true,
  get: function get() {
    return _graphql.graphql;
  }
});
Object.defineProperty(exports, "graphqlSync", {
  enumerable: true,
  get: function get() {
    return _graphql.graphqlSync;
  }
});
Object.defineProperty(exports, "GraphQLSchema", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLSchema;
  }
});
Object.defineProperty(exports, "GraphQLDirective", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLDirective;
  }
});
Object.defineProperty(exports, "GraphQLScalarType", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLScalarType;
  }
});
Object.defineProperty(exports, "GraphQLObjectType", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLObjectType;
  }
});
Object.defineProperty(exports, "GraphQLInterfaceType", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLInterfaceType;
  }
});
Object.defineProperty(exports, "GraphQLUnionType", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLUnionType;
  }
});
Object.defineProperty(exports, "GraphQLEnumType", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLEnumType;
  }
});
Object.defineProperty(exports, "GraphQLInputObjectType", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLInputObjectType;
  }
});
Object.defineProperty(exports, "GraphQLList", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLList;
  }
});
Object.defineProperty(exports, "GraphQLNonNull", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLNonNull;
  }
});
Object.defineProperty(exports, "specifiedScalarTypes", {
  enumerable: true,
  get: function get() {
    return _index.specifiedScalarTypes;
  }
});
Object.defineProperty(exports, "GraphQLInt", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLInt;
  }
});
Object.defineProperty(exports, "GraphQLFloat", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLFloat;
  }
});
Object.defineProperty(exports, "GraphQLString", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLString;
  }
});
Object.defineProperty(exports, "GraphQLBoolean", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLBoolean;
  }
});
Object.defineProperty(exports, "GraphQLID", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLID;
  }
});
Object.defineProperty(exports, "specifiedDirectives", {
  enumerable: true,
  get: function get() {
    return _index.specifiedDirectives;
  }
});
Object.defineProperty(exports, "GraphQLIncludeDirective", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLIncludeDirective;
  }
});
Object.defineProperty(exports, "GraphQLSkipDirective", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLSkipDirective;
  }
});
Object.defineProperty(exports, "GraphQLDeprecatedDirective", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLDeprecatedDirective;
  }
});
Object.defineProperty(exports, "GraphQLSpecifiedByDirective", {
  enumerable: true,
  get: function get() {
    return _index.GraphQLSpecifiedByDirective;
  }
});
Object.defineProperty(exports, "TypeKind", {
  enumerable: true,
  get: function get() {
    return _index.TypeKind;
  }
});
Object.defineProperty(exports, "DEFAULT_DEPRECATION_REASON", {
  enumerable: true,
  get: function get() {
    return _index.DEFAULT_DEPRECATION_REASON;
  }
});
Object.defineProperty(exports, "introspectionTypes", {
  enumerable: true,
  get: function get() {
    return _index.introspectionTypes;
  }
});
Object.defineProperty(exports, "__Schema", {
  enumerable: true,
  get: function get() {
    return _index.__Schema;
  }
});
Object.defineProperty(exports, "__Directive", {
  enumerable: true,
  get: function get() {
    return _index.__Directive;
  }
});
Object.defineProperty(exports, "__DirectiveLocation", {
  enumerable: true,
  get: function get() {
    return _index.__DirectiveLocation;
  }
});
Object.defineProperty(exports, "__Type", {
  enumerable: true,
  get: function get() {
    return _index.__Type;
  }
});
Object.defineProperty(exports, "__Field", {
  enumerable: true,
  get: function get() {
    return _index.__Field;
  }
});
Object.defineProperty(exports, "__InputValue", {
  enumerable: true,
  get: function get() {
    return _index.__InputValue;
  }
});
Object.defineProperty(exports, "__EnumValue", {
  enumerable: true,
  get: function get() {
    return _index.__EnumValue;
  }
});
Object.defineProperty(exports, "__TypeKind", {
  enumerable: true,
  get: function get() {
    return _index.__TypeKind;
  }
});
Object.defineProperty(exports, "SchemaMetaFieldDef", {
  enumerable: true,
  get: function get() {
    return _index.SchemaMetaFieldDef;
  }
});
Object.defineProperty(exports, "TypeMetaFieldDef", {
  enumerable: true,
  get: function get() {
    return _index.TypeMetaFieldDef;
  }
});
Object.defineProperty(exports, "TypeNameMetaFieldDef", {
  enumerable: true,
  get: function get() {
    return _index.TypeNameMetaFieldDef;
  }
});
Object.defineProperty(exports, "isSchema", {
  enumerable: true,
  get: function get() {
    return _index.isSchema;
  }
});
Object.defineProperty(exports, "isDirective", {
  enumerable: true,
  get: function get() {
    return _index.isDirective;
  }
});
Object.defineProperty(exports, "isType", {
  enumerable: true,
  get: function get() {
    return _index.isType;
  }
});
Object.defineProperty(exports, "isScalarType", {
  enumerable: true,
  get: function get() {
    return _index.isScalarType;
  }
});
Object.defineProperty(exports, "isObjectType", {
  enumerable: true,
  get: function get() {
    return _index.isObjectType;
  }
});
Object.defineProperty(exports, "isInterfaceType", {
  enumerable: true,
  get: function get() {
    return _index.isInterfaceType;
  }
});
Object.defineProperty(exports, "isUnionType", {
  enumerable: true,
  get: function get() {
    return _index.isUnionType;
  }
});
Object.defineProperty(exports, "isEnumType", {
  enumerable: true,
  get: function get() {
    return _index.isEnumType;
  }
});
Object.defineProperty(exports, "isInputObjectType", {
  enumerable: true,
  get: function get() {
    return _index.isInputObjectType;
  }
});
Object.defineProperty(exports, "isListType", {
  enumerable: true,
  get: function get() {
    return _index.isListType;
  }
});
Object.defineProperty(exports, "isNonNullType", {
  enumerable: true,
  get: function get() {
    return _index.isNonNullType;
  }
});
Object.defineProperty(exports, "isInputType", {
  enumerable: true,
  get: function get() {
    return _index.isInputType;
  }
});
Object.defineProperty(exports, "isOutputType", {
  enumerable: true,
  get: function get() {
    return _index.isOutputType;
  }
});
Object.defineProperty(exports, "isLeafType", {
  enumerable: true,
  get: function get() {
    return _index.isLeafType;
  }
});
Object.defineProperty(exports, "isCompositeType", {
  enumerable: true,
  get: function get() {
    return _index.isCompositeType;
  }
});
Object.defineProperty(exports, "isAbstractType", {
  enumerable: true,
  get: function get() {
    return _index.isAbstractType;
  }
});
Object.defineProperty(exports, "isWrappingType", {
  enumerable: true,
  get: function get() {
    return _index.isWrappingType;
  }
});
Object.defineProperty(exports, "isNullableType", {
  enumerable: true,
  get: function get() {
    return _index.isNullableType;
  }
});
Object.defineProperty(exports, "isNamedType", {
  enumerable: true,
  get: function get() {
    return _index.isNamedType;
  }
});
Object.defineProperty(exports, "isRequiredArgument", {
  enumerable: true,
  get: function get() {
    return _index.isRequiredArgument;
  }
});
Object.defineProperty(exports, "isRequiredInputField", {
  enumerable: true,
  get: function get() {
    return _index.isRequiredInputField;
  }
});
Object.defineProperty(exports, "isSpecifiedScalarType", {
  enumerable: true,
  get: function get() {
    return _index.isSpecifiedScalarType;
  }
});
Object.defineProperty(exports, "isIntrospectionType", {
  enumerable: true,
  get: function get() {
    return _index.isIntrospectionType;
  }
});
Object.defineProperty(exports, "isSpecifiedDirective", {
  enumerable: true,
  get: function get() {
    return _index.isSpecifiedDirective;
  }
});
Object.defineProperty(exports, "assertSchema", {
  enumerable: true,
  get: function get() {
    return _index.assertSchema;
  }
});
Object.defineProperty(exports, "assertDirective", {
  enumerable: true,
  get: function get() {
    return _index.assertDirective;
  }
});
Object.defineProperty(exports, "assertType", {
  enumerable: true,
  get: function get() {
    return _index.assertType;
  }
});
Object.defineProperty(exports, "assertScalarType", {
  enumerable: true,
  get: function get() {
    return _index.assertScalarType;
  }
});
Object.defineProperty(exports, "assertObjectType", {
  enumerable: true,
  get: function get() {
    return _index.assertObjectType;
  }
});
Object.defineProperty(exports, "assertInterfaceType", {
  enumerable: true,
  get: function get() {
    return _index.assertInterfaceType;
  }
});
Object.defineProperty(exports, "assertUnionType", {
  enumerable: true,
  get: function get() {
    return _index.assertUnionType;
  }
});
Object.defineProperty(exports, "assertEnumType", {
  enumerable: true,
  get: function get() {
    return _index.assertEnumType;
  }
});
Object.defineProperty(exports, "assertInputObjectType", {
  enumerable: true,
  get: function get() {
    return _index.assertInputObjectType;
  }
});
Object.defineProperty(exports, "assertListType", {
  enumerable: true,
  get: function get() {
    return _index.assertListType;
  }
});
Object.defineProperty(exports, "assertNonNullType", {
  enumerable: true,
  get: function get() {
    return _index.assertNonNullType;
  }
});
Object.defineProperty(exports, "assertInputType", {
  enumerable: true,
  get: function get() {
    return _index.assertInputType;
  }
});
Object.defineProperty(exports, "assertOutputType", {
  enumerable: true,
  get: function get() {
    return _index.assertOutputType;
  }
});
Object.defineProperty(exports, "assertLeafType", {
  enumerable: true,
  get: function get() {
    return _index.assertLeafType;
  }
});
Object.defineProperty(exports, "assertCompositeType", {
  enumerable: true,
  get: function get() {
    return _index.assertCompositeType;
  }
});
Object.defineProperty(exports, "assertAbstractType", {
  enumerable: true,
  get: function get() {
    return _index.assertAbstractType;
  }
});
Object.defineProperty(exports, "assertWrappingType", {
  enumerable: true,
  get: function get() {
    return _index.assertWrappingType;
  }
});
Object.defineProperty(exports, "assertNullableType", {
  enumerable: true,
  get: function get() {
    return _index.assertNullableType;
  }
});
Object.defineProperty(exports, "assertNamedType", {
  enumerable: true,
  get: function get() {
    return _index.assertNamedType;
  }
});
Object.defineProperty(exports, "getNullableType", {
  enumerable: true,
  get: function get() {
    return _index.getNullableType;
  }
});
Object.defineProperty(exports, "getNamedType", {
  enumerable: true,
  get: function get() {
    return _index.getNamedType;
  }
});
Object.defineProperty(exports, "validateSchema", {
  enumerable: true,
  get: function get() {
    return _index.validateSchema;
  }
});
Object.defineProperty(exports, "assertValidSchema", {
  enumerable: true,
  get: function get() {
    return _index.assertValidSchema;
  }
});
Object.defineProperty(exports, "Token", {
  enumerable: true,
  get: function get() {
    return _index2.Token;
  }
});
Object.defineProperty(exports, "Source", {
  enumerable: true,
  get: function get() {
    return _index2.Source;
  }
});
Object.defineProperty(exports, "Location", {
  enumerable: true,
  get: function get() {
    return _index2.Location;
  }
});
Object.defineProperty(exports, "getLocation", {
  enumerable: true,
  get: function get() {
    return _index2.getLocation;
  }
});
Object.defineProperty(exports, "printLocation", {
  enumerable: true,
  get: function get() {
    return _index2.printLocation;
  }
});
Object.defineProperty(exports, "printSourceLocation", {
  enumerable: true,
  get: function get() {
    return _index2.printSourceLocation;
  }
});
Object.defineProperty(exports, "Lexer", {
  enumerable: true,
  get: function get() {
    return _index2.Lexer;
  }
});
Object.defineProperty(exports, "TokenKind", {
  enumerable: true,
  get: function get() {
    return _index2.TokenKind;
  }
});
Object.defineProperty(exports, "parse", {
  enumerable: true,
  get: function get() {
    return _index2.parse;
  }
});
Object.defineProperty(exports, "parseValue", {
  enumerable: true,
  get: function get() {
    return _index2.parseValue;
  }
});
Object.defineProperty(exports, "parseType", {
  enumerable: true,
  get: function get() {
    return _index2.parseType;
  }
});
Object.defineProperty(exports, "print", {
  enumerable: true,
  get: function get() {
    return _index2.print;
  }
});
Object.defineProperty(exports, "visit", {
  enumerable: true,
  get: function get() {
    return _index2.visit;
  }
});
Object.defineProperty(exports, "visitInParallel", {
  enumerable: true,
  get: function get() {
    return _index2.visitInParallel;
  }
});
Object.defineProperty(exports, "getVisitFn", {
  enumerable: true,
  get: function get() {
    return _index2.getVisitFn;
  }
});
Object.defineProperty(exports, "BREAK", {
  enumerable: true,
  get: function get() {
    return _index2.BREAK;
  }
});
Object.defineProperty(exports, "Kind", {
  enumerable: true,
  get: function get() {
    return _index2.Kind;
  }
});
Object.defineProperty(exports, "DirectiveLocation", {
  enumerable: true,
  get: function get() {
    return _index2.DirectiveLocation;
  }
});
Object.defineProperty(exports, "isDefinitionNode", {
  enumerable: true,
  get: function get() {
    return _index2.isDefinitionNode;
  }
});
Object.defineProperty(exports, "isExecutableDefinitionNode", {
  enumerable: true,
  get: function get() {
    return _index2.isExecutableDefinitionNode;
  }
});
Object.defineProperty(exports, "isSelectionNode", {
  enumerable: true,
  get: function get() {
    return _index2.isSelectionNode;
  }
});
Object.defineProperty(exports, "isValueNode", {
  enumerable: true,
  get: function get() {
    return _index2.isValueNode;
  }
});
Object.defineProperty(exports, "isTypeNode", {
  enumerable: true,
  get: function get() {
    return _index2.isTypeNode;
  }
});
Object.defineProperty(exports, "isTypeSystemDefinitionNode", {
  enumerable: true,
  get: function get() {
    return _index2.isTypeSystemDefinitionNode;
  }
});
Object.defineProperty(exports, "isTypeDefinitionNode", {
  enumerable: true,
  get: function get() {
    return _index2.isTypeDefinitionNode;
  }
});
Object.defineProperty(exports, "isTypeSystemExtensionNode", {
  enumerable: true,
  get: function get() {
    return _index2.isTypeSystemExtensionNode;
  }
});
Object.defineProperty(exports, "isTypeExtensionNode", {
  enumerable: true,
  get: function get() {
    return _index2.isTypeExtensionNode;
  }
});
Object.defineProperty(exports, "execute", {
  enumerable: true,
  get: function get() {
    return _index3.execute;
  }
});
Object.defineProperty(exports, "executeSync", {
  enumerable: true,
  get: function get() {
    return _index3.executeSync;
  }
});
Object.defineProperty(exports, "defaultFieldResolver", {
  enumerable: true,
  get: function get() {
    return _index3.defaultFieldResolver;
  }
});
Object.defineProperty(exports, "defaultTypeResolver", {
  enumerable: true,
  get: function get() {
    return _index3.defaultTypeResolver;
  }
});
Object.defineProperty(exports, "responsePathAsArray", {
  enumerable: true,
  get: function get() {
    return _index3.responsePathAsArray;
  }
});
Object.defineProperty(exports, "getDirectiveValues", {
  enumerable: true,
  get: function get() {
    return _index3.getDirectiveValues;
  }
});
Object.defineProperty(exports, "subscribe", {
  enumerable: true,
  get: function get() {
    return _index4.subscribe;
  }
});
Object.defineProperty(exports, "createSourceEventStream", {
  enumerable: true,
  get: function get() {
    return _index4.createSourceEventStream;
  }
});
Object.defineProperty(exports, "validate", {
  enumerable: true,
  get: function get() {
    return _index5.validate;
  }
});
Object.defineProperty(exports, "ValidationContext", {
  enumerable: true,
  get: function get() {
    return _index5.ValidationContext;
  }
});
Object.defineProperty(exports, "specifiedRules", {
  enumerable: true,
  get: function get() {
    return _index5.specifiedRules;
  }
});
Object.defineProperty(exports, "ExecutableDefinitionsRule", {
  enumerable: true,
  get: function get() {
    return _index5.ExecutableDefinitionsRule;
  }
});
Object.defineProperty(exports, "FieldsOnCorrectTypeRule", {
  enumerable: true,
  get: function get() {
    return _index5.FieldsOnCorrectTypeRule;
  }
});
Object.defineProperty(exports, "FragmentsOnCompositeTypesRule", {
  enumerable: true,
  get: function get() {
    return _index5.FragmentsOnCompositeTypesRule;
  }
});
Object.defineProperty(exports, "KnownArgumentNamesRule", {
  enumerable: true,
  get: function get() {
    return _index5.KnownArgumentNamesRule;
  }
});
Object.defineProperty(exports, "KnownDirectivesRule", {
  enumerable: true,
  get: function get() {
    return _index5.KnownDirectivesRule;
  }
});
Object.defineProperty(exports, "KnownFragmentNamesRule", {
  enumerable: true,
  get: function get() {
    return _index5.KnownFragmentNamesRule;
  }
});
Object.defineProperty(exports, "KnownTypeNamesRule", {
  enumerable: true,
  get: function get() {
    return _index5.KnownTypeNamesRule;
  }
});
Object.defineProperty(exports, "LoneAnonymousOperationRule", {
  enumerable: true,
  get: function get() {
    return _index5.LoneAnonymousOperationRule;
  }
});
Object.defineProperty(exports, "NoFragmentCyclesRule", {
  enumerable: true,
  get: function get() {
    return _index5.NoFragmentCyclesRule;
  }
});
Object.defineProperty(exports, "NoUndefinedVariablesRule", {
  enumerable: true,
  get: function get() {
    return _index5.NoUndefinedVariablesRule;
  }
});
Object.defineProperty(exports, "NoUnusedFragmentsRule", {
  enumerable: true,
  get: function get() {
    return _index5.NoUnusedFragmentsRule;
  }
});
Object.defineProperty(exports, "NoUnusedVariablesRule", {
  enumerable: true,
  get: function get() {
    return _index5.NoUnusedVariablesRule;
  }
});
Object.defineProperty(exports, "OverlappingFieldsCanBeMergedRule", {
  enumerable: true,
  get: function get() {
    return _index5.OverlappingFieldsCanBeMergedRule;
  }
});
Object.defineProperty(exports, "PossibleFragmentSpreadsRule", {
  enumerable: true,
  get: function get() {
    return _index5.PossibleFragmentSpreadsRule;
  }
});
Object.defineProperty(exports, "ProvidedRequiredArgumentsRule", {
  enumerable: true,
  get: function get() {
    return _index5.ProvidedRequiredArgumentsRule;
  }
});
Object.defineProperty(exports, "ScalarLeafsRule", {
  enumerable: true,
  get: function get() {
    return _index5.ScalarLeafsRule;
  }
});
Object.defineProperty(exports, "SingleFieldSubscriptionsRule", {
  enumerable: true,
  get: function get() {
    return _index5.SingleFieldSubscriptionsRule;
  }
});
Object.defineProperty(exports, "UniqueArgumentNamesRule", {
  enumerable: true,
  get: function get() {
    return _index5.UniqueArgumentNamesRule;
  }
});
Object.defineProperty(exports, "UniqueDirectivesPerLocationRule", {
  enumerable: true,
  get: function get() {
    return _index5.UniqueDirectivesPerLocationRule;
  }
});
Object.defineProperty(exports, "UniqueFragmentNamesRule", {
  enumerable: true,
  get: function get() {
    return _index5.UniqueFragmentNamesRule;
  }
});
Object.defineProperty(exports, "UniqueInputFieldNamesRule", {
  enumerable: true,
  get: function get() {
    return _index5.UniqueInputFieldNamesRule;
  }
});
Object.defineProperty(exports, "UniqueOperationNamesRule", {
  enumerable: true,
  get: function get() {
    return _index5.UniqueOperationNamesRule;
  }
});
Object.defineProperty(exports, "UniqueVariableNamesRule", {
  enumerable: true,
  get: function get() {
    return _index5.UniqueVariableNamesRule;
  }
});
Object.defineProperty(exports, "ValuesOfCorrectTypeRule", {
  enumerable: true,
  get: function get() {
    return _index5.ValuesOfCorrectTypeRule;
  }
});
Object.defineProperty(exports, "VariablesAreInputTypesRule", {
  enumerable: true,
  get: function get() {
    return _index5.VariablesAreInputTypesRule;
  }
});
Object.defineProperty(exports, "VariablesInAllowedPositionRule", {
  enumerable: true,
  get: function get() {
    return _index5.VariablesInAllowedPositionRule;
  }
});
Object.defineProperty(exports, "LoneSchemaDefinitionRule", {
  enumerable: true,
  get: function get() {
    return _index5.LoneSchemaDefinitionRule;
  }
});
Object.defineProperty(exports, "UniqueOperationTypesRule", {
  enumerable: true,
  get: function get() {
    return _index5.UniqueOperationTypesRule;
  }
});
Object.defineProperty(exports, "UniqueTypeNamesRule", {
  enumerable: true,
  get: function get() {
    return _index5.UniqueTypeNamesRule;
  }
});
Object.defineProperty(exports, "UniqueEnumValueNamesRule", {
  enumerable: true,
  get: function get() {
    return _index5.UniqueEnumValueNamesRule;
  }
});
Object.defineProperty(exports, "UniqueFieldDefinitionNamesRule", {
  enumerable: true,
  get: function get() {
    return _index5.UniqueFieldDefinitionNamesRule;
  }
});
Object.defineProperty(exports, "UniqueDirectiveNamesRule", {
  enumerable: true,
  get: function get() {
    return _index5.UniqueDirectiveNamesRule;
  }
});
Object.defineProperty(exports, "PossibleTypeExtensionsRule", {
  enumerable: true,
  get: function get() {
    return _index5.PossibleTypeExtensionsRule;
  }
});
Object.defineProperty(exports, "NoDeprecatedCustomRule", {
  enumerable: true,
  get: function get() {
    return _index5.NoDeprecatedCustomRule;
  }
});
Object.defineProperty(exports, "NoSchemaIntrospectionCustomRule", {
  enumerable: true,
  get: function get() {
    return _index5.NoSchemaIntrospectionCustomRule;
  }
});
Object.defineProperty(exports, "GraphQLError", {
  enumerable: true,
  get: function get() {
    return _index6.GraphQLError;
  }
});
Object.defineProperty(exports, "syntaxError", {
  enumerable: true,
  get: function get() {
    return _index6.syntaxError;
  }
});
Object.defineProperty(exports, "locatedError", {
  enumerable: true,
  get: function get() {
    return _index6.locatedError;
  }
});
Object.defineProperty(exports, "printError", {
  enumerable: true,
  get: function get() {
    return _index6.printError;
  }
});
Object.defineProperty(exports, "formatError", {
  enumerable: true,
  get: function get() {
    return _index6.formatError;
  }
});
Object.defineProperty(exports, "getIntrospectionQuery", {
  enumerable: true,
  get: function get() {
    return _index7.getIntrospectionQuery;
  }
});
Object.defineProperty(exports, "getOperationAST", {
  enumerable: true,
  get: function get() {
    return _index7.getOperationAST;
  }
});
Object.defineProperty(exports, "getOperationRootType", {
  enumerable: true,
  get: function get() {
    return _index7.getOperationRootType;
  }
});
Object.defineProperty(exports, "introspectionFromSchema", {
  enumerable: true,
  get: function get() {
    return _index7.introspectionFromSchema;
  }
});
Object.defineProperty(exports, "buildClientSchema", {
  enumerable: true,
  get: function get() {
    return _index7.buildClientSchema;
  }
});
Object.defineProperty(exports, "buildASTSchema", {
  enumerable: true,
  get: function get() {
    return _index7.buildASTSchema;
  }
});
Object.defineProperty(exports, "buildSchema", {
  enumerable: true,
  get: function get() {
    return _index7.buildSchema;
  }
});
Object.defineProperty(exports, "getDescription", {
  enumerable: true,
  get: function get() {
    return _index7.getDescription;
  }
});
Object.defineProperty(exports, "extendSchema", {
  enumerable: true,
  get: function get() {
    return _index7.extendSchema;
  }
});
Object.defineProperty(exports, "lexicographicSortSchema", {
  enumerable: true,
  get: function get() {
    return _index7.lexicographicSortSchema;
  }
});
Object.defineProperty(exports, "printSchema", {
  enumerable: true,
  get: function get() {
    return _index7.printSchema;
  }
});
Object.defineProperty(exports, "printType", {
  enumerable: true,
  get: function get() {
    return _index7.printType;
  }
});
Object.defineProperty(exports, "printIntrospectionSchema", {
  enumerable: true,
  get: function get() {
    return _index7.printIntrospectionSchema;
  }
});
Object.defineProperty(exports, "typeFromAST", {
  enumerable: true,
  get: function get() {
    return _index7.typeFromAST;
  }
});
Object.defineProperty(exports, "valueFromAST", {
  enumerable: true,
  get: function get() {
    return _index7.valueFromAST;
  }
});
Object.defineProperty(exports, "valueFromASTUntyped", {
  enumerable: true,
  get: function get() {
    return _index7.valueFromASTUntyped;
  }
});
Object.defineProperty(exports, "astFromValue", {
  enumerable: true,
  get: function get() {
    return _index7.astFromValue;
  }
});
Object.defineProperty(exports, "TypeInfo", {
  enumerable: true,
  get: function get() {
    return _index7.TypeInfo;
  }
});
Object.defineProperty(exports, "visitWithTypeInfo", {
  enumerable: true,
  get: function get() {
    return _index7.visitWithTypeInfo;
  }
});
Object.defineProperty(exports, "coerceInputValue", {
  enumerable: true,
  get: function get() {
    return _index7.coerceInputValue;
  }
});
Object.defineProperty(exports, "concatAST", {
  enumerable: true,
  get: function get() {
    return _index7.concatAST;
  }
});
Object.defineProperty(exports, "separateOperations", {
  enumerable: true,
  get: function get() {
    return _index7.separateOperations;
  }
});
Object.defineProperty(exports, "stripIgnoredCharacters", {
  enumerable: true,
  get: function get() {
    return _index7.stripIgnoredCharacters;
  }
});
Object.defineProperty(exports, "isEqualType", {
  enumerable: true,
  get: function get() {
    return _index7.isEqualType;
  }
});
Object.defineProperty(exports, "isTypeSubTypeOf", {
  enumerable: true,
  get: function get() {
    return _index7.isTypeSubTypeOf;
  }
});
Object.defineProperty(exports, "doTypesOverlap", {
  enumerable: true,
  get: function get() {
    return _index7.doTypesOverlap;
  }
});
Object.defineProperty(exports, "assertValidName", {
  enumerable: true,
  get: function get() {
    return _index7.assertValidName;
  }
});
Object.defineProperty(exports, "isValidNameError", {
  enumerable: true,
  get: function get() {
    return _index7.isValidNameError;
  }
});
Object.defineProperty(exports, "BreakingChangeType", {
  enumerable: true,
  get: function get() {
    return _index7.BreakingChangeType;
  }
});
Object.defineProperty(exports, "DangerousChangeType", {
  enumerable: true,
  get: function get() {
    return _index7.DangerousChangeType;
  }
});
Object.defineProperty(exports, "findBreakingChanges", {
  enumerable: true,
  get: function get() {
    return _index7.findBreakingChanges;
  }
});
Object.defineProperty(exports, "findDangerousChanges", {
  enumerable: true,
  get: function get() {
    return _index7.findDangerousChanges;
  }
});
Object.defineProperty(exports, "findDeprecatedUsages", {
  enumerable: true,
  get: function get() {
    return _index7.findDeprecatedUsages;
  }
});

var _version = require("./version.js");

var _graphql = require("./graphql.js");

var _index = require("./type/index.js");

var _index2 = require("./language/index.js");

var _index3 = require("./execution/index.js");

var _index4 = require("./subscription/index.js");

var _index5 = require("./validation/index.js");

var _index6 = require("./error/index.js");

var _index7 = require("./utilities/index.js");

},{"./error/index.js":20,"./execution/index.js":24,"./graphql.js":26,"./language/index.js":54,"./subscription/index.js":72,"./type/index.js":77,"./utilities/index.js":95,"./validation/index.js":106,"./version.js":144}],28:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.addPath = addPath;
exports.pathToArray = pathToArray;

/**
 * Given a Path and a key, return a new Path containing the new key.
 */
function addPath(prev, key, typename) {
  return {
    prev: prev,
    key: key,
    typename: typename
  };
}
/**
 * Given a Path, return an Array of the path keys.
 */


function pathToArray(path) {
  var flattened = [];
  var curr = path;

  while (curr) {
    flattened.push(curr.key);
    curr = curr.prev;
  }

  return flattened.reverse();
}

},{}],29:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = defineInspect;

var _invariant = _interopRequireDefault(require("./invariant.js"));

var _nodejsCustomInspectSymbol = _interopRequireDefault(require("./nodejsCustomInspectSymbol.js"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * The `defineInspect()` function defines `inspect()` prototype method as alias of `toJSON`
 */
function defineInspect(classObject) {
  var fn = classObject.prototype.toJSON;
  typeof fn === 'function' || (0, _invariant.default)(0);
  classObject.prototype.inspect = fn; // istanbul ignore else (See: 'https://github.com/graphql/graphql-js/issues/2317')

  if (_nodejsCustomInspectSymbol.default) {
    classObject.prototype[_nodejsCustomInspectSymbol.default] = fn;
  }
}

},{"./invariant.js":35,"./nodejsCustomInspectSymbol.js":44}],30:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = devAssert;

function devAssert(condition, message) {
  var booleanCondition = Boolean(condition); // istanbul ignore else (See transformation done in './resources/inlineInvariant.js')

  if (!booleanCondition) {
    throw new Error(message);
  }
}

},{}],31:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = didYouMean;
var MAX_SUGGESTIONS = 5;
/**
 * Given [ A, B, C ] return ' Did you mean A, B, or C?'.
 */

// eslint-disable-next-line no-redeclare
function didYouMean(firstArg, secondArg) {
  var _ref = typeof firstArg === 'string' ? [firstArg, secondArg] : [undefined, firstArg],
      subMessage = _ref[0],
      suggestionsArg = _ref[1];

  var message = ' Did you mean ';

  if (subMessage) {
    message += subMessage + ' ';
  }

  var suggestions = suggestionsArg.map(function (x) {
    return "\"".concat(x, "\"");
  });

  switch (suggestions.length) {
    case 0:
      return '';

    case 1:
      return message + suggestions[0] + '?';

    case 2:
      return message + suggestions[0] + ' or ' + suggestions[1] + '?';
  }

  var selected = suggestions.slice(0, MAX_SUGGESTIONS);
  var lastItem = selected.pop();
  return message + selected.join(', ') + ', or ' + lastItem + '?';
}

},{}],32:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = identityFunc;

/**
 * Returns the first argument it receives.
 */
function identityFunc(x) {
  return x;
}

},{}],33:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = inspect;

var _nodejsCustomInspectSymbol = _interopRequireDefault(require("./nodejsCustomInspectSymbol.js"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _typeof(obj) { "@babel/helpers - typeof"; if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

var MAX_ARRAY_LENGTH = 10;
var MAX_RECURSIVE_DEPTH = 2;
/**
 * Used to print values in error messages.
 */

function inspect(value) {
  return formatValue(value, []);
}

function formatValue(value, seenValues) {
  switch (_typeof(value)) {
    case 'string':
      return JSON.stringify(value);

    case 'function':
      return value.name ? "[function ".concat(value.name, "]") : '[function]';

    case 'object':
      if (value === null) {
        return 'null';
      }

      return formatObjectValue(value, seenValues);

    default:
      return String(value);
  }
}

function formatObjectValue(value, previouslySeenValues) {
  if (previouslySeenValues.indexOf(value) !== -1) {
    return '[Circular]';
  }

  var seenValues = [].concat(previouslySeenValues, [value]);
  var customInspectFn = getCustomFn(value);

  if (customInspectFn !== undefined) {
    var customValue = customInspectFn.call(value); // check for infinite recursion

    if (customValue !== value) {
      return typeof customValue === 'string' ? customValue : formatValue(customValue, seenValues);
    }
  } else if (Array.isArray(value)) {
    return formatArray(value, seenValues);
  }

  return formatObject(value, seenValues);
}

function formatObject(object, seenValues) {
  var keys = Object.keys(object);

  if (keys.length === 0) {
    return '{}';
  }

  if (seenValues.length > MAX_RECURSIVE_DEPTH) {
    return '[' + getObjectTag(object) + ']';
  }

  var properties = keys.map(function (key) {
    var value = formatValue(object[key], seenValues);
    return key + ': ' + value;
  });
  return '{ ' + properties.join(', ') + ' }';
}

function formatArray(array, seenValues) {
  if (array.length === 0) {
    return '[]';
  }

  if (seenValues.length > MAX_RECURSIVE_DEPTH) {
    return '[Array]';
  }

  var len = Math.min(MAX_ARRAY_LENGTH, array.length);
  var remaining = array.length - len;
  var items = [];

  for (var i = 0; i < len; ++i) {
    items.push(formatValue(array[i], seenValues));
  }

  if (remaining === 1) {
    items.push('... 1 more item');
  } else if (remaining > 1) {
    items.push("... ".concat(remaining, " more items"));
  }

  return '[' + items.join(', ') + ']';
}

function getCustomFn(object) {
  var customInspectFn = object[String(_nodejsCustomInspectSymbol.default)];

  if (typeof customInspectFn === 'function') {
    return customInspectFn;
  }

  if (typeof object.inspect === 'function') {
    return object.inspect;
  }
}

function getObjectTag(object) {
  var tag = Object.prototype.toString.call(object).replace(/^\[object /, '').replace(/]$/, '');

  if (tag === 'Object' && typeof object.constructor === 'function') {
    var name = object.constructor.name;

    if (typeof name === 'string' && name !== '') {
      return name;
    }
  }

  return tag;
}

},{"./nodejsCustomInspectSymbol.js":44}],34:[function(require,module,exports){
(function (process){(function (){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

/**
 * A replacement for instanceof which includes an error warning when multi-realm
 * constructors are detected.
 */
// See: https://expressjs.com/en/advanced/best-practice-performance.html#set-node_env-to-production
// See: https://webpack.js.org/guides/production/
var _default = process.env.NODE_ENV === 'production' ? // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2317')
// eslint-disable-next-line no-shadow
function instanceOf(value, constructor) {
  return value instanceof constructor;
} : // eslint-disable-next-line no-shadow
function instanceOf(value, constructor) {
  if (value instanceof constructor) {
    return true;
  }

  if (value) {
    var valueClass = value.constructor;
    var className = constructor.name;

    if (className && valueClass && valueClass.name === className) {
      throw new Error("Cannot use ".concat(className, " \"").concat(value, "\" from another module or realm.\n\nEnsure that there is only one instance of \"graphql\" in the node_modules\ndirectory. If different versions of \"graphql\" are the dependencies of other\nrelied on modules, use \"resolutions\" to ensure only one version is installed.\n\nhttps://yarnpkg.com/en/docs/selective-version-resolutions\n\nDuplicate \"graphql\" modules cannot be used at the same time since different\nversions may have different capabilities and behavior. The data from one\nversion used in the function from another could produce confusing and\nspurious results."));
    }
  }

  return false;
};

exports.default = _default;

}).call(this)}).call(this,require('_process'))
},{"_process":146}],35:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = invariant;

function invariant(condition, message) {
  var booleanCondition = Boolean(condition); // istanbul ignore else (See transformation done in './resources/inlineInvariant.js')

  if (!booleanCondition) {
    throw new Error(message != null ? message : 'Unexpected invariant triggered.');
  }
}

},{}],36:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = isAsyncIterable;

var _symbols = require("../polyfills/symbols.js");

// eslint-disable-next-line no-redeclare
function isAsyncIterable(maybeAsyncIterable) {
  return typeof (maybeAsyncIterable === null || maybeAsyncIterable === void 0 ? void 0 : maybeAsyncIterable[_symbols.SYMBOL_ASYNC_ITERATOR]) === 'function';
}

},{"../polyfills/symbols.js":71}],37:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = isObjectLike;

function _typeof(obj) { "@babel/helpers - typeof"; if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

/**
 * Return true if `value` is object-like. A value is object-like if it's not
 * `null` and has a `typeof` result of "object".
 */
function isObjectLike(value) {
  return _typeof(value) == 'object' && value !== null;
}

},{}],38:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = isPromise;

/**
 * Returns true if the value acts like a Promise, i.e. has a "then" function,
 * otherwise returns false.
 */
// eslint-disable-next-line no-redeclare
function isPromise(value) {
  return typeof (value === null || value === void 0 ? void 0 : value.then) === 'function';
}

},{}],39:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = keyMap;

/**
 * Creates a keyed JS object from an array, given a function to produce the keys
 * for each value in the array.
 *
 * This provides a convenient lookup for the array items if the key function
 * produces unique results.
 *
 *     const phoneBook = [
 *       { name: 'Jon', num: '555-1234' },
 *       { name: 'Jenny', num: '867-5309' }
 *     ]
 *
 *     // { Jon: { name: 'Jon', num: '555-1234' },
 *     //   Jenny: { name: 'Jenny', num: '867-5309' } }
 *     const entriesByName = keyMap(
 *       phoneBook,
 *       entry => entry.name
 *     )
 *
 *     // { name: 'Jenny', num: '857-6309' }
 *     const jennyEntry = entriesByName['Jenny']
 *
 */
function keyMap(list, keyFn) {
  return list.reduce(function (map, item) {
    map[keyFn(item)] = item;
    return map;
  }, Object.create(null));
}

},{}],40:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = keyValMap;

/**
 * Creates a keyed JS object from an array, given a function to produce the keys
 * and a function to produce the values from each item in the array.
 *
 *     const phoneBook = [
 *       { name: 'Jon', num: '555-1234' },
 *       { name: 'Jenny', num: '867-5309' }
 *     ]
 *
 *     // { Jon: '555-1234', Jenny: '867-5309' }
 *     const phonesByName = keyValMap(
 *       phoneBook,
 *       entry => entry.name,
 *       entry => entry.num
 *     )
 *
 */
function keyValMap(list, keyFn, valFn) {
  return list.reduce(function (map, item) {
    map[keyFn(item)] = valFn(item);
    return map;
  }, Object.create(null));
}

},{}],41:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = mapValue;

var _objectEntries3 = _interopRequireDefault(require("../polyfills/objectEntries.js"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Creates an object map with the same keys as `map` and values generated by
 * running each value of `map` thru `fn`.
 */
function mapValue(map, fn) {
  var result = Object.create(null);

  for (var _i2 = 0, _objectEntries2 = (0, _objectEntries3.default)(map); _i2 < _objectEntries2.length; _i2++) {
    var _ref2 = _objectEntries2[_i2];
    var _key = _ref2[0];
    var _value = _ref2[1];
    result[_key] = fn(_value, _key);
  }

  return result;
}

},{"../polyfills/objectEntries.js":69}],42:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = memoize3;

/**
 * Memoizes the provided three-argument function.
 */
function memoize3(fn) {
  var cache0;
  return function memoized(a1, a2, a3) {
    if (!cache0) {
      cache0 = new WeakMap();
    }

    var cache1 = cache0.get(a1);
    var cache2;

    if (cache1) {
      cache2 = cache1.get(a2);

      if (cache2) {
        var cachedValue = cache2.get(a3);

        if (cachedValue !== undefined) {
          return cachedValue;
        }
      }
    } else {
      cache1 = new WeakMap();
      cache0.set(a1, cache1);
    }

    if (!cache2) {
      cache2 = new WeakMap();
      cache1.set(a2, cache2);
    }

    var newValue = fn(a1, a2, a3);
    cache2.set(a3, newValue);
    return newValue;
  };
}

},{}],43:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = naturalCompare;

/**
 * Returns a number indicating whether a reference string comes before, or after,
 * or is the same as the given string in natural sort order.
 *
 * See: https://en.wikipedia.org/wiki/Natural_sort_order
 *
 */
function naturalCompare(aStr, bStr) {
  var aIdx = 0;
  var bIdx = 0;

  while (aIdx < aStr.length && bIdx < bStr.length) {
    var aChar = aStr.charCodeAt(aIdx);
    var bChar = bStr.charCodeAt(bIdx);

    if (isDigit(aChar) && isDigit(bChar)) {
      var aNum = 0;

      do {
        ++aIdx;
        aNum = aNum * 10 + aChar - DIGIT_0;
        aChar = aStr.charCodeAt(aIdx);
      } while (isDigit(aChar) && aNum > 0);

      var bNum = 0;

      do {
        ++bIdx;
        bNum = bNum * 10 + bChar - DIGIT_0;
        bChar = bStr.charCodeAt(bIdx);
      } while (isDigit(bChar) && bNum > 0);

      if (aNum < bNum) {
        return -1;
      }

      if (aNum > bNum) {
        return 1;
      }
    } else {
      if (aChar < bChar) {
        return -1;
      }

      if (aChar > bChar) {
        return 1;
      }

      ++aIdx;
      ++bIdx;
    }
  }

  return aStr.length - bStr.length;
}

var DIGIT_0 = 48;
var DIGIT_9 = 57;

function isDigit(code) {
  return !isNaN(code) && DIGIT_0 <= code && code <= DIGIT_9;
}

},{}],44:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;
// istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2317')
var nodejsCustomInspectSymbol = typeof Symbol === 'function' && typeof Symbol.for === 'function' ? Symbol.for('nodejs.util.inspect.custom') : undefined;
var _default = nodejsCustomInspectSymbol;
exports.default = _default;

},{}],45:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = printPathArray;

/**
 * Build a string describing the path.
 */
function printPathArray(path) {
  return path.map(function (key) {
    return typeof key === 'number' ? '[' + key.toString() + ']' : '.' + key;
  }).join('');
}

},{}],46:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = promiseForObject;

/**
 * This function transforms a JS object `ObjMap<Promise<T>>` into
 * a `Promise<ObjMap<T>>`
 *
 * This is akin to bluebird's `Promise.props`, but implemented only using
 * `Promise.all` so it will work with any implementation of ES6 promises.
 */
function promiseForObject(object) {
  var keys = Object.keys(object);
  var valuesAndPromises = keys.map(function (name) {
    return object[name];
  });
  return Promise.all(valuesAndPromises).then(function (values) {
    return values.reduce(function (resolvedObject, value, i) {
      resolvedObject[keys[i]] = value;
      return resolvedObject;
    }, Object.create(null));
  });
}

},{}],47:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = promiseReduce;

var _isPromise = _interopRequireDefault(require("./isPromise.js"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Similar to Array.prototype.reduce(), however the reducing callback may return
 * a Promise, in which case reduction will continue after each promise resolves.
 *
 * If the callback does not return a Promise, then this function will also not
 * return a Promise.
 */
function promiseReduce(values, callback, initialValue) {
  return values.reduce(function (previous, value) {
    return (0, _isPromise.default)(previous) ? previous.then(function (resolved) {
      return callback(resolved, value);
    }) : callback(previous, value);
  }, initialValue);
}

},{"./isPromise.js":38}],48:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = safeArrayFrom;

var _symbols = require("../polyfills/symbols.js");

function _typeof(obj) { "@babel/helpers - typeof"; if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

/**
 * Safer version of `Array.from` that return `null` if value isn't convertible to array.
 * Also protects against Array-like objects without items.
 *
 * @example
 *
 * safeArrayFrom([ 1, 2, 3 ]) // [1, 2, 3]
 * safeArrayFrom('ABC') // null
 * safeArrayFrom({ length: 1 }) // null
 * safeArrayFrom({ length: 1, 0: 'Alpha' }) // ['Alpha']
 * safeArrayFrom({ key: 'value' }) // null
 * safeArrayFrom(new Map()) // []
 *
 */
function safeArrayFrom(collection) {
  var mapFn = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : function (item) {
    return item;
  };

  if (collection == null || _typeof(collection) !== 'object') {
    return null;
  }

  if (Array.isArray(collection)) {
    return collection.map(mapFn);
  } // Is Iterable?


  var iteratorMethod = collection[_symbols.SYMBOL_ITERATOR];

  if (typeof iteratorMethod === 'function') {
    // $FlowFixMe[incompatible-use]
    var iterator = iteratorMethod.call(collection);
    var result = [];
    var step;

    for (var i = 0; !(step = iterator.next()).done; ++i) {
      result.push(mapFn(step.value, i));
    }

    return result;
  } // Is Array like?


  var length = collection.length;

  if (typeof length === 'number' && length >= 0 && length % 1 === 0) {
    var _result = [];

    for (var _i = 0; _i < length; ++_i) {
      if (!Object.prototype.hasOwnProperty.call(collection, _i)) {
        return null;
      }

      _result.push(mapFn(collection[String(_i)], _i));
    }

    return _result;
  }

  return null;
}

},{"../polyfills/symbols.js":71}],49:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = suggestionList;

var _naturalCompare = _interopRequireDefault(require("./naturalCompare.js"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Given an invalid input string and a list of valid options, returns a filtered
 * list of valid options sorted based on their similarity with the input.
 */
function suggestionList(input, options) {
  var optionsByDistance = Object.create(null);
  var lexicalDistance = new LexicalDistance(input);
  var threshold = Math.floor(input.length * 0.4) + 1;

  for (var _i2 = 0; _i2 < options.length; _i2++) {
    var option = options[_i2];
    var distance = lexicalDistance.measure(option, threshold);

    if (distance !== undefined) {
      optionsByDistance[option] = distance;
    }
  }

  return Object.keys(optionsByDistance).sort(function (a, b) {
    var distanceDiff = optionsByDistance[a] - optionsByDistance[b];
    return distanceDiff !== 0 ? distanceDiff : (0, _naturalCompare.default)(a, b);
  });
}
/**
 * Computes the lexical distance between strings A and B.
 *
 * The "distance" between two strings is given by counting the minimum number
 * of edits needed to transform string A into string B. An edit can be an
 * insertion, deletion, or substitution of a single character, or a swap of two
 * adjacent characters.
 *
 * Includes a custom alteration from Damerau-Levenshtein to treat case changes
 * as a single edit which helps identify mis-cased values with an edit distance
 * of 1.
 *
 * This distance can be useful for detecting typos in input or sorting
 */


var LexicalDistance = /*#__PURE__*/function () {
  function LexicalDistance(input) {
    this._input = input;
    this._inputLowerCase = input.toLowerCase();
    this._inputArray = stringToArray(this._inputLowerCase);
    this._rows = [new Array(input.length + 1).fill(0), new Array(input.length + 1).fill(0), new Array(input.length + 1).fill(0)];
  }

  var _proto = LexicalDistance.prototype;

  _proto.measure = function measure(option, threshold) {
    if (this._input === option) {
      return 0;
    }

    var optionLowerCase = option.toLowerCase(); // Any case change counts as a single edit

    if (this._inputLowerCase === optionLowerCase) {
      return 1;
    }

    var a = stringToArray(optionLowerCase);
    var b = this._inputArray;

    if (a.length < b.length) {
      var tmp = a;
      a = b;
      b = tmp;
    }

    var aLength = a.length;
    var bLength = b.length;

    if (aLength - bLength > threshold) {
      return undefined;
    }

    var rows = this._rows;

    for (var j = 0; j <= bLength; j++) {
      rows[0][j] = j;
    }

    for (var i = 1; i <= aLength; i++) {
      var upRow = rows[(i - 1) % 3];
      var currentRow = rows[i % 3];
      var smallestCell = currentRow[0] = i;

      for (var _j = 1; _j <= bLength; _j++) {
        var cost = a[i - 1] === b[_j - 1] ? 0 : 1;
        var currentCell = Math.min(upRow[_j] + 1, // delete
        currentRow[_j - 1] + 1, // insert
        upRow[_j - 1] + cost // substitute
        );

        if (i > 1 && _j > 1 && a[i - 1] === b[_j - 2] && a[i - 2] === b[_j - 1]) {
          // transposition
          var doubleDiagonalCell = rows[(i - 2) % 3][_j - 2];
          currentCell = Math.min(currentCell, doubleDiagonalCell + 1);
        }

        if (currentCell < smallestCell) {
          smallestCell = currentCell;
        }

        currentRow[_j] = currentCell;
      } // Early exit, since distance can't go smaller than smallest element of the previous row.


      if (smallestCell > threshold) {
        return undefined;
      }
    }

    var distance = rows[aLength % 3][bLength];
    return distance <= threshold ? distance : undefined;
  };

  return LexicalDistance;
}();

function stringToArray(str) {
  var strLength = str.length;
  var array = new Array(strLength);

  for (var i = 0; i < strLength; ++i) {
    array[i] = str.charCodeAt(i);
  }

  return array;
}

},{"./naturalCompare.js":43}],50:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = toObjMap;

var _objectEntries3 = _interopRequireDefault(require("../polyfills/objectEntries.js"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function toObjMap(obj) {
  /* eslint-enable no-redeclare */
  if (Object.getPrototypeOf(obj) === null) {
    return obj;
  }

  var map = Object.create(null);

  for (var _i2 = 0, _objectEntries2 = (0, _objectEntries3.default)(obj); _i2 < _objectEntries2.length; _i2++) {
    var _ref2 = _objectEntries2[_i2];
    var key = _ref2[0];
    var value = _ref2[1];
    map[key] = value;
  }

  return map;
}

},{"../polyfills/objectEntries.js":69}],51:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isNode = isNode;
exports.Token = exports.Location = void 0;

var _defineInspect = _interopRequireDefault(require("../jsutils/defineInspect.js"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Contains a range of UTF-8 character offsets and token references that
 * identify the region of the source from which the AST derived.
 */
var Location = /*#__PURE__*/function () {
  /**
   * The character offset at which this Node begins.
   */

  /**
   * The character offset at which this Node ends.
   */

  /**
   * The Token at which this Node begins.
   */

  /**
   * The Token at which this Node ends.
   */

  /**
   * The Source document the AST represents.
   */
  function Location(startToken, endToken, source) {
    this.start = startToken.start;
    this.end = endToken.end;
    this.startToken = startToken;
    this.endToken = endToken;
    this.source = source;
  }

  var _proto = Location.prototype;

  _proto.toJSON = function toJSON() {
    return {
      start: this.start,
      end: this.end
    };
  };

  return Location;
}(); // Print a simplified form when appearing in `inspect` and `util.inspect`.


exports.Location = Location;
(0, _defineInspect.default)(Location);
/**
 * Represents a range of characters represented by a lexical token
 * within a Source.
 */

var Token = /*#__PURE__*/function () {
  /**
   * The kind of Token.
   */

  /**
   * The character offset at which this Node begins.
   */

  /**
   * The character offset at which this Node ends.
   */

  /**
   * The 1-indexed line number on which this Token appears.
   */

  /**
   * The 1-indexed column number at which this Token begins.
   */

  /**
   * For non-punctuation tokens, represents the interpreted value of the token.
   */

  /**
   * Tokens exist as nodes in a double-linked-list amongst all tokens
   * including ignored tokens. <SOF> is always the first node and <EOF>
   * the last.
   */
  function Token(kind, start, end, line, column, prev, value) {
    this.kind = kind;
    this.start = start;
    this.end = end;
    this.line = line;
    this.column = column;
    this.value = value;
    this.prev = prev;
    this.next = null;
  }

  var _proto2 = Token.prototype;

  _proto2.toJSON = function toJSON() {
    return {
      kind: this.kind,
      value: this.value,
      line: this.line,
      column: this.column
    };
  };

  return Token;
}(); // Print a simplified form when appearing in `inspect` and `util.inspect`.


exports.Token = Token;
(0, _defineInspect.default)(Token);
/**
 * @internal
 */

function isNode(maybeNode) {
  return maybeNode != null && typeof maybeNode.kind === 'string';
}
/**
 * The list of all possible AST node types.
 */

},{"../jsutils/defineInspect.js":29}],52:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.dedentBlockStringValue = dedentBlockStringValue;
exports.getBlockStringIndentation = getBlockStringIndentation;
exports.printBlockString = printBlockString;

/**
 * Produces the value of a block string from its parsed raw value, similar to
 * CoffeeScript's block string, Python's docstring trim or Ruby's strip_heredoc.
 *
 * This implements the GraphQL spec's BlockStringValue() static algorithm.
 *
 * @internal
 */
function dedentBlockStringValue(rawString) {
  // Expand a block string's raw value into independent lines.
  var lines = rawString.split(/\r\n|[\n\r]/g); // Remove common indentation from all lines but first.

  var commonIndent = getBlockStringIndentation(rawString);

  if (commonIndent !== 0) {
    for (var i = 1; i < lines.length; i++) {
      lines[i] = lines[i].slice(commonIndent);
    }
  } // Remove leading and trailing blank lines.


  var startLine = 0;

  while (startLine < lines.length && isBlank(lines[startLine])) {
    ++startLine;
  }

  var endLine = lines.length;

  while (endLine > startLine && isBlank(lines[endLine - 1])) {
    --endLine;
  } // Return a string of the lines joined with U+000A.


  return lines.slice(startLine, endLine).join('\n');
}

function isBlank(str) {
  for (var i = 0; i < str.length; ++i) {
    if (str[i] !== ' ' && str[i] !== '\t') {
      return false;
    }
  }

  return true;
}
/**
 * @internal
 */


function getBlockStringIndentation(value) {
  var _commonIndent;

  var isFirstLine = true;
  var isEmptyLine = true;
  var indent = 0;
  var commonIndent = null;

  for (var i = 0; i < value.length; ++i) {
    switch (value.charCodeAt(i)) {
      case 13:
        //  \r
        if (value.charCodeAt(i + 1) === 10) {
          ++i; // skip \r\n as one symbol
        }

      // falls through

      case 10:
        //  \n
        isFirstLine = false;
        isEmptyLine = true;
        indent = 0;
        break;

      case 9: //   \t

      case 32:
        //  <space>
        ++indent;
        break;

      default:
        if (isEmptyLine && !isFirstLine && (commonIndent === null || indent < commonIndent)) {
          commonIndent = indent;
        }

        isEmptyLine = false;
    }
  }

  return (_commonIndent = commonIndent) !== null && _commonIndent !== void 0 ? _commonIndent : 0;
}
/**
 * Print a block string in the indented block form by adding a leading and
 * trailing blank line. However, if a block string starts with whitespace and is
 * a single-line, adding a leading blank line would strip that whitespace.
 *
 * @internal
 */


function printBlockString(value) {
  var indentation = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : '';
  var preferMultipleLines = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;
  var isSingleLine = value.indexOf('\n') === -1;
  var hasLeadingSpace = value[0] === ' ' || value[0] === '\t';
  var hasTrailingQuote = value[value.length - 1] === '"';
  var hasTrailingSlash = value[value.length - 1] === '\\';
  var printAsMultipleLines = !isSingleLine || hasTrailingQuote || hasTrailingSlash || preferMultipleLines;
  var result = ''; // Format a multi-line block quote to account for leading space.

  if (printAsMultipleLines && !(isSingleLine && hasLeadingSpace)) {
    result += '\n' + indentation;
  }

  result += indentation ? value.replace(/\n/g, '\n' + indentation) : value;

  if (printAsMultipleLines) {
    result += '\n';
  }

  return '"""' + result.replace(/"""/g, '\\"""') + '"""';
}

},{}],53:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.DirectiveLocation = void 0;

/**
 * The set of allowed directive location values.
 */
var DirectiveLocation = Object.freeze({
  // Request Definitions
  QUERY: 'QUERY',
  MUTATION: 'MUTATION',
  SUBSCRIPTION: 'SUBSCRIPTION',
  FIELD: 'FIELD',
  FRAGMENT_DEFINITION: 'FRAGMENT_DEFINITION',
  FRAGMENT_SPREAD: 'FRAGMENT_SPREAD',
  INLINE_FRAGMENT: 'INLINE_FRAGMENT',
  VARIABLE_DEFINITION: 'VARIABLE_DEFINITION',
  // Type System Definitions
  SCHEMA: 'SCHEMA',
  SCALAR: 'SCALAR',
  OBJECT: 'OBJECT',
  FIELD_DEFINITION: 'FIELD_DEFINITION',
  ARGUMENT_DEFINITION: 'ARGUMENT_DEFINITION',
  INTERFACE: 'INTERFACE',
  UNION: 'UNION',
  ENUM: 'ENUM',
  ENUM_VALUE: 'ENUM_VALUE',
  INPUT_OBJECT: 'INPUT_OBJECT',
  INPUT_FIELD_DEFINITION: 'INPUT_FIELD_DEFINITION'
});
/**
 * The enum type representing the directive location values.
 */

exports.DirectiveLocation = DirectiveLocation;

},{}],54:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "Source", {
  enumerable: true,
  get: function get() {
    return _source.Source;
  }
});
Object.defineProperty(exports, "getLocation", {
  enumerable: true,
  get: function get() {
    return _location.getLocation;
  }
});
Object.defineProperty(exports, "printLocation", {
  enumerable: true,
  get: function get() {
    return _printLocation.printLocation;
  }
});
Object.defineProperty(exports, "printSourceLocation", {
  enumerable: true,
  get: function get() {
    return _printLocation.printSourceLocation;
  }
});
Object.defineProperty(exports, "Kind", {
  enumerable: true,
  get: function get() {
    return _kinds.Kind;
  }
});
Object.defineProperty(exports, "TokenKind", {
  enumerable: true,
  get: function get() {
    return _tokenKind.TokenKind;
  }
});
Object.defineProperty(exports, "Lexer", {
  enumerable: true,
  get: function get() {
    return _lexer.Lexer;
  }
});
Object.defineProperty(exports, "parse", {
  enumerable: true,
  get: function get() {
    return _parser.parse;
  }
});
Object.defineProperty(exports, "parseValue", {
  enumerable: true,
  get: function get() {
    return _parser.parseValue;
  }
});
Object.defineProperty(exports, "parseType", {
  enumerable: true,
  get: function get() {
    return _parser.parseType;
  }
});
Object.defineProperty(exports, "print", {
  enumerable: true,
  get: function get() {
    return _printer.print;
  }
});
Object.defineProperty(exports, "visit", {
  enumerable: true,
  get: function get() {
    return _visitor.visit;
  }
});
Object.defineProperty(exports, "visitInParallel", {
  enumerable: true,
  get: function get() {
    return _visitor.visitInParallel;
  }
});
Object.defineProperty(exports, "getVisitFn", {
  enumerable: true,
  get: function get() {
    return _visitor.getVisitFn;
  }
});
Object.defineProperty(exports, "BREAK", {
  enumerable: true,
  get: function get() {
    return _visitor.BREAK;
  }
});
Object.defineProperty(exports, "Location", {
  enumerable: true,
  get: function get() {
    return _ast.Location;
  }
});
Object.defineProperty(exports, "Token", {
  enumerable: true,
  get: function get() {
    return _ast.Token;
  }
});
Object.defineProperty(exports, "isDefinitionNode", {
  enumerable: true,
  get: function get() {
    return _predicates.isDefinitionNode;
  }
});
Object.defineProperty(exports, "isExecutableDefinitionNode", {
  enumerable: true,
  get: function get() {
    return _predicates.isExecutableDefinitionNode;
  }
});
Object.defineProperty(exports, "isSelectionNode", {
  enumerable: true,
  get: function get() {
    return _predicates.isSelectionNode;
  }
});
Object.defineProperty(exports, "isValueNode", {
  enumerable: true,
  get: function get() {
    return _predicates.isValueNode;
  }
});
Object.defineProperty(exports, "isTypeNode", {
  enumerable: true,
  get: function get() {
    return _predicates.isTypeNode;
  }
});
Object.defineProperty(exports, "isTypeSystemDefinitionNode", {
  enumerable: true,
  get: function get() {
    return _predicates.isTypeSystemDefinitionNode;
  }
});
Object.defineProperty(exports, "isTypeDefinitionNode", {
  enumerable: true,
  get: function get() {
    return _predicates.isTypeDefinitionNode;
  }
});
Object.defineProperty(exports, "isTypeSystemExtensionNode", {
  enumerable: true,
  get: function get() {
    return _predicates.isTypeSystemExtensionNode;
  }
});
Object.defineProperty(exports, "isTypeExtensionNode", {
  enumerable: true,
  get: function get() {
    return _predicates.isTypeExtensionNode;
  }
});
Object.defineProperty(exports, "DirectiveLocation", {
  enumerable: true,
  get: function get() {
    return _directiveLocation.DirectiveLocation;
  }
});

var _source = require("./source.js");

var _location = require("./location.js");

var _printLocation = require("./printLocation.js");

var _kinds = require("./kinds.js");

var _tokenKind = require("./tokenKind.js");

var _lexer = require("./lexer.js");

var _parser = require("./parser.js");

var _printer = require("./printer.js");

var _visitor = require("./visitor.js");

var _ast = require("./ast.js");

var _predicates = require("./predicates.js");

var _directiveLocation = require("./directiveLocation.js");

},{"./ast.js":51,"./directiveLocation.js":53,"./kinds.js":55,"./lexer.js":56,"./location.js":57,"./parser.js":58,"./predicates.js":59,"./printLocation.js":60,"./printer.js":61,"./source.js":62,"./tokenKind.js":63,"./visitor.js":64}],55:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Kind = void 0;

/**
 * The set of allowed kind values for AST nodes.
 */
var Kind = Object.freeze({
  // Name
  NAME: 'Name',
  // Document
  DOCUMENT: 'Document',
  OPERATION_DEFINITION: 'OperationDefinition',
  VARIABLE_DEFINITION: 'VariableDefinition',
  SELECTION_SET: 'SelectionSet',
  FIELD: 'Field',
  ARGUMENT: 'Argument',
  // Fragments
  FRAGMENT_SPREAD: 'FragmentSpread',
  INLINE_FRAGMENT: 'InlineFragment',
  FRAGMENT_DEFINITION: 'FragmentDefinition',
  // Values
  VARIABLE: 'Variable',
  INT: 'IntValue',
  FLOAT: 'FloatValue',
  STRING: 'StringValue',
  BOOLEAN: 'BooleanValue',
  NULL: 'NullValue',
  ENUM: 'EnumValue',
  LIST: 'ListValue',
  OBJECT: 'ObjectValue',
  OBJECT_FIELD: 'ObjectField',
  // Directives
  DIRECTIVE: 'Directive',
  // Types
  NAMED_TYPE: 'NamedType',
  LIST_TYPE: 'ListType',
  NON_NULL_TYPE: 'NonNullType',
  // Type System Definitions
  SCHEMA_DEFINITION: 'SchemaDefinition',
  OPERATION_TYPE_DEFINITION: 'OperationTypeDefinition',
  // Type Definitions
  SCALAR_TYPE_DEFINITION: 'ScalarTypeDefinition',
  OBJECT_TYPE_DEFINITION: 'ObjectTypeDefinition',
  FIELD_DEFINITION: 'FieldDefinition',
  INPUT_VALUE_DEFINITION: 'InputValueDefinition',
  INTERFACE_TYPE_DEFINITION: 'InterfaceTypeDefinition',
  UNION_TYPE_DEFINITION: 'UnionTypeDefinition',
  ENUM_TYPE_DEFINITION: 'EnumTypeDefinition',
  ENUM_VALUE_DEFINITION: 'EnumValueDefinition',
  INPUT_OBJECT_TYPE_DEFINITION: 'InputObjectTypeDefinition',
  // Directive Definitions
  DIRECTIVE_DEFINITION: 'DirectiveDefinition',
  // Type System Extensions
  SCHEMA_EXTENSION: 'SchemaExtension',
  // Type Extensions
  SCALAR_TYPE_EXTENSION: 'ScalarTypeExtension',
  OBJECT_TYPE_EXTENSION: 'ObjectTypeExtension',
  INTERFACE_TYPE_EXTENSION: 'InterfaceTypeExtension',
  UNION_TYPE_EXTENSION: 'UnionTypeExtension',
  ENUM_TYPE_EXTENSION: 'EnumTypeExtension',
  INPUT_OBJECT_TYPE_EXTENSION: 'InputObjectTypeExtension'
});
/**
 * The enum type representing the possible kind values of AST nodes.
 */

exports.Kind = Kind;

},{}],56:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isPunctuatorTokenKind = isPunctuatorTokenKind;
exports.Lexer = void 0;

var _syntaxError = require("../error/syntaxError.js");

var _ast = require("./ast.js");

var _tokenKind = require("./tokenKind.js");

var _blockString = require("./blockString.js");

/**
 * Given a Source object, creates a Lexer for that source.
 * A Lexer is a stateful stream generator in that every time
 * it is advanced, it returns the next token in the Source. Assuming the
 * source lexes, the final Token emitted by the lexer will be of kind
 * EOF, after which the lexer will repeatedly return the same EOF token
 * whenever called.
 */
var Lexer = /*#__PURE__*/function () {
  /**
   * The previously focused non-ignored token.
   */

  /**
   * The currently focused non-ignored token.
   */

  /**
   * The (1-indexed) line containing the current token.
   */

  /**
   * The character offset at which the current line begins.
   */
  function Lexer(source) {
    var startOfFileToken = new _ast.Token(_tokenKind.TokenKind.SOF, 0, 0, 0, 0, null);
    this.source = source;
    this.lastToken = startOfFileToken;
    this.token = startOfFileToken;
    this.line = 1;
    this.lineStart = 0;
  }
  /**
   * Advances the token stream to the next non-ignored token.
   */


  var _proto = Lexer.prototype;

  _proto.advance = function advance() {
    this.lastToken = this.token;
    var token = this.token = this.lookahead();
    return token;
  }
  /**
   * Looks ahead and returns the next non-ignored token, but does not change
   * the state of Lexer.
   */
  ;

  _proto.lookahead = function lookahead() {
    var token = this.token;

    if (token.kind !== _tokenKind.TokenKind.EOF) {
      do {
        var _token$next;

        // Note: next is only mutable during parsing, so we cast to allow this.
        token = (_token$next = token.next) !== null && _token$next !== void 0 ? _token$next : token.next = readToken(this, token);
      } while (token.kind === _tokenKind.TokenKind.COMMENT);
    }

    return token;
  };

  return Lexer;
}();
/**
 * @internal
 */


exports.Lexer = Lexer;

function isPunctuatorTokenKind(kind) {
  return kind === _tokenKind.TokenKind.BANG || kind === _tokenKind.TokenKind.DOLLAR || kind === _tokenKind.TokenKind.AMP || kind === _tokenKind.TokenKind.PAREN_L || kind === _tokenKind.TokenKind.PAREN_R || kind === _tokenKind.TokenKind.SPREAD || kind === _tokenKind.TokenKind.COLON || kind === _tokenKind.TokenKind.EQUALS || kind === _tokenKind.TokenKind.AT || kind === _tokenKind.TokenKind.BRACKET_L || kind === _tokenKind.TokenKind.BRACKET_R || kind === _tokenKind.TokenKind.BRACE_L || kind === _tokenKind.TokenKind.PIPE || kind === _tokenKind.TokenKind.BRACE_R;
}

function printCharCode(code) {
  return (// NaN/undefined represents access beyond the end of the file.
    isNaN(code) ? _tokenKind.TokenKind.EOF : // Trust JSON for ASCII.
    code < 0x007f ? JSON.stringify(String.fromCharCode(code)) : // Otherwise print the escaped form.
    "\"\\u".concat(('00' + code.toString(16).toUpperCase()).slice(-4), "\"")
  );
}
/**
 * Gets the next token from the source starting at the given position.
 *
 * This skips over whitespace until it finds the next lexable token, then lexes
 * punctuators immediately or calls the appropriate helper function for more
 * complicated tokens.
 */


function readToken(lexer, prev) {
  var source = lexer.source;
  var body = source.body;
  var bodyLength = body.length;
  var pos = prev.end;

  while (pos < bodyLength) {
    var code = body.charCodeAt(pos);
    var _line = lexer.line;

    var _col = 1 + pos - lexer.lineStart; // SourceCharacter


    switch (code) {
      case 0xfeff: // <BOM>

      case 9: //   \t

      case 32: //  <space>

      case 44:
        //  ,
        ++pos;
        continue;

      case 10:
        //  \n
        ++pos;
        ++lexer.line;
        lexer.lineStart = pos;
        continue;

      case 13:
        //  \r
        if (body.charCodeAt(pos + 1) === 10) {
          pos += 2;
        } else {
          ++pos;
        }

        ++lexer.line;
        lexer.lineStart = pos;
        continue;

      case 33:
        //  !
        return new _ast.Token(_tokenKind.TokenKind.BANG, pos, pos + 1, _line, _col, prev);

      case 35:
        //  #
        return readComment(source, pos, _line, _col, prev);

      case 36:
        //  $
        return new _ast.Token(_tokenKind.TokenKind.DOLLAR, pos, pos + 1, _line, _col, prev);

      case 38:
        //  &
        return new _ast.Token(_tokenKind.TokenKind.AMP, pos, pos + 1, _line, _col, prev);

      case 40:
        //  (
        return new _ast.Token(_tokenKind.TokenKind.PAREN_L, pos, pos + 1, _line, _col, prev);

      case 41:
        //  )
        return new _ast.Token(_tokenKind.TokenKind.PAREN_R, pos, pos + 1, _line, _col, prev);

      case 46:
        //  .
        if (body.charCodeAt(pos + 1) === 46 && body.charCodeAt(pos + 2) === 46) {
          return new _ast.Token(_tokenKind.TokenKind.SPREAD, pos, pos + 3, _line, _col, prev);
        }

        break;

      case 58:
        //  :
        return new _ast.Token(_tokenKind.TokenKind.COLON, pos, pos + 1, _line, _col, prev);

      case 61:
        //  =
        return new _ast.Token(_tokenKind.TokenKind.EQUALS, pos, pos + 1, _line, _col, prev);

      case 64:
        //  @
        return new _ast.Token(_tokenKind.TokenKind.AT, pos, pos + 1, _line, _col, prev);

      case 91:
        //  [
        return new _ast.Token(_tokenKind.TokenKind.BRACKET_L, pos, pos + 1, _line, _col, prev);

      case 93:
        //  ]
        return new _ast.Token(_tokenKind.TokenKind.BRACKET_R, pos, pos + 1, _line, _col, prev);

      case 123:
        // {
        return new _ast.Token(_tokenKind.TokenKind.BRACE_L, pos, pos + 1, _line, _col, prev);

      case 124:
        // |
        return new _ast.Token(_tokenKind.TokenKind.PIPE, pos, pos + 1, _line, _col, prev);

      case 125:
        // }
        return new _ast.Token(_tokenKind.TokenKind.BRACE_R, pos, pos + 1, _line, _col, prev);

      case 34:
        //  "
        if (body.charCodeAt(pos + 1) === 34 && body.charCodeAt(pos + 2) === 34) {
          return readBlockString(source, pos, _line, _col, prev, lexer);
        }

        return readString(source, pos, _line, _col, prev);

      case 45: //  -

      case 48: //  0

      case 49: //  1

      case 50: //  2

      case 51: //  3

      case 52: //  4

      case 53: //  5

      case 54: //  6

      case 55: //  7

      case 56: //  8

      case 57:
        //  9
        return readNumber(source, pos, code, _line, _col, prev);

      case 65: //  A

      case 66: //  B

      case 67: //  C

      case 68: //  D

      case 69: //  E

      case 70: //  F

      case 71: //  G

      case 72: //  H

      case 73: //  I

      case 74: //  J

      case 75: //  K

      case 76: //  L

      case 77: //  M

      case 78: //  N

      case 79: //  O

      case 80: //  P

      case 81: //  Q

      case 82: //  R

      case 83: //  S

      case 84: //  T

      case 85: //  U

      case 86: //  V

      case 87: //  W

      case 88: //  X

      case 89: //  Y

      case 90: //  Z

      case 95: //  _

      case 97: //  a

      case 98: //  b

      case 99: //  c

      case 100: // d

      case 101: // e

      case 102: // f

      case 103: // g

      case 104: // h

      case 105: // i

      case 106: // j

      case 107: // k

      case 108: // l

      case 109: // m

      case 110: // n

      case 111: // o

      case 112: // p

      case 113: // q

      case 114: // r

      case 115: // s

      case 116: // t

      case 117: // u

      case 118: // v

      case 119: // w

      case 120: // x

      case 121: // y

      case 122:
        // z
        return readName(source, pos, _line, _col, prev);
    }

    throw (0, _syntaxError.syntaxError)(source, pos, unexpectedCharacterMessage(code));
  }

  var line = lexer.line;
  var col = 1 + pos - lexer.lineStart;
  return new _ast.Token(_tokenKind.TokenKind.EOF, bodyLength, bodyLength, line, col, prev);
}
/**
 * Report a message that an unexpected character was encountered.
 */


function unexpectedCharacterMessage(code) {
  if (code < 0x0020 && code !== 0x0009 && code !== 0x000a && code !== 0x000d) {
    return "Cannot contain the invalid character ".concat(printCharCode(code), ".");
  }

  if (code === 39) {
    // '
    return 'Unexpected single quote character (\'), did you mean to use a double quote (")?';
  }

  return "Cannot parse the unexpected character ".concat(printCharCode(code), ".");
}
/**
 * Reads a comment token from the source file.
 *
 * #[\u0009\u0020-\uFFFF]*
 */


function readComment(source, start, line, col, prev) {
  var body = source.body;
  var code;
  var position = start;

  do {
    code = body.charCodeAt(++position);
  } while (!isNaN(code) && ( // SourceCharacter but not LineTerminator
  code > 0x001f || code === 0x0009));

  return new _ast.Token(_tokenKind.TokenKind.COMMENT, start, position, line, col, prev, body.slice(start + 1, position));
}
/**
 * Reads a number token from the source file, either a float
 * or an int depending on whether a decimal point appears.
 *
 * Int:   -?(0|[1-9][0-9]*)
 * Float: -?(0|[1-9][0-9]*)(\.[0-9]+)?((E|e)(+|-)?[0-9]+)?
 */


function readNumber(source, start, firstCode, line, col, prev) {
  var body = source.body;
  var code = firstCode;
  var position = start;
  var isFloat = false;

  if (code === 45) {
    // -
    code = body.charCodeAt(++position);
  }

  if (code === 48) {
    // 0
    code = body.charCodeAt(++position);

    if (code >= 48 && code <= 57) {
      throw (0, _syntaxError.syntaxError)(source, position, "Invalid number, unexpected digit after 0: ".concat(printCharCode(code), "."));
    }
  } else {
    position = readDigits(source, position, code);
    code = body.charCodeAt(position);
  }

  if (code === 46) {
    // .
    isFloat = true;
    code = body.charCodeAt(++position);
    position = readDigits(source, position, code);
    code = body.charCodeAt(position);
  }

  if (code === 69 || code === 101) {
    // E e
    isFloat = true;
    code = body.charCodeAt(++position);

    if (code === 43 || code === 45) {
      // + -
      code = body.charCodeAt(++position);
    }

    position = readDigits(source, position, code);
    code = body.charCodeAt(position);
  } // Numbers cannot be followed by . or NameStart


  if (code === 46 || isNameStart(code)) {
    throw (0, _syntaxError.syntaxError)(source, position, "Invalid number, expected digit but got: ".concat(printCharCode(code), "."));
  }

  return new _ast.Token(isFloat ? _tokenKind.TokenKind.FLOAT : _tokenKind.TokenKind.INT, start, position, line, col, prev, body.slice(start, position));
}
/**
 * Returns the new position in the source after reading digits.
 */


function readDigits(source, start, firstCode) {
  var body = source.body;
  var position = start;
  var code = firstCode;

  if (code >= 48 && code <= 57) {
    // 0 - 9
    do {
      code = body.charCodeAt(++position);
    } while (code >= 48 && code <= 57); // 0 - 9


    return position;
  }

  throw (0, _syntaxError.syntaxError)(source, position, "Invalid number, expected digit but got: ".concat(printCharCode(code), "."));
}
/**
 * Reads a string token from the source file.
 *
 * "([^"\\\u000A\u000D]|(\\(u[0-9a-fA-F]{4}|["\\/bfnrt])))*"
 */


function readString(source, start, line, col, prev) {
  var body = source.body;
  var position = start + 1;
  var chunkStart = position;
  var code = 0;
  var value = '';

  while (position < body.length && !isNaN(code = body.charCodeAt(position)) && // not LineTerminator
  code !== 0x000a && code !== 0x000d) {
    // Closing Quote (")
    if (code === 34) {
      value += body.slice(chunkStart, position);
      return new _ast.Token(_tokenKind.TokenKind.STRING, start, position + 1, line, col, prev, value);
    } // SourceCharacter


    if (code < 0x0020 && code !== 0x0009) {
      throw (0, _syntaxError.syntaxError)(source, position, "Invalid character within String: ".concat(printCharCode(code), "."));
    }

    ++position;

    if (code === 92) {
      // \
      value += body.slice(chunkStart, position - 1);
      code = body.charCodeAt(position);

      switch (code) {
        case 34:
          value += '"';
          break;

        case 47:
          value += '/';
          break;

        case 92:
          value += '\\';
          break;

        case 98:
          value += '\b';
          break;

        case 102:
          value += '\f';
          break;

        case 110:
          value += '\n';
          break;

        case 114:
          value += '\r';
          break;

        case 116:
          value += '\t';
          break;

        case 117:
          {
            // uXXXX
            var charCode = uniCharCode(body.charCodeAt(position + 1), body.charCodeAt(position + 2), body.charCodeAt(position + 3), body.charCodeAt(position + 4));

            if (charCode < 0) {
              var invalidSequence = body.slice(position + 1, position + 5);
              throw (0, _syntaxError.syntaxError)(source, position, "Invalid character escape sequence: \\u".concat(invalidSequence, "."));
            }

            value += String.fromCharCode(charCode);
            position += 4;
            break;
          }

        default:
          throw (0, _syntaxError.syntaxError)(source, position, "Invalid character escape sequence: \\".concat(String.fromCharCode(code), "."));
      }

      ++position;
      chunkStart = position;
    }
  }

  throw (0, _syntaxError.syntaxError)(source, position, 'Unterminated string.');
}
/**
 * Reads a block string token from the source file.
 *
 * """("?"?(\\"""|\\(?!=""")|[^"\\]))*"""
 */


function readBlockString(source, start, line, col, prev, lexer) {
  var body = source.body;
  var position = start + 3;
  var chunkStart = position;
  var code = 0;
  var rawValue = '';

  while (position < body.length && !isNaN(code = body.charCodeAt(position))) {
    // Closing Triple-Quote (""")
    if (code === 34 && body.charCodeAt(position + 1) === 34 && body.charCodeAt(position + 2) === 34) {
      rawValue += body.slice(chunkStart, position);
      return new _ast.Token(_tokenKind.TokenKind.BLOCK_STRING, start, position + 3, line, col, prev, (0, _blockString.dedentBlockStringValue)(rawValue));
    } // SourceCharacter


    if (code < 0x0020 && code !== 0x0009 && code !== 0x000a && code !== 0x000d) {
      throw (0, _syntaxError.syntaxError)(source, position, "Invalid character within String: ".concat(printCharCode(code), "."));
    }

    if (code === 10) {
      // new line
      ++position;
      ++lexer.line;
      lexer.lineStart = position;
    } else if (code === 13) {
      // carriage return
      if (body.charCodeAt(position + 1) === 10) {
        position += 2;
      } else {
        ++position;
      }

      ++lexer.line;
      lexer.lineStart = position;
    } else if ( // Escape Triple-Quote (\""")
    code === 92 && body.charCodeAt(position + 1) === 34 && body.charCodeAt(position + 2) === 34 && body.charCodeAt(position + 3) === 34) {
      rawValue += body.slice(chunkStart, position) + '"""';
      position += 4;
      chunkStart = position;
    } else {
      ++position;
    }
  }

  throw (0, _syntaxError.syntaxError)(source, position, 'Unterminated string.');
}
/**
 * Converts four hexadecimal chars to the integer that the
 * string represents. For example, uniCharCode('0','0','0','f')
 * will return 15, and uniCharCode('0','0','f','f') returns 255.
 *
 * Returns a negative number on error, if a char was invalid.
 *
 * This is implemented by noting that char2hex() returns -1 on error,
 * which means the result of ORing the char2hex() will also be negative.
 */


function uniCharCode(a, b, c, d) {
  return char2hex(a) << 12 | char2hex(b) << 8 | char2hex(c) << 4 | char2hex(d);
}
/**
 * Converts a hex character to its integer value.
 * '0' becomes 0, '9' becomes 9
 * 'A' becomes 10, 'F' becomes 15
 * 'a' becomes 10, 'f' becomes 15
 *
 * Returns -1 on error.
 */


function char2hex(a) {
  return a >= 48 && a <= 57 ? a - 48 // 0-9
  : a >= 65 && a <= 70 ? a - 55 // A-F
  : a >= 97 && a <= 102 ? a - 87 // a-f
  : -1;
}
/**
 * Reads an alphanumeric + underscore name from the source.
 *
 * [_A-Za-z][_0-9A-Za-z]*
 */


function readName(source, start, line, col, prev) {
  var body = source.body;
  var bodyLength = body.length;
  var position = start + 1;
  var code = 0;

  while (position !== bodyLength && !isNaN(code = body.charCodeAt(position)) && (code === 95 || // _
  code >= 48 && code <= 57 || // 0-9
  code >= 65 && code <= 90 || // A-Z
  code >= 97 && code <= 122) // a-z
  ) {
    ++position;
  }

  return new _ast.Token(_tokenKind.TokenKind.NAME, start, position, line, col, prev, body.slice(start, position));
} // _ A-Z a-z


function isNameStart(code) {
  return code === 95 || code >= 65 && code <= 90 || code >= 97 && code <= 122;
}

},{"../error/syntaxError.js":22,"./ast.js":51,"./blockString.js":52,"./tokenKind.js":63}],57:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.getLocation = getLocation;

/**
 * Represents a location in a Source.
 */

/**
 * Takes a Source and a UTF-8 character offset, and returns the corresponding
 * line and column as a SourceLocation.
 */
function getLocation(source, position) {
  var lineRegexp = /\r\n|[\n\r]/g;
  var line = 1;
  var column = position + 1;
  var match;

  while ((match = lineRegexp.exec(source.body)) && match.index < position) {
    line += 1;
    column = position + 1 - (match.index + match[0].length);
  }

  return {
    line: line,
    column: column
  };
}

},{}],58:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.parse = parse;
exports.parseValue = parseValue;
exports.parseType = parseType;
exports.Parser = void 0;

var _syntaxError = require("../error/syntaxError.js");

var _kinds = require("./kinds.js");

var _ast = require("./ast.js");

var _tokenKind = require("./tokenKind.js");

var _source = require("./source.js");

var _directiveLocation = require("./directiveLocation.js");

var _lexer = require("./lexer.js");

/**
 * Given a GraphQL source, parses it into a Document.
 * Throws GraphQLError if a syntax error is encountered.
 */
function parse(source, options) {
  var parser = new Parser(source, options);
  return parser.parseDocument();
}
/**
 * Given a string containing a GraphQL value (ex. `[42]`), parse the AST for
 * that value.
 * Throws GraphQLError if a syntax error is encountered.
 *
 * This is useful within tools that operate upon GraphQL Values directly and
 * in isolation of complete GraphQL documents.
 *
 * Consider providing the results to the utility function: valueFromAST().
 */


function parseValue(source, options) {
  var parser = new Parser(source, options);
  parser.expectToken(_tokenKind.TokenKind.SOF);
  var value = parser.parseValueLiteral(false);
  parser.expectToken(_tokenKind.TokenKind.EOF);
  return value;
}
/**
 * Given a string containing a GraphQL Type (ex. `[Int!]`), parse the AST for
 * that type.
 * Throws GraphQLError if a syntax error is encountered.
 *
 * This is useful within tools that operate upon GraphQL Types directly and
 * in isolation of complete GraphQL documents.
 *
 * Consider providing the results to the utility function: typeFromAST().
 */


function parseType(source, options) {
  var parser = new Parser(source, options);
  parser.expectToken(_tokenKind.TokenKind.SOF);
  var type = parser.parseTypeReference();
  parser.expectToken(_tokenKind.TokenKind.EOF);
  return type;
}
/**
 * This class is exported only to assist people in implementing their own parsers
 * without duplicating too much code and should be used only as last resort for cases
 * such as experimental syntax or if certain features could not be contributed upstream.
 *
 * It is still part of the internal API and is versioned, so any changes to it are never
 * considered breaking changes. If you still need to support multiple versions of the
 * library, please use the `versionInfo` variable for version detection.
 *
 * @internal
 */


var Parser = /*#__PURE__*/function () {
  function Parser(source, options) {
    var sourceObj = (0, _source.isSource)(source) ? source : new _source.Source(source);
    this._lexer = new _lexer.Lexer(sourceObj);
    this._options = options;
  }
  /**
   * Converts a name lex token into a name parse node.
   */


  var _proto = Parser.prototype;

  _proto.parseName = function parseName() {
    var token = this.expectToken(_tokenKind.TokenKind.NAME);
    return {
      kind: _kinds.Kind.NAME,
      value: token.value,
      loc: this.loc(token)
    };
  } // Implements the parsing rules in the Document section.

  /**
   * Document : Definition+
   */
  ;

  _proto.parseDocument = function parseDocument() {
    var start = this._lexer.token;
    return {
      kind: _kinds.Kind.DOCUMENT,
      definitions: this.many(_tokenKind.TokenKind.SOF, this.parseDefinition, _tokenKind.TokenKind.EOF),
      loc: this.loc(start)
    };
  }
  /**
   * Definition :
   *   - ExecutableDefinition
   *   - TypeSystemDefinition
   *   - TypeSystemExtension
   *
   * ExecutableDefinition :
   *   - OperationDefinition
   *   - FragmentDefinition
   */
  ;

  _proto.parseDefinition = function parseDefinition() {
    if (this.peek(_tokenKind.TokenKind.NAME)) {
      switch (this._lexer.token.value) {
        case 'query':
        case 'mutation':
        case 'subscription':
          return this.parseOperationDefinition();

        case 'fragment':
          return this.parseFragmentDefinition();

        case 'schema':
        case 'scalar':
        case 'type':
        case 'interface':
        case 'union':
        case 'enum':
        case 'input':
        case 'directive':
          return this.parseTypeSystemDefinition();

        case 'extend':
          return this.parseTypeSystemExtension();
      }
    } else if (this.peek(_tokenKind.TokenKind.BRACE_L)) {
      return this.parseOperationDefinition();
    } else if (this.peekDescription()) {
      return this.parseTypeSystemDefinition();
    }

    throw this.unexpected();
  } // Implements the parsing rules in the Operations section.

  /**
   * OperationDefinition :
   *  - SelectionSet
   *  - OperationType Name? VariableDefinitions? Directives? SelectionSet
   */
  ;

  _proto.parseOperationDefinition = function parseOperationDefinition() {
    var start = this._lexer.token;

    if (this.peek(_tokenKind.TokenKind.BRACE_L)) {
      return {
        kind: _kinds.Kind.OPERATION_DEFINITION,
        operation: 'query',
        name: undefined,
        variableDefinitions: [],
        directives: [],
        selectionSet: this.parseSelectionSet(),
        loc: this.loc(start)
      };
    }

    var operation = this.parseOperationType();
    var name;

    if (this.peek(_tokenKind.TokenKind.NAME)) {
      name = this.parseName();
    }

    return {
      kind: _kinds.Kind.OPERATION_DEFINITION,
      operation: operation,
      name: name,
      variableDefinitions: this.parseVariableDefinitions(),
      directives: this.parseDirectives(false),
      selectionSet: this.parseSelectionSet(),
      loc: this.loc(start)
    };
  }
  /**
   * OperationType : one of query mutation subscription
   */
  ;

  _proto.parseOperationType = function parseOperationType() {
    var operationToken = this.expectToken(_tokenKind.TokenKind.NAME);

    switch (operationToken.value) {
      case 'query':
        return 'query';

      case 'mutation':
        return 'mutation';

      case 'subscription':
        return 'subscription';
    }

    throw this.unexpected(operationToken);
  }
  /**
   * VariableDefinitions : ( VariableDefinition+ )
   */
  ;

  _proto.parseVariableDefinitions = function parseVariableDefinitions() {
    return this.optionalMany(_tokenKind.TokenKind.PAREN_L, this.parseVariableDefinition, _tokenKind.TokenKind.PAREN_R);
  }
  /**
   * VariableDefinition : Variable : Type DefaultValue? Directives[Const]?
   */
  ;

  _proto.parseVariableDefinition = function parseVariableDefinition() {
    var start = this._lexer.token;
    return {
      kind: _kinds.Kind.VARIABLE_DEFINITION,
      variable: this.parseVariable(),
      type: (this.expectToken(_tokenKind.TokenKind.COLON), this.parseTypeReference()),
      defaultValue: this.expectOptionalToken(_tokenKind.TokenKind.EQUALS) ? this.parseValueLiteral(true) : undefined,
      directives: this.parseDirectives(true),
      loc: this.loc(start)
    };
  }
  /**
   * Variable : $ Name
   */
  ;

  _proto.parseVariable = function parseVariable() {
    var start = this._lexer.token;
    this.expectToken(_tokenKind.TokenKind.DOLLAR);
    return {
      kind: _kinds.Kind.VARIABLE,
      name: this.parseName(),
      loc: this.loc(start)
    };
  }
  /**
   * SelectionSet : { Selection+ }
   */
  ;

  _proto.parseSelectionSet = function parseSelectionSet() {
    var start = this._lexer.token;
    return {
      kind: _kinds.Kind.SELECTION_SET,
      selections: this.many(_tokenKind.TokenKind.BRACE_L, this.parseSelection, _tokenKind.TokenKind.BRACE_R),
      loc: this.loc(start)
    };
  }
  /**
   * Selection :
   *   - Field
   *   - FragmentSpread
   *   - InlineFragment
   */
  ;

  _proto.parseSelection = function parseSelection() {
    return this.peek(_tokenKind.TokenKind.SPREAD) ? this.parseFragment() : this.parseField();
  }
  /**
   * Field : Alias? Name Arguments? Directives? SelectionSet?
   *
   * Alias : Name :
   */
  ;

  _proto.parseField = function parseField() {
    var start = this._lexer.token;
    var nameOrAlias = this.parseName();
    var alias;
    var name;

    if (this.expectOptionalToken(_tokenKind.TokenKind.COLON)) {
      alias = nameOrAlias;
      name = this.parseName();
    } else {
      name = nameOrAlias;
    }

    return {
      kind: _kinds.Kind.FIELD,
      alias: alias,
      name: name,
      arguments: this.parseArguments(false),
      directives: this.parseDirectives(false),
      selectionSet: this.peek(_tokenKind.TokenKind.BRACE_L) ? this.parseSelectionSet() : undefined,
      loc: this.loc(start)
    };
  }
  /**
   * Arguments[Const] : ( Argument[?Const]+ )
   */
  ;

  _proto.parseArguments = function parseArguments(isConst) {
    var item = isConst ? this.parseConstArgument : this.parseArgument;
    return this.optionalMany(_tokenKind.TokenKind.PAREN_L, item, _tokenKind.TokenKind.PAREN_R);
  }
  /**
   * Argument[Const] : Name : Value[?Const]
   */
  ;

  _proto.parseArgument = function parseArgument() {
    var start = this._lexer.token;
    var name = this.parseName();
    this.expectToken(_tokenKind.TokenKind.COLON);
    return {
      kind: _kinds.Kind.ARGUMENT,
      name: name,
      value: this.parseValueLiteral(false),
      loc: this.loc(start)
    };
  };

  _proto.parseConstArgument = function parseConstArgument() {
    var start = this._lexer.token;
    return {
      kind: _kinds.Kind.ARGUMENT,
      name: this.parseName(),
      value: (this.expectToken(_tokenKind.TokenKind.COLON), this.parseValueLiteral(true)),
      loc: this.loc(start)
    };
  } // Implements the parsing rules in the Fragments section.

  /**
   * Corresponds to both FragmentSpread and InlineFragment in the spec.
   *
   * FragmentSpread : ... FragmentName Directives?
   *
   * InlineFragment : ... TypeCondition? Directives? SelectionSet
   */
  ;

  _proto.parseFragment = function parseFragment() {
    var start = this._lexer.token;
    this.expectToken(_tokenKind.TokenKind.SPREAD);
    var hasTypeCondition = this.expectOptionalKeyword('on');

    if (!hasTypeCondition && this.peek(_tokenKind.TokenKind.NAME)) {
      return {
        kind: _kinds.Kind.FRAGMENT_SPREAD,
        name: this.parseFragmentName(),
        directives: this.parseDirectives(false),
        loc: this.loc(start)
      };
    }

    return {
      kind: _kinds.Kind.INLINE_FRAGMENT,
      typeCondition: hasTypeCondition ? this.parseNamedType() : undefined,
      directives: this.parseDirectives(false),
      selectionSet: this.parseSelectionSet(),
      loc: this.loc(start)
    };
  }
  /**
   * FragmentDefinition :
   *   - fragment FragmentName on TypeCondition Directives? SelectionSet
   *
   * TypeCondition : NamedType
   */
  ;

  _proto.parseFragmentDefinition = function parseFragmentDefinition() {
    var _this$_options;

    var start = this._lexer.token;
    this.expectKeyword('fragment'); // Experimental support for defining variables within fragments changes
    // the grammar of FragmentDefinition:
    //   - fragment FragmentName VariableDefinitions? on TypeCondition Directives? SelectionSet

    if (((_this$_options = this._options) === null || _this$_options === void 0 ? void 0 : _this$_options.experimentalFragmentVariables) === true) {
      return {
        kind: _kinds.Kind.FRAGMENT_DEFINITION,
        name: this.parseFragmentName(),
        variableDefinitions: this.parseVariableDefinitions(),
        typeCondition: (this.expectKeyword('on'), this.parseNamedType()),
        directives: this.parseDirectives(false),
        selectionSet: this.parseSelectionSet(),
        loc: this.loc(start)
      };
    }

    return {
      kind: _kinds.Kind.FRAGMENT_DEFINITION,
      name: this.parseFragmentName(),
      typeCondition: (this.expectKeyword('on'), this.parseNamedType()),
      directives: this.parseDirectives(false),
      selectionSet: this.parseSelectionSet(),
      loc: this.loc(start)
    };
  }
  /**
   * FragmentName : Name but not `on`
   */
  ;

  _proto.parseFragmentName = function parseFragmentName() {
    if (this._lexer.token.value === 'on') {
      throw this.unexpected();
    }

    return this.parseName();
  } // Implements the parsing rules in the Values section.

  /**
   * Value[Const] :
   *   - [~Const] Variable
   *   - IntValue
   *   - FloatValue
   *   - StringValue
   *   - BooleanValue
   *   - NullValue
   *   - EnumValue
   *   - ListValue[?Const]
   *   - ObjectValue[?Const]
   *
   * BooleanValue : one of `true` `false`
   *
   * NullValue : `null`
   *
   * EnumValue : Name but not `true`, `false` or `null`
   */
  ;

  _proto.parseValueLiteral = function parseValueLiteral(isConst) {
    var token = this._lexer.token;

    switch (token.kind) {
      case _tokenKind.TokenKind.BRACKET_L:
        return this.parseList(isConst);

      case _tokenKind.TokenKind.BRACE_L:
        return this.parseObject(isConst);

      case _tokenKind.TokenKind.INT:
        this._lexer.advance();

        return {
          kind: _kinds.Kind.INT,
          value: token.value,
          loc: this.loc(token)
        };

      case _tokenKind.TokenKind.FLOAT:
        this._lexer.advance();

        return {
          kind: _kinds.Kind.FLOAT,
          value: token.value,
          loc: this.loc(token)
        };

      case _tokenKind.TokenKind.STRING:
      case _tokenKind.TokenKind.BLOCK_STRING:
        return this.parseStringLiteral();

      case _tokenKind.TokenKind.NAME:
        this._lexer.advance();

        switch (token.value) {
          case 'true':
            return {
              kind: _kinds.Kind.BOOLEAN,
              value: true,
              loc: this.loc(token)
            };

          case 'false':
            return {
              kind: _kinds.Kind.BOOLEAN,
              value: false,
              loc: this.loc(token)
            };

          case 'null':
            return {
              kind: _kinds.Kind.NULL,
              loc: this.loc(token)
            };

          default:
            return {
              kind: _kinds.Kind.ENUM,
              value: token.value,
              loc: this.loc(token)
            };
        }

      case _tokenKind.TokenKind.DOLLAR:
        if (!isConst) {
          return this.parseVariable();
        }

        break;
    }

    throw this.unexpected();
  };

  _proto.parseStringLiteral = function parseStringLiteral() {
    var token = this._lexer.token;

    this._lexer.advance();

    return {
      kind: _kinds.Kind.STRING,
      value: token.value,
      block: token.kind === _tokenKind.TokenKind.BLOCK_STRING,
      loc: this.loc(token)
    };
  }
  /**
   * ListValue[Const] :
   *   - [ ]
   *   - [ Value[?Const]+ ]
   */
  ;

  _proto.parseList = function parseList(isConst) {
    var _this = this;

    var start = this._lexer.token;

    var item = function item() {
      return _this.parseValueLiteral(isConst);
    };

    return {
      kind: _kinds.Kind.LIST,
      values: this.any(_tokenKind.TokenKind.BRACKET_L, item, _tokenKind.TokenKind.BRACKET_R),
      loc: this.loc(start)
    };
  }
  /**
   * ObjectValue[Const] :
   *   - { }
   *   - { ObjectField[?Const]+ }
   */
  ;

  _proto.parseObject = function parseObject(isConst) {
    var _this2 = this;

    var start = this._lexer.token;

    var item = function item() {
      return _this2.parseObjectField(isConst);
    };

    return {
      kind: _kinds.Kind.OBJECT,
      fields: this.any(_tokenKind.TokenKind.BRACE_L, item, _tokenKind.TokenKind.BRACE_R),
      loc: this.loc(start)
    };
  }
  /**
   * ObjectField[Const] : Name : Value[?Const]
   */
  ;

  _proto.parseObjectField = function parseObjectField(isConst) {
    var start = this._lexer.token;
    var name = this.parseName();
    this.expectToken(_tokenKind.TokenKind.COLON);
    return {
      kind: _kinds.Kind.OBJECT_FIELD,
      name: name,
      value: this.parseValueLiteral(isConst),
      loc: this.loc(start)
    };
  } // Implements the parsing rules in the Directives section.

  /**
   * Directives[Const] : Directive[?Const]+
   */
  ;

  _proto.parseDirectives = function parseDirectives(isConst) {
    var directives = [];

    while (this.peek(_tokenKind.TokenKind.AT)) {
      directives.push(this.parseDirective(isConst));
    }

    return directives;
  }
  /**
   * Directive[Const] : @ Name Arguments[?Const]?
   */
  ;

  _proto.parseDirective = function parseDirective(isConst) {
    var start = this._lexer.token;
    this.expectToken(_tokenKind.TokenKind.AT);
    return {
      kind: _kinds.Kind.DIRECTIVE,
      name: this.parseName(),
      arguments: this.parseArguments(isConst),
      loc: this.loc(start)
    };
  } // Implements the parsing rules in the Types section.

  /**
   * Type :
   *   - NamedType
   *   - ListType
   *   - NonNullType
   */
  ;

  _proto.parseTypeReference = function parseTypeReference() {
    var start = this._lexer.token;
    var type;

    if (this.expectOptionalToken(_tokenKind.TokenKind.BRACKET_L)) {
      type = this.parseTypeReference();
      this.expectToken(_tokenKind.TokenKind.BRACKET_R);
      type = {
        kind: _kinds.Kind.LIST_TYPE,
        type: type,
        loc: this.loc(start)
      };
    } else {
      type = this.parseNamedType();
    }

    if (this.expectOptionalToken(_tokenKind.TokenKind.BANG)) {
      return {
        kind: _kinds.Kind.NON_NULL_TYPE,
        type: type,
        loc: this.loc(start)
      };
    }

    return type;
  }
  /**
   * NamedType : Name
   */
  ;

  _proto.parseNamedType = function parseNamedType() {
    var start = this._lexer.token;
    return {
      kind: _kinds.Kind.NAMED_TYPE,
      name: this.parseName(),
      loc: this.loc(start)
    };
  } // Implements the parsing rules in the Type Definition section.

  /**
   * TypeSystemDefinition :
   *   - SchemaDefinition
   *   - TypeDefinition
   *   - DirectiveDefinition
   *
   * TypeDefinition :
   *   - ScalarTypeDefinition
   *   - ObjectTypeDefinition
   *   - InterfaceTypeDefinition
   *   - UnionTypeDefinition
   *   - EnumTypeDefinition
   *   - InputObjectTypeDefinition
   */
  ;

  _proto.parseTypeSystemDefinition = function parseTypeSystemDefinition() {
    // Many definitions begin with a description and require a lookahead.
    var keywordToken = this.peekDescription() ? this._lexer.lookahead() : this._lexer.token;

    if (keywordToken.kind === _tokenKind.TokenKind.NAME) {
      switch (keywordToken.value) {
        case 'schema':
          return this.parseSchemaDefinition();

        case 'scalar':
          return this.parseScalarTypeDefinition();

        case 'type':
          return this.parseObjectTypeDefinition();

        case 'interface':
          return this.parseInterfaceTypeDefinition();

        case 'union':
          return this.parseUnionTypeDefinition();

        case 'enum':
          return this.parseEnumTypeDefinition();

        case 'input':
          return this.parseInputObjectTypeDefinition();

        case 'directive':
          return this.parseDirectiveDefinition();
      }
    }

    throw this.unexpected(keywordToken);
  };

  _proto.peekDescription = function peekDescription() {
    return this.peek(_tokenKind.TokenKind.STRING) || this.peek(_tokenKind.TokenKind.BLOCK_STRING);
  }
  /**
   * Description : StringValue
   */
  ;

  _proto.parseDescription = function parseDescription() {
    if (this.peekDescription()) {
      return this.parseStringLiteral();
    }
  }
  /**
   * SchemaDefinition : Description? schema Directives[Const]? { OperationTypeDefinition+ }
   */
  ;

  _proto.parseSchemaDefinition = function parseSchemaDefinition() {
    var start = this._lexer.token;
    var description = this.parseDescription();
    this.expectKeyword('schema');
    var directives = this.parseDirectives(true);
    var operationTypes = this.many(_tokenKind.TokenKind.BRACE_L, this.parseOperationTypeDefinition, _tokenKind.TokenKind.BRACE_R);
    return {
      kind: _kinds.Kind.SCHEMA_DEFINITION,
      description: description,
      directives: directives,
      operationTypes: operationTypes,
      loc: this.loc(start)
    };
  }
  /**
   * OperationTypeDefinition : OperationType : NamedType
   */
  ;

  _proto.parseOperationTypeDefinition = function parseOperationTypeDefinition() {
    var start = this._lexer.token;
    var operation = this.parseOperationType();
    this.expectToken(_tokenKind.TokenKind.COLON);
    var type = this.parseNamedType();
    return {
      kind: _kinds.Kind.OPERATION_TYPE_DEFINITION,
      operation: operation,
      type: type,
      loc: this.loc(start)
    };
  }
  /**
   * ScalarTypeDefinition : Description? scalar Name Directives[Const]?
   */
  ;

  _proto.parseScalarTypeDefinition = function parseScalarTypeDefinition() {
    var start = this._lexer.token;
    var description = this.parseDescription();
    this.expectKeyword('scalar');
    var name = this.parseName();
    var directives = this.parseDirectives(true);
    return {
      kind: _kinds.Kind.SCALAR_TYPE_DEFINITION,
      description: description,
      name: name,
      directives: directives,
      loc: this.loc(start)
    };
  }
  /**
   * ObjectTypeDefinition :
   *   Description?
   *   type Name ImplementsInterfaces? Directives[Const]? FieldsDefinition?
   */
  ;

  _proto.parseObjectTypeDefinition = function parseObjectTypeDefinition() {
    var start = this._lexer.token;
    var description = this.parseDescription();
    this.expectKeyword('type');
    var name = this.parseName();
    var interfaces = this.parseImplementsInterfaces();
    var directives = this.parseDirectives(true);
    var fields = this.parseFieldsDefinition();
    return {
      kind: _kinds.Kind.OBJECT_TYPE_DEFINITION,
      description: description,
      name: name,
      interfaces: interfaces,
      directives: directives,
      fields: fields,
      loc: this.loc(start)
    };
  }
  /**
   * ImplementsInterfaces :
   *   - implements `&`? NamedType
   *   - ImplementsInterfaces & NamedType
   */
  ;

  _proto.parseImplementsInterfaces = function parseImplementsInterfaces() {
    var _this$_options2;

    if (!this.expectOptionalKeyword('implements')) {
      return [];
    }

    if (((_this$_options2 = this._options) === null || _this$_options2 === void 0 ? void 0 : _this$_options2.allowLegacySDLImplementsInterfaces) === true) {
      var types = []; // Optional leading ampersand

      this.expectOptionalToken(_tokenKind.TokenKind.AMP);

      do {
        types.push(this.parseNamedType());
      } while (this.expectOptionalToken(_tokenKind.TokenKind.AMP) || this.peek(_tokenKind.TokenKind.NAME));

      return types;
    }

    return this.delimitedMany(_tokenKind.TokenKind.AMP, this.parseNamedType);
  }
  /**
   * FieldsDefinition : { FieldDefinition+ }
   */
  ;

  _proto.parseFieldsDefinition = function parseFieldsDefinition() {
    var _this$_options3;

    // Legacy support for the SDL?
    if (((_this$_options3 = this._options) === null || _this$_options3 === void 0 ? void 0 : _this$_options3.allowLegacySDLEmptyFields) === true && this.peek(_tokenKind.TokenKind.BRACE_L) && this._lexer.lookahead().kind === _tokenKind.TokenKind.BRACE_R) {
      this._lexer.advance();

      this._lexer.advance();

      return [];
    }

    return this.optionalMany(_tokenKind.TokenKind.BRACE_L, this.parseFieldDefinition, _tokenKind.TokenKind.BRACE_R);
  }
  /**
   * FieldDefinition :
   *   - Description? Name ArgumentsDefinition? : Type Directives[Const]?
   */
  ;

  _proto.parseFieldDefinition = function parseFieldDefinition() {
    var start = this._lexer.token;
    var description = this.parseDescription();
    var name = this.parseName();
    var args = this.parseArgumentDefs();
    this.expectToken(_tokenKind.TokenKind.COLON);
    var type = this.parseTypeReference();
    var directives = this.parseDirectives(true);
    return {
      kind: _kinds.Kind.FIELD_DEFINITION,
      description: description,
      name: name,
      arguments: args,
      type: type,
      directives: directives,
      loc: this.loc(start)
    };
  }
  /**
   * ArgumentsDefinition : ( InputValueDefinition+ )
   */
  ;

  _proto.parseArgumentDefs = function parseArgumentDefs() {
    return this.optionalMany(_tokenKind.TokenKind.PAREN_L, this.parseInputValueDef, _tokenKind.TokenKind.PAREN_R);
  }
  /**
   * InputValueDefinition :
   *   - Description? Name : Type DefaultValue? Directives[Const]?
   */
  ;

  _proto.parseInputValueDef = function parseInputValueDef() {
    var start = this._lexer.token;
    var description = this.parseDescription();
    var name = this.parseName();
    this.expectToken(_tokenKind.TokenKind.COLON);
    var type = this.parseTypeReference();
    var defaultValue;

    if (this.expectOptionalToken(_tokenKind.TokenKind.EQUALS)) {
      defaultValue = this.parseValueLiteral(true);
    }

    var directives = this.parseDirectives(true);
    return {
      kind: _kinds.Kind.INPUT_VALUE_DEFINITION,
      description: description,
      name: name,
      type: type,
      defaultValue: defaultValue,
      directives: directives,
      loc: this.loc(start)
    };
  }
  /**
   * InterfaceTypeDefinition :
   *   - Description? interface Name Directives[Const]? FieldsDefinition?
   */
  ;

  _proto.parseInterfaceTypeDefinition = function parseInterfaceTypeDefinition() {
    var start = this._lexer.token;
    var description = this.parseDescription();
    this.expectKeyword('interface');
    var name = this.parseName();
    var interfaces = this.parseImplementsInterfaces();
    var directives = this.parseDirectives(true);
    var fields = this.parseFieldsDefinition();
    return {
      kind: _kinds.Kind.INTERFACE_TYPE_DEFINITION,
      description: description,
      name: name,
      interfaces: interfaces,
      directives: directives,
      fields: fields,
      loc: this.loc(start)
    };
  }
  /**
   * UnionTypeDefinition :
   *   - Description? union Name Directives[Const]? UnionMemberTypes?
   */
  ;

  _proto.parseUnionTypeDefinition = function parseUnionTypeDefinition() {
    var start = this._lexer.token;
    var description = this.parseDescription();
    this.expectKeyword('union');
    var name = this.parseName();
    var directives = this.parseDirectives(true);
    var types = this.parseUnionMemberTypes();
    return {
      kind: _kinds.Kind.UNION_TYPE_DEFINITION,
      description: description,
      name: name,
      directives: directives,
      types: types,
      loc: this.loc(start)
    };
  }
  /**
   * UnionMemberTypes :
   *   - = `|`? NamedType
   *   - UnionMemberTypes | NamedType
   */
  ;

  _proto.parseUnionMemberTypes = function parseUnionMemberTypes() {
    return this.expectOptionalToken(_tokenKind.TokenKind.EQUALS) ? this.delimitedMany(_tokenKind.TokenKind.PIPE, this.parseNamedType) : [];
  }
  /**
   * EnumTypeDefinition :
   *   - Description? enum Name Directives[Const]? EnumValuesDefinition?
   */
  ;

  _proto.parseEnumTypeDefinition = function parseEnumTypeDefinition() {
    var start = this._lexer.token;
    var description = this.parseDescription();
    this.expectKeyword('enum');
    var name = this.parseName();
    var directives = this.parseDirectives(true);
    var values = this.parseEnumValuesDefinition();
    return {
      kind: _kinds.Kind.ENUM_TYPE_DEFINITION,
      description: description,
      name: name,
      directives: directives,
      values: values,
      loc: this.loc(start)
    };
  }
  /**
   * EnumValuesDefinition : { EnumValueDefinition+ }
   */
  ;

  _proto.parseEnumValuesDefinition = function parseEnumValuesDefinition() {
    return this.optionalMany(_tokenKind.TokenKind.BRACE_L, this.parseEnumValueDefinition, _tokenKind.TokenKind.BRACE_R);
  }
  /**
   * EnumValueDefinition : Description? EnumValue Directives[Const]?
   *
   * EnumValue : Name
   */
  ;

  _proto.parseEnumValueDefinition = function parseEnumValueDefinition() {
    var start = this._lexer.token;
    var description = this.parseDescription();
    var name = this.parseName();
    var directives = this.parseDirectives(true);
    return {
      kind: _kinds.Kind.ENUM_VALUE_DEFINITION,
      description: description,
      name: name,
      directives: directives,
      loc: this.loc(start)
    };
  }
  /**
   * InputObjectTypeDefinition :
   *   - Description? input Name Directives[Const]? InputFieldsDefinition?
   */
  ;

  _proto.parseInputObjectTypeDefinition = function parseInputObjectTypeDefinition() {
    var start = this._lexer.token;
    var description = this.parseDescription();
    this.expectKeyword('input');
    var name = this.parseName();
    var directives = this.parseDirectives(true);
    var fields = this.parseInputFieldsDefinition();
    return {
      kind: _kinds.Kind.INPUT_OBJECT_TYPE_DEFINITION,
      description: description,
      name: name,
      directives: directives,
      fields: fields,
      loc: this.loc(start)
    };
  }
  /**
   * InputFieldsDefinition : { InputValueDefinition+ }
   */
  ;

  _proto.parseInputFieldsDefinition = function parseInputFieldsDefinition() {
    return this.optionalMany(_tokenKind.TokenKind.BRACE_L, this.parseInputValueDef, _tokenKind.TokenKind.BRACE_R);
  }
  /**
   * TypeSystemExtension :
   *   - SchemaExtension
   *   - TypeExtension
   *
   * TypeExtension :
   *   - ScalarTypeExtension
   *   - ObjectTypeExtension
   *   - InterfaceTypeExtension
   *   - UnionTypeExtension
   *   - EnumTypeExtension
   *   - InputObjectTypeDefinition
   */
  ;

  _proto.parseTypeSystemExtension = function parseTypeSystemExtension() {
    var keywordToken = this._lexer.lookahead();

    if (keywordToken.kind === _tokenKind.TokenKind.NAME) {
      switch (keywordToken.value) {
        case 'schema':
          return this.parseSchemaExtension();

        case 'scalar':
          return this.parseScalarTypeExtension();

        case 'type':
          return this.parseObjectTypeExtension();

        case 'interface':
          return this.parseInterfaceTypeExtension();

        case 'union':
          return this.parseUnionTypeExtension();

        case 'enum':
          return this.parseEnumTypeExtension();

        case 'input':
          return this.parseInputObjectTypeExtension();
      }
    }

    throw this.unexpected(keywordToken);
  }
  /**
   * SchemaExtension :
   *  - extend schema Directives[Const]? { OperationTypeDefinition+ }
   *  - extend schema Directives[Const]
   */
  ;

  _proto.parseSchemaExtension = function parseSchemaExtension() {
    var start = this._lexer.token;
    this.expectKeyword('extend');
    this.expectKeyword('schema');
    var directives = this.parseDirectives(true);
    var operationTypes = this.optionalMany(_tokenKind.TokenKind.BRACE_L, this.parseOperationTypeDefinition, _tokenKind.TokenKind.BRACE_R);

    if (directives.length === 0 && operationTypes.length === 0) {
      throw this.unexpected();
    }

    return {
      kind: _kinds.Kind.SCHEMA_EXTENSION,
      directives: directives,
      operationTypes: operationTypes,
      loc: this.loc(start)
    };
  }
  /**
   * ScalarTypeExtension :
   *   - extend scalar Name Directives[Const]
   */
  ;

  _proto.parseScalarTypeExtension = function parseScalarTypeExtension() {
    var start = this._lexer.token;
    this.expectKeyword('extend');
    this.expectKeyword('scalar');
    var name = this.parseName();
    var directives = this.parseDirectives(true);

    if (directives.length === 0) {
      throw this.unexpected();
    }

    return {
      kind: _kinds.Kind.SCALAR_TYPE_EXTENSION,
      name: name,
      directives: directives,
      loc: this.loc(start)
    };
  }
  /**
   * ObjectTypeExtension :
   *  - extend type Name ImplementsInterfaces? Directives[Const]? FieldsDefinition
   *  - extend type Name ImplementsInterfaces? Directives[Const]
   *  - extend type Name ImplementsInterfaces
   */
  ;

  _proto.parseObjectTypeExtension = function parseObjectTypeExtension() {
    var start = this._lexer.token;
    this.expectKeyword('extend');
    this.expectKeyword('type');
    var name = this.parseName();
    var interfaces = this.parseImplementsInterfaces();
    var directives = this.parseDirectives(true);
    var fields = this.parseFieldsDefinition();

    if (interfaces.length === 0 && directives.length === 0 && fields.length === 0) {
      throw this.unexpected();
    }

    return {
      kind: _kinds.Kind.OBJECT_TYPE_EXTENSION,
      name: name,
      interfaces: interfaces,
      directives: directives,
      fields: fields,
      loc: this.loc(start)
    };
  }
  /**
   * InterfaceTypeExtension :
   *  - extend interface Name ImplementsInterfaces? Directives[Const]? FieldsDefinition
   *  - extend interface Name ImplementsInterfaces? Directives[Const]
   *  - extend interface Name ImplementsInterfaces
   */
  ;

  _proto.parseInterfaceTypeExtension = function parseInterfaceTypeExtension() {
    var start = this._lexer.token;
    this.expectKeyword('extend');
    this.expectKeyword('interface');
    var name = this.parseName();
    var interfaces = this.parseImplementsInterfaces();
    var directives = this.parseDirectives(true);
    var fields = this.parseFieldsDefinition();

    if (interfaces.length === 0 && directives.length === 0 && fields.length === 0) {
      throw this.unexpected();
    }

    return {
      kind: _kinds.Kind.INTERFACE_TYPE_EXTENSION,
      name: name,
      interfaces: interfaces,
      directives: directives,
      fields: fields,
      loc: this.loc(start)
    };
  }
  /**
   * UnionTypeExtension :
   *   - extend union Name Directives[Const]? UnionMemberTypes
   *   - extend union Name Directives[Const]
   */
  ;

  _proto.parseUnionTypeExtension = function parseUnionTypeExtension() {
    var start = this._lexer.token;
    this.expectKeyword('extend');
    this.expectKeyword('union');
    var name = this.parseName();
    var directives = this.parseDirectives(true);
    var types = this.parseUnionMemberTypes();

    if (directives.length === 0 && types.length === 0) {
      throw this.unexpected();
    }

    return {
      kind: _kinds.Kind.UNION_TYPE_EXTENSION,
      name: name,
      directives: directives,
      types: types,
      loc: this.loc(start)
    };
  }
  /**
   * EnumTypeExtension :
   *   - extend enum Name Directives[Const]? EnumValuesDefinition
   *   - extend enum Name Directives[Const]
   */
  ;

  _proto.parseEnumTypeExtension = function parseEnumTypeExtension() {
    var start = this._lexer.token;
    this.expectKeyword('extend');
    this.expectKeyword('enum');
    var name = this.parseName();
    var directives = this.parseDirectives(true);
    var values = this.parseEnumValuesDefinition();

    if (directives.length === 0 && values.length === 0) {
      throw this.unexpected();
    }

    return {
      kind: _kinds.Kind.ENUM_TYPE_EXTENSION,
      name: name,
      directives: directives,
      values: values,
      loc: this.loc(start)
    };
  }
  /**
   * InputObjectTypeExtension :
   *   - extend input Name Directives[Const]? InputFieldsDefinition
   *   - extend input Name Directives[Const]
   */
  ;

  _proto.parseInputObjectTypeExtension = function parseInputObjectTypeExtension() {
    var start = this._lexer.token;
    this.expectKeyword('extend');
    this.expectKeyword('input');
    var name = this.parseName();
    var directives = this.parseDirectives(true);
    var fields = this.parseInputFieldsDefinition();

    if (directives.length === 0 && fields.length === 0) {
      throw this.unexpected();
    }

    return {
      kind: _kinds.Kind.INPUT_OBJECT_TYPE_EXTENSION,
      name: name,
      directives: directives,
      fields: fields,
      loc: this.loc(start)
    };
  }
  /**
   * DirectiveDefinition :
   *   - Description? directive @ Name ArgumentsDefinition? `repeatable`? on DirectiveLocations
   */
  ;

  _proto.parseDirectiveDefinition = function parseDirectiveDefinition() {
    var start = this._lexer.token;
    var description = this.parseDescription();
    this.expectKeyword('directive');
    this.expectToken(_tokenKind.TokenKind.AT);
    var name = this.parseName();
    var args = this.parseArgumentDefs();
    var repeatable = this.expectOptionalKeyword('repeatable');
    this.expectKeyword('on');
    var locations = this.parseDirectiveLocations();
    return {
      kind: _kinds.Kind.DIRECTIVE_DEFINITION,
      description: description,
      name: name,
      arguments: args,
      repeatable: repeatable,
      locations: locations,
      loc: this.loc(start)
    };
  }
  /**
   * DirectiveLocations :
   *   - `|`? DirectiveLocation
   *   - DirectiveLocations | DirectiveLocation
   */
  ;

  _proto.parseDirectiveLocations = function parseDirectiveLocations() {
    return this.delimitedMany(_tokenKind.TokenKind.PIPE, this.parseDirectiveLocation);
  }
  /*
   * DirectiveLocation :
   *   - ExecutableDirectiveLocation
   *   - TypeSystemDirectiveLocation
   *
   * ExecutableDirectiveLocation : one of
   *   `QUERY`
   *   `MUTATION`
   *   `SUBSCRIPTION`
   *   `FIELD`
   *   `FRAGMENT_DEFINITION`
   *   `FRAGMENT_SPREAD`
   *   `INLINE_FRAGMENT`
   *
   * TypeSystemDirectiveLocation : one of
   *   `SCHEMA`
   *   `SCALAR`
   *   `OBJECT`
   *   `FIELD_DEFINITION`
   *   `ARGUMENT_DEFINITION`
   *   `INTERFACE`
   *   `UNION`
   *   `ENUM`
   *   `ENUM_VALUE`
   *   `INPUT_OBJECT`
   *   `INPUT_FIELD_DEFINITION`
   */
  ;

  _proto.parseDirectiveLocation = function parseDirectiveLocation() {
    var start = this._lexer.token;
    var name = this.parseName();

    if (_directiveLocation.DirectiveLocation[name.value] !== undefined) {
      return name;
    }

    throw this.unexpected(start);
  } // Core parsing utility functions

  /**
   * Returns a location object, used to identify the place in the source that created a given parsed object.
   */
  ;

  _proto.loc = function loc(startToken) {
    var _this$_options4;

    if (((_this$_options4 = this._options) === null || _this$_options4 === void 0 ? void 0 : _this$_options4.noLocation) !== true) {
      return new _ast.Location(startToken, this._lexer.lastToken, this._lexer.source);
    }
  }
  /**
   * Determines if the next token is of a given kind
   */
  ;

  _proto.peek = function peek(kind) {
    return this._lexer.token.kind === kind;
  }
  /**
   * If the next token is of the given kind, return that token after advancing the lexer.
   * Otherwise, do not change the parser state and throw an error.
   */
  ;

  _proto.expectToken = function expectToken(kind) {
    var token = this._lexer.token;

    if (token.kind === kind) {
      this._lexer.advance();

      return token;
    }

    throw (0, _syntaxError.syntaxError)(this._lexer.source, token.start, "Expected ".concat(getTokenKindDesc(kind), ", found ").concat(getTokenDesc(token), "."));
  }
  /**
   * If the next token is of the given kind, return that token after advancing the lexer.
   * Otherwise, do not change the parser state and return undefined.
   */
  ;

  _proto.expectOptionalToken = function expectOptionalToken(kind) {
    var token = this._lexer.token;

    if (token.kind === kind) {
      this._lexer.advance();

      return token;
    }

    return undefined;
  }
  /**
   * If the next token is a given keyword, advance the lexer.
   * Otherwise, do not change the parser state and throw an error.
   */
  ;

  _proto.expectKeyword = function expectKeyword(value) {
    var token = this._lexer.token;

    if (token.kind === _tokenKind.TokenKind.NAME && token.value === value) {
      this._lexer.advance();
    } else {
      throw (0, _syntaxError.syntaxError)(this._lexer.source, token.start, "Expected \"".concat(value, "\", found ").concat(getTokenDesc(token), "."));
    }
  }
  /**
   * If the next token is a given keyword, return "true" after advancing the lexer.
   * Otherwise, do not change the parser state and return "false".
   */
  ;

  _proto.expectOptionalKeyword = function expectOptionalKeyword(value) {
    var token = this._lexer.token;

    if (token.kind === _tokenKind.TokenKind.NAME && token.value === value) {
      this._lexer.advance();

      return true;
    }

    return false;
  }
  /**
   * Helper function for creating an error when an unexpected lexed token is encountered.
   */
  ;

  _proto.unexpected = function unexpected(atToken) {
    var token = atToken !== null && atToken !== void 0 ? atToken : this._lexer.token;
    return (0, _syntaxError.syntaxError)(this._lexer.source, token.start, "Unexpected ".concat(getTokenDesc(token), "."));
  }
  /**
   * Returns a possibly empty list of parse nodes, determined by the parseFn.
   * This list begins with a lex token of openKind and ends with a lex token of closeKind.
   * Advances the parser to the next lex token after the closing token.
   */
  ;

  _proto.any = function any(openKind, parseFn, closeKind) {
    this.expectToken(openKind);
    var nodes = [];

    while (!this.expectOptionalToken(closeKind)) {
      nodes.push(parseFn.call(this));
    }

    return nodes;
  }
  /**
   * Returns a list of parse nodes, determined by the parseFn.
   * It can be empty only if open token is missing otherwise it will always return non-empty list
   * that begins with a lex token of openKind and ends with a lex token of closeKind.
   * Advances the parser to the next lex token after the closing token.
   */
  ;

  _proto.optionalMany = function optionalMany(openKind, parseFn, closeKind) {
    if (this.expectOptionalToken(openKind)) {
      var nodes = [];

      do {
        nodes.push(parseFn.call(this));
      } while (!this.expectOptionalToken(closeKind));

      return nodes;
    }

    return [];
  }
  /**
   * Returns a non-empty list of parse nodes, determined by the parseFn.
   * This list begins with a lex token of openKind and ends with a lex token of closeKind.
   * Advances the parser to the next lex token after the closing token.
   */
  ;

  _proto.many = function many(openKind, parseFn, closeKind) {
    this.expectToken(openKind);
    var nodes = [];

    do {
      nodes.push(parseFn.call(this));
    } while (!this.expectOptionalToken(closeKind));

    return nodes;
  }
  /**
   * Returns a non-empty list of parse nodes, determined by the parseFn.
   * This list may begin with a lex token of delimiterKind followed by items separated by lex tokens of tokenKind.
   * Advances the parser to the next lex token after last item in the list.
   */
  ;

  _proto.delimitedMany = function delimitedMany(delimiterKind, parseFn) {
    this.expectOptionalToken(delimiterKind);
    var nodes = [];

    do {
      nodes.push(parseFn.call(this));
    } while (this.expectOptionalToken(delimiterKind));

    return nodes;
  };

  return Parser;
}();
/**
 * A helper function to describe a token as a string for debugging.
 */


exports.Parser = Parser;

function getTokenDesc(token) {
  var value = token.value;
  return getTokenKindDesc(token.kind) + (value != null ? " \"".concat(value, "\"") : '');
}
/**
 * A helper function to describe a token kind as a string for debugging.
 */


function getTokenKindDesc(kind) {
  return (0, _lexer.isPunctuatorTokenKind)(kind) ? "\"".concat(kind, "\"") : kind;
}

},{"../error/syntaxError.js":22,"./ast.js":51,"./directiveLocation.js":53,"./kinds.js":55,"./lexer.js":56,"./source.js":62,"./tokenKind.js":63}],59:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isDefinitionNode = isDefinitionNode;
exports.isExecutableDefinitionNode = isExecutableDefinitionNode;
exports.isSelectionNode = isSelectionNode;
exports.isValueNode = isValueNode;
exports.isTypeNode = isTypeNode;
exports.isTypeSystemDefinitionNode = isTypeSystemDefinitionNode;
exports.isTypeDefinitionNode = isTypeDefinitionNode;
exports.isTypeSystemExtensionNode = isTypeSystemExtensionNode;
exports.isTypeExtensionNode = isTypeExtensionNode;

var _kinds = require("./kinds.js");

function isDefinitionNode(node) {
  return isExecutableDefinitionNode(node) || isTypeSystemDefinitionNode(node) || isTypeSystemExtensionNode(node);
}

function isExecutableDefinitionNode(node) {
  return node.kind === _kinds.Kind.OPERATION_DEFINITION || node.kind === _kinds.Kind.FRAGMENT_DEFINITION;
}

function isSelectionNode(node) {
  return node.kind === _kinds.Kind.FIELD || node.kind === _kinds.Kind.FRAGMENT_SPREAD || node.kind === _kinds.Kind.INLINE_FRAGMENT;
}

function isValueNode(node) {
  return node.kind === _kinds.Kind.VARIABLE || node.kind === _kinds.Kind.INT || node.kind === _kinds.Kind.FLOAT || node.kind === _kinds.Kind.STRING || node.kind === _kinds.Kind.BOOLEAN || node.kind === _kinds.Kind.NULL || node.kind === _kinds.Kind.ENUM || node.kind === _kinds.Kind.LIST || node.kind === _kinds.Kind.OBJECT;
}

function isTypeNode(node) {
  return node.kind === _kinds.Kind.NAMED_TYPE || node.kind === _kinds.Kind.LIST_TYPE || node.kind === _kinds.Kind.NON_NULL_TYPE;
}

function isTypeSystemDefinitionNode(node) {
  return node.kind === _kinds.Kind.SCHEMA_DEFINITION || isTypeDefinitionNode(node) || node.kind === _kinds.Kind.DIRECTIVE_DEFINITION;
}

function isTypeDefinitionNode(node) {
  return node.kind === _kinds.Kind.SCALAR_TYPE_DEFINITION || node.kind === _kinds.Kind.OBJECT_TYPE_DEFINITION || node.kind === _kinds.Kind.INTERFACE_TYPE_DEFINITION || node.kind === _kinds.Kind.UNION_TYPE_DEFINITION || node.kind === _kinds.Kind.ENUM_TYPE_DEFINITION || node.kind === _kinds.Kind.INPUT_OBJECT_TYPE_DEFINITION;
}

function isTypeSystemExtensionNode(node) {
  return node.kind === _kinds.Kind.SCHEMA_EXTENSION || isTypeExtensionNode(node);
}

function isTypeExtensionNode(node) {
  return node.kind === _kinds.Kind.SCALAR_TYPE_EXTENSION || node.kind === _kinds.Kind.OBJECT_TYPE_EXTENSION || node.kind === _kinds.Kind.INTERFACE_TYPE_EXTENSION || node.kind === _kinds.Kind.UNION_TYPE_EXTENSION || node.kind === _kinds.Kind.ENUM_TYPE_EXTENSION || node.kind === _kinds.Kind.INPUT_OBJECT_TYPE_EXTENSION;
}

},{"./kinds.js":55}],60:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.printLocation = printLocation;
exports.printSourceLocation = printSourceLocation;

var _location = require("./location.js");

/**
 * Render a helpful description of the location in the GraphQL Source document.
 */
function printLocation(location) {
  return printSourceLocation(location.source, (0, _location.getLocation)(location.source, location.start));
}
/**
 * Render a helpful description of the location in the GraphQL Source document.
 */


function printSourceLocation(source, sourceLocation) {
  var firstLineColumnOffset = source.locationOffset.column - 1;
  var body = whitespace(firstLineColumnOffset) + source.body;
  var lineIndex = sourceLocation.line - 1;
  var lineOffset = source.locationOffset.line - 1;
  var lineNum = sourceLocation.line + lineOffset;
  var columnOffset = sourceLocation.line === 1 ? firstLineColumnOffset : 0;
  var columnNum = sourceLocation.column + columnOffset;
  var locationStr = "".concat(source.name, ":").concat(lineNum, ":").concat(columnNum, "\n");
  var lines = body.split(/\r\n|[\n\r]/g);
  var locationLine = lines[lineIndex]; // Special case for minified documents

  if (locationLine.length > 120) {
    var subLineIndex = Math.floor(columnNum / 80);
    var subLineColumnNum = columnNum % 80;
    var subLines = [];

    for (var i = 0; i < locationLine.length; i += 80) {
      subLines.push(locationLine.slice(i, i + 80));
    }

    return locationStr + printPrefixedLines([["".concat(lineNum), subLines[0]]].concat(subLines.slice(1, subLineIndex + 1).map(function (subLine) {
      return ['', subLine];
    }), [[' ', whitespace(subLineColumnNum - 1) + '^'], ['', subLines[subLineIndex + 1]]]));
  }

  return locationStr + printPrefixedLines([// Lines specified like this: ["prefix", "string"],
  ["".concat(lineNum - 1), lines[lineIndex - 1]], ["".concat(lineNum), locationLine], ['', whitespace(columnNum - 1) + '^'], ["".concat(lineNum + 1), lines[lineIndex + 1]]]);
}

function printPrefixedLines(lines) {
  var existingLines = lines.filter(function (_ref) {
    var _ = _ref[0],
        line = _ref[1];
    return line !== undefined;
  });
  var padLen = Math.max.apply(Math, existingLines.map(function (_ref2) {
    var prefix = _ref2[0];
    return prefix.length;
  }));
  return existingLines.map(function (_ref3) {
    var prefix = _ref3[0],
        line = _ref3[1];
    return leftPad(padLen, prefix) + (line ? ' | ' + line : ' |');
  }).join('\n');
}

function whitespace(len) {
  return Array(len + 1).join(' ');
}

function leftPad(len, str) {
  return whitespace(len - str.length) + str;
}

},{"./location.js":57}],61:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.print = print;

var _visitor = require("./visitor.js");

var _blockString = require("./blockString.js");

/**
 * Converts an AST into a string, using one set of reasonable
 * formatting rules.
 */
function print(ast) {
  return (0, _visitor.visit)(ast, {
    leave: printDocASTReducer
  });
}

var MAX_LINE_LENGTH = 80; // TODO: provide better type coverage in future

var printDocASTReducer = {
  Name: function Name(node) {
    return node.value;
  },
  Variable: function Variable(node) {
    return '$' + node.name;
  },
  // Document
  Document: function Document(node) {
    return join(node.definitions, '\n\n') + '\n';
  },
  OperationDefinition: function OperationDefinition(node) {
    var op = node.operation;
    var name = node.name;
    var varDefs = wrap('(', join(node.variableDefinitions, ', '), ')');
    var directives = join(node.directives, ' ');
    var selectionSet = node.selectionSet; // Anonymous queries with no directives or variable definitions can use
    // the query short form.

    return !name && !directives && !varDefs && op === 'query' ? selectionSet : join([op, join([name, varDefs]), directives, selectionSet], ' ');
  },
  VariableDefinition: function VariableDefinition(_ref) {
    var variable = _ref.variable,
        type = _ref.type,
        defaultValue = _ref.defaultValue,
        directives = _ref.directives;
    return variable + ': ' + type + wrap(' = ', defaultValue) + wrap(' ', join(directives, ' '));
  },
  SelectionSet: function SelectionSet(_ref2) {
    var selections = _ref2.selections;
    return block(selections);
  },
  Field: function Field(_ref3) {
    var alias = _ref3.alias,
        name = _ref3.name,
        args = _ref3.arguments,
        directives = _ref3.directives,
        selectionSet = _ref3.selectionSet;
    var prefix = wrap('', alias, ': ') + name;
    var argsLine = prefix + wrap('(', join(args, ', '), ')');

    if (argsLine.length > MAX_LINE_LENGTH) {
      argsLine = prefix + wrap('(\n', indent(join(args, '\n')), '\n)');
    }

    return join([argsLine, join(directives, ' '), selectionSet], ' ');
  },
  Argument: function Argument(_ref4) {
    var name = _ref4.name,
        value = _ref4.value;
    return name + ': ' + value;
  },
  // Fragments
  FragmentSpread: function FragmentSpread(_ref5) {
    var name = _ref5.name,
        directives = _ref5.directives;
    return '...' + name + wrap(' ', join(directives, ' '));
  },
  InlineFragment: function InlineFragment(_ref6) {
    var typeCondition = _ref6.typeCondition,
        directives = _ref6.directives,
        selectionSet = _ref6.selectionSet;
    return join(['...', wrap('on ', typeCondition), join(directives, ' '), selectionSet], ' ');
  },
  FragmentDefinition: function FragmentDefinition(_ref7) {
    var name = _ref7.name,
        typeCondition = _ref7.typeCondition,
        variableDefinitions = _ref7.variableDefinitions,
        directives = _ref7.directives,
        selectionSet = _ref7.selectionSet;
    return (// Note: fragment variable definitions are experimental and may be changed
      // or removed in the future.
      "fragment ".concat(name).concat(wrap('(', join(variableDefinitions, ', '), ')'), " ") + "on ".concat(typeCondition, " ").concat(wrap('', join(directives, ' '), ' ')) + selectionSet
    );
  },
  // Value
  IntValue: function IntValue(_ref8) {
    var value = _ref8.value;
    return value;
  },
  FloatValue: function FloatValue(_ref9) {
    var value = _ref9.value;
    return value;
  },
  StringValue: function StringValue(_ref10, key) {
    var value = _ref10.value,
        isBlockString = _ref10.block;
    return isBlockString ? (0, _blockString.printBlockString)(value, key === 'description' ? '' : '  ') : JSON.stringify(value);
  },
  BooleanValue: function BooleanValue(_ref11) {
    var value = _ref11.value;
    return value ? 'true' : 'false';
  },
  NullValue: function NullValue() {
    return 'null';
  },
  EnumValue: function EnumValue(_ref12) {
    var value = _ref12.value;
    return value;
  },
  ListValue: function ListValue(_ref13) {
    var values = _ref13.values;
    return '[' + join(values, ', ') + ']';
  },
  ObjectValue: function ObjectValue(_ref14) {
    var fields = _ref14.fields;
    return '{' + join(fields, ', ') + '}';
  },
  ObjectField: function ObjectField(_ref15) {
    var name = _ref15.name,
        value = _ref15.value;
    return name + ': ' + value;
  },
  // Directive
  Directive: function Directive(_ref16) {
    var name = _ref16.name,
        args = _ref16.arguments;
    return '@' + name + wrap('(', join(args, ', '), ')');
  },
  // Type
  NamedType: function NamedType(_ref17) {
    var name = _ref17.name;
    return name;
  },
  ListType: function ListType(_ref18) {
    var type = _ref18.type;
    return '[' + type + ']';
  },
  NonNullType: function NonNullType(_ref19) {
    var type = _ref19.type;
    return type + '!';
  },
  // Type System Definitions
  SchemaDefinition: addDescription(function (_ref20) {
    var directives = _ref20.directives,
        operationTypes = _ref20.operationTypes;
    return join(['schema', join(directives, ' '), block(operationTypes)], ' ');
  }),
  OperationTypeDefinition: function OperationTypeDefinition(_ref21) {
    var operation = _ref21.operation,
        type = _ref21.type;
    return operation + ': ' + type;
  },
  ScalarTypeDefinition: addDescription(function (_ref22) {
    var name = _ref22.name,
        directives = _ref22.directives;
    return join(['scalar', name, join(directives, ' ')], ' ');
  }),
  ObjectTypeDefinition: addDescription(function (_ref23) {
    var name = _ref23.name,
        interfaces = _ref23.interfaces,
        directives = _ref23.directives,
        fields = _ref23.fields;
    return join(['type', name, wrap('implements ', join(interfaces, ' & ')), join(directives, ' '), block(fields)], ' ');
  }),
  FieldDefinition: addDescription(function (_ref24) {
    var name = _ref24.name,
        args = _ref24.arguments,
        type = _ref24.type,
        directives = _ref24.directives;
    return name + (hasMultilineItems(args) ? wrap('(\n', indent(join(args, '\n')), '\n)') : wrap('(', join(args, ', '), ')')) + ': ' + type + wrap(' ', join(directives, ' '));
  }),
  InputValueDefinition: addDescription(function (_ref25) {
    var name = _ref25.name,
        type = _ref25.type,
        defaultValue = _ref25.defaultValue,
        directives = _ref25.directives;
    return join([name + ': ' + type, wrap('= ', defaultValue), join(directives, ' ')], ' ');
  }),
  InterfaceTypeDefinition: addDescription(function (_ref26) {
    var name = _ref26.name,
        interfaces = _ref26.interfaces,
        directives = _ref26.directives,
        fields = _ref26.fields;
    return join(['interface', name, wrap('implements ', join(interfaces, ' & ')), join(directives, ' '), block(fields)], ' ');
  }),
  UnionTypeDefinition: addDescription(function (_ref27) {
    var name = _ref27.name,
        directives = _ref27.directives,
        types = _ref27.types;
    return join(['union', name, join(directives, ' '), types && types.length !== 0 ? '= ' + join(types, ' | ') : ''], ' ');
  }),
  EnumTypeDefinition: addDescription(function (_ref28) {
    var name = _ref28.name,
        directives = _ref28.directives,
        values = _ref28.values;
    return join(['enum', name, join(directives, ' '), block(values)], ' ');
  }),
  EnumValueDefinition: addDescription(function (_ref29) {
    var name = _ref29.name,
        directives = _ref29.directives;
    return join([name, join(directives, ' ')], ' ');
  }),
  InputObjectTypeDefinition: addDescription(function (_ref30) {
    var name = _ref30.name,
        directives = _ref30.directives,
        fields = _ref30.fields;
    return join(['input', name, join(directives, ' '), block(fields)], ' ');
  }),
  DirectiveDefinition: addDescription(function (_ref31) {
    var name = _ref31.name,
        args = _ref31.arguments,
        repeatable = _ref31.repeatable,
        locations = _ref31.locations;
    return 'directive @' + name + (hasMultilineItems(args) ? wrap('(\n', indent(join(args, '\n')), '\n)') : wrap('(', join(args, ', '), ')')) + (repeatable ? ' repeatable' : '') + ' on ' + join(locations, ' | ');
  }),
  SchemaExtension: function SchemaExtension(_ref32) {
    var directives = _ref32.directives,
        operationTypes = _ref32.operationTypes;
    return join(['extend schema', join(directives, ' '), block(operationTypes)], ' ');
  },
  ScalarTypeExtension: function ScalarTypeExtension(_ref33) {
    var name = _ref33.name,
        directives = _ref33.directives;
    return join(['extend scalar', name, join(directives, ' ')], ' ');
  },
  ObjectTypeExtension: function ObjectTypeExtension(_ref34) {
    var name = _ref34.name,
        interfaces = _ref34.interfaces,
        directives = _ref34.directives,
        fields = _ref34.fields;
    return join(['extend type', name, wrap('implements ', join(interfaces, ' & ')), join(directives, ' '), block(fields)], ' ');
  },
  InterfaceTypeExtension: function InterfaceTypeExtension(_ref35) {
    var name = _ref35.name,
        interfaces = _ref35.interfaces,
        directives = _ref35.directives,
        fields = _ref35.fields;
    return join(['extend interface', name, wrap('implements ', join(interfaces, ' & ')), join(directives, ' '), block(fields)], ' ');
  },
  UnionTypeExtension: function UnionTypeExtension(_ref36) {
    var name = _ref36.name,
        directives = _ref36.directives,
        types = _ref36.types;
    return join(['extend union', name, join(directives, ' '), types && types.length !== 0 ? '= ' + join(types, ' | ') : ''], ' ');
  },
  EnumTypeExtension: function EnumTypeExtension(_ref37) {
    var name = _ref37.name,
        directives = _ref37.directives,
        values = _ref37.values;
    return join(['extend enum', name, join(directives, ' '), block(values)], ' ');
  },
  InputObjectTypeExtension: function InputObjectTypeExtension(_ref38) {
    var name = _ref38.name,
        directives = _ref38.directives,
        fields = _ref38.fields;
    return join(['extend input', name, join(directives, ' '), block(fields)], ' ');
  }
};

function addDescription(cb) {
  return function (node) {
    return join([node.description, cb(node)], '\n');
  };
}
/**
 * Given maybeArray, print an empty string if it is null or empty, otherwise
 * print all items together separated by separator if provided
 */


function join(maybeArray) {
  var _maybeArray$filter$jo;

  var separator = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : '';
  return (_maybeArray$filter$jo = maybeArray === null || maybeArray === void 0 ? void 0 : maybeArray.filter(function (x) {
    return x;
  }).join(separator)) !== null && _maybeArray$filter$jo !== void 0 ? _maybeArray$filter$jo : '';
}
/**
 * Given array, print each item on its own line, wrapped in an
 * indented "{ }" block.
 */


function block(array) {
  return wrap('{\n', indent(join(array, '\n')), '\n}');
}
/**
 * If maybeString is not null or empty, then wrap with start and end, otherwise print an empty string.
 */


function wrap(start, maybeString) {
  var end = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : '';
  return maybeString != null && maybeString !== '' ? start + maybeString + end : '';
}

function indent(str) {
  return wrap('  ', str.replace(/\n/g, '\n  '));
}

function isMultiline(str) {
  return str.indexOf('\n') !== -1;
}

function hasMultilineItems(maybeArray) {
  return maybeArray != null && maybeArray.some(isMultiline);
}

},{"./blockString.js":52,"./visitor.js":64}],62:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isSource = isSource;
exports.Source = void 0;

var _symbols = require("../polyfills/symbols.js");

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _devAssert = _interopRequireDefault(require("../jsutils/devAssert.js"));

var _instanceOf = _interopRequireDefault(require("../jsutils/instanceOf.js"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

/**
 * A representation of source input to GraphQL. The `name` and `locationOffset` parameters are
 * optional, but they are useful for clients who store GraphQL documents in source files.
 * For example, if the GraphQL input starts at line 40 in a file named `Foo.graphql`, it might
 * be useful for `name` to be `"Foo.graphql"` and location to be `{ line: 40, column: 1 }`.
 * The `line` and `column` properties in `locationOffset` are 1-indexed.
 */
var Source = /*#__PURE__*/function () {
  function Source(body) {
    var name = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 'GraphQL request';
    var locationOffset = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : {
      line: 1,
      column: 1
    };
    typeof body === 'string' || (0, _devAssert.default)(0, "Body must be a string. Received: ".concat((0, _inspect.default)(body), "."));
    this.body = body;
    this.name = name;
    this.locationOffset = locationOffset;
    this.locationOffset.line > 0 || (0, _devAssert.default)(0, 'line in locationOffset is 1-indexed and must be positive.');
    this.locationOffset.column > 0 || (0, _devAssert.default)(0, 'column in locationOffset is 1-indexed and must be positive.');
  } // $FlowFixMe[unsupported-syntax] Flow doesn't support computed properties yet


  _createClass(Source, [{
    key: _symbols.SYMBOL_TO_STRING_TAG,
    get: function get() {
      return 'Source';
    }
  }]);

  return Source;
}();
/**
 * Test if the given value is a Source object.
 *
 * @internal
 */


exports.Source = Source;

// eslint-disable-next-line no-redeclare
function isSource(source) {
  return (0, _instanceOf.default)(source, Source);
}

},{"../jsutils/devAssert.js":30,"../jsutils/inspect.js":33,"../jsutils/instanceOf.js":34,"../polyfills/symbols.js":71}],63:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TokenKind = void 0;

/**
 * An exported enum describing the different kinds of tokens that the
 * lexer emits.
 */
var TokenKind = Object.freeze({
  SOF: '<SOF>',
  EOF: '<EOF>',
  BANG: '!',
  DOLLAR: '$',
  AMP: '&',
  PAREN_L: '(',
  PAREN_R: ')',
  SPREAD: '...',
  COLON: ':',
  EQUALS: '=',
  AT: '@',
  BRACKET_L: '[',
  BRACKET_R: ']',
  BRACE_L: '{',
  PIPE: '|',
  BRACE_R: '}',
  NAME: 'Name',
  INT: 'Int',
  FLOAT: 'Float',
  STRING: 'String',
  BLOCK_STRING: 'BlockString',
  COMMENT: 'Comment'
});
/**
 * The enum type representing the token kinds values.
 */

exports.TokenKind = TokenKind;

},{}],64:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.visit = visit;
exports.visitInParallel = visitInParallel;
exports.getVisitFn = getVisitFn;
exports.BREAK = exports.QueryDocumentKeys = void 0;

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _ast = require("./ast.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var QueryDocumentKeys = {
  Name: [],
  Document: ['definitions'],
  OperationDefinition: ['name', 'variableDefinitions', 'directives', 'selectionSet'],
  VariableDefinition: ['variable', 'type', 'defaultValue', 'directives'],
  Variable: ['name'],
  SelectionSet: ['selections'],
  Field: ['alias', 'name', 'arguments', 'directives', 'selectionSet'],
  Argument: ['name', 'value'],
  FragmentSpread: ['name', 'directives'],
  InlineFragment: ['typeCondition', 'directives', 'selectionSet'],
  FragmentDefinition: ['name', // Note: fragment variable definitions are experimental and may be changed
  // or removed in the future.
  'variableDefinitions', 'typeCondition', 'directives', 'selectionSet'],
  IntValue: [],
  FloatValue: [],
  StringValue: [],
  BooleanValue: [],
  NullValue: [],
  EnumValue: [],
  ListValue: ['values'],
  ObjectValue: ['fields'],
  ObjectField: ['name', 'value'],
  Directive: ['name', 'arguments'],
  NamedType: ['name'],
  ListType: ['type'],
  NonNullType: ['type'],
  SchemaDefinition: ['description', 'directives', 'operationTypes'],
  OperationTypeDefinition: ['type'],
  ScalarTypeDefinition: ['description', 'name', 'directives'],
  ObjectTypeDefinition: ['description', 'name', 'interfaces', 'directives', 'fields'],
  FieldDefinition: ['description', 'name', 'arguments', 'type', 'directives'],
  InputValueDefinition: ['description', 'name', 'type', 'defaultValue', 'directives'],
  InterfaceTypeDefinition: ['description', 'name', 'interfaces', 'directives', 'fields'],
  UnionTypeDefinition: ['description', 'name', 'directives', 'types'],
  EnumTypeDefinition: ['description', 'name', 'directives', 'values'],
  EnumValueDefinition: ['description', 'name', 'directives'],
  InputObjectTypeDefinition: ['description', 'name', 'directives', 'fields'],
  DirectiveDefinition: ['description', 'name', 'arguments', 'locations'],
  SchemaExtension: ['directives', 'operationTypes'],
  ScalarTypeExtension: ['name', 'directives'],
  ObjectTypeExtension: ['name', 'interfaces', 'directives', 'fields'],
  InterfaceTypeExtension: ['name', 'interfaces', 'directives', 'fields'],
  UnionTypeExtension: ['name', 'directives', 'types'],
  EnumTypeExtension: ['name', 'directives', 'values'],
  InputObjectTypeExtension: ['name', 'directives', 'fields']
};
exports.QueryDocumentKeys = QueryDocumentKeys;
var BREAK = Object.freeze({});
/**
 * visit() will walk through an AST using a depth-first traversal, calling
 * the visitor's enter function at each node in the traversal, and calling the
 * leave function after visiting that node and all of its child nodes.
 *
 * By returning different values from the enter and leave functions, the
 * behavior of the visitor can be altered, including skipping over a sub-tree of
 * the AST (by returning false), editing the AST by returning a value or null
 * to remove the value, or to stop the whole traversal by returning BREAK.
 *
 * When using visit() to edit an AST, the original AST will not be modified, and
 * a new version of the AST with the changes applied will be returned from the
 * visit function.
 *
 *     const editedAST = visit(ast, {
 *       enter(node, key, parent, path, ancestors) {
 *         // @return
 *         //   undefined: no action
 *         //   false: skip visiting this node
 *         //   visitor.BREAK: stop visiting altogether
 *         //   null: delete this node
 *         //   any value: replace this node with the returned value
 *       },
 *       leave(node, key, parent, path, ancestors) {
 *         // @return
 *         //   undefined: no action
 *         //   false: no action
 *         //   visitor.BREAK: stop visiting altogether
 *         //   null: delete this node
 *         //   any value: replace this node with the returned value
 *       }
 *     });
 *
 * Alternatively to providing enter() and leave() functions, a visitor can
 * instead provide functions named the same as the kinds of AST nodes, or
 * enter/leave visitors at a named key, leading to four permutations of the
 * visitor API:
 *
 * 1) Named visitors triggered when entering a node of a specific kind.
 *
 *     visit(ast, {
 *       Kind(node) {
 *         // enter the "Kind" node
 *       }
 *     })
 *
 * 2) Named visitors that trigger upon entering and leaving a node of
 *    a specific kind.
 *
 *     visit(ast, {
 *       Kind: {
 *         enter(node) {
 *           // enter the "Kind" node
 *         }
 *         leave(node) {
 *           // leave the "Kind" node
 *         }
 *       }
 *     })
 *
 * 3) Generic visitors that trigger upon entering and leaving any node.
 *
 *     visit(ast, {
 *       enter(node) {
 *         // enter any node
 *       },
 *       leave(node) {
 *         // leave any node
 *       }
 *     })
 *
 * 4) Parallel visitors for entering and leaving nodes of a specific kind.
 *
 *     visit(ast, {
 *       enter: {
 *         Kind(node) {
 *           // enter the "Kind" node
 *         }
 *       },
 *       leave: {
 *         Kind(node) {
 *           // leave the "Kind" node
 *         }
 *       }
 *     })
 */

exports.BREAK = BREAK;

function visit(root, visitor) {
  var visitorKeys = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : QueryDocumentKeys;

  /* eslint-disable no-undef-init */
  var stack = undefined;
  var inArray = Array.isArray(root);
  var keys = [root];
  var index = -1;
  var edits = [];
  var node = undefined;
  var key = undefined;
  var parent = undefined;
  var path = [];
  var ancestors = [];
  var newRoot = root;
  /* eslint-enable no-undef-init */

  do {
    index++;
    var isLeaving = index === keys.length;
    var isEdited = isLeaving && edits.length !== 0;

    if (isLeaving) {
      key = ancestors.length === 0 ? undefined : path[path.length - 1];
      node = parent;
      parent = ancestors.pop();

      if (isEdited) {
        if (inArray) {
          node = node.slice();
        } else {
          var clone = {};

          for (var _i2 = 0, _Object$keys2 = Object.keys(node); _i2 < _Object$keys2.length; _i2++) {
            var k = _Object$keys2[_i2];
            clone[k] = node[k];
          }

          node = clone;
        }

        var editOffset = 0;

        for (var ii = 0; ii < edits.length; ii++) {
          var editKey = edits[ii][0];
          var editValue = edits[ii][1];

          if (inArray) {
            editKey -= editOffset;
          }

          if (inArray && editValue === null) {
            node.splice(editKey, 1);
            editOffset++;
          } else {
            node[editKey] = editValue;
          }
        }
      }

      index = stack.index;
      keys = stack.keys;
      edits = stack.edits;
      inArray = stack.inArray;
      stack = stack.prev;
    } else {
      key = parent ? inArray ? index : keys[index] : undefined;
      node = parent ? parent[key] : newRoot;

      if (node === null || node === undefined) {
        continue;
      }

      if (parent) {
        path.push(key);
      }
    }

    var result = void 0;

    if (!Array.isArray(node)) {
      if (!(0, _ast.isNode)(node)) {
        throw new Error("Invalid AST Node: ".concat((0, _inspect.default)(node), "."));
      }

      var visitFn = getVisitFn(visitor, node.kind, isLeaving);

      if (visitFn) {
        result = visitFn.call(visitor, node, key, parent, path, ancestors);

        if (result === BREAK) {
          break;
        }

        if (result === false) {
          if (!isLeaving) {
            path.pop();
            continue;
          }
        } else if (result !== undefined) {
          edits.push([key, result]);

          if (!isLeaving) {
            if ((0, _ast.isNode)(result)) {
              node = result;
            } else {
              path.pop();
              continue;
            }
          }
        }
      }
    }

    if (result === undefined && isEdited) {
      edits.push([key, node]);
    }

    if (isLeaving) {
      path.pop();
    } else {
      var _visitorKeys$node$kin;

      stack = {
        inArray: inArray,
        index: index,
        keys: keys,
        edits: edits,
        prev: stack
      };
      inArray = Array.isArray(node);
      keys = inArray ? node : (_visitorKeys$node$kin = visitorKeys[node.kind]) !== null && _visitorKeys$node$kin !== void 0 ? _visitorKeys$node$kin : [];
      index = -1;
      edits = [];

      if (parent) {
        ancestors.push(parent);
      }

      parent = node;
    }
  } while (stack !== undefined);

  if (edits.length !== 0) {
    newRoot = edits[edits.length - 1][1];
  }

  return newRoot;
}
/**
 * Creates a new visitor instance which delegates to many visitors to run in
 * parallel. Each visitor will be visited for each node before moving on.
 *
 * If a prior visitor edits a node, no following visitors will see that node.
 */


function visitInParallel(visitors) {
  var skipping = new Array(visitors.length);
  return {
    enter: function enter(node) {
      for (var i = 0; i < visitors.length; i++) {
        if (skipping[i] == null) {
          var fn = getVisitFn(visitors[i], node.kind,
          /* isLeaving */
          false);

          if (fn) {
            var result = fn.apply(visitors[i], arguments);

            if (result === false) {
              skipping[i] = node;
            } else if (result === BREAK) {
              skipping[i] = BREAK;
            } else if (result !== undefined) {
              return result;
            }
          }
        }
      }
    },
    leave: function leave(node) {
      for (var i = 0; i < visitors.length; i++) {
        if (skipping[i] == null) {
          var fn = getVisitFn(visitors[i], node.kind,
          /* isLeaving */
          true);

          if (fn) {
            var result = fn.apply(visitors[i], arguments);

            if (result === BREAK) {
              skipping[i] = BREAK;
            } else if (result !== undefined && result !== false) {
              return result;
            }
          }
        } else if (skipping[i] === node) {
          skipping[i] = null;
        }
      }
    }
  };
}
/**
 * Given a visitor instance, if it is leaving or not, and a node kind, return
 * the function the visitor runtime should call.
 */


function getVisitFn(visitor, kind, isLeaving) {
  var kindVisitor = visitor[kind];

  if (kindVisitor) {
    if (!isLeaving && typeof kindVisitor === 'function') {
      // { Kind() {} }
      return kindVisitor;
    }

    var kindSpecificVisitor = isLeaving ? kindVisitor.leave : kindVisitor.enter;

    if (typeof kindSpecificVisitor === 'function') {
      // { Kind: { enter() {}, leave() {} } }
      return kindSpecificVisitor;
    }
  } else {
    var specificVisitor = isLeaving ? visitor.leave : visitor.enter;

    if (specificVisitor) {
      if (typeof specificVisitor === 'function') {
        // { enter() {}, leave() {} }
        return specificVisitor;
      }

      var specificKindVisitor = specificVisitor[kind];

      if (typeof specificKindVisitor === 'function') {
        // { enter: { Kind() {} }, leave: { Kind() {} } }
        return specificKindVisitor;
      }
    }
  }
}

},{"../jsutils/inspect.js":33,"./ast.js":51}],65:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _symbols = require("./symbols.js");

/* eslint-disable no-redeclare */
// $FlowFixMe[name-already-bound]
var arrayFrom = Array.from || function (obj, mapFn, thisArg) {
  if (obj == null) {
    throw new TypeError('Array.from requires an array-like object - not null or undefined');
  } // Is Iterable?


  var iteratorMethod = obj[_symbols.SYMBOL_ITERATOR];

  if (typeof iteratorMethod === 'function') {
    var iterator = iteratorMethod.call(obj);
    var result = [];
    var step;

    for (var i = 0; !(step = iterator.next()).done; ++i) {
      result.push(mapFn.call(thisArg, step.value, i)); // Infinite Iterators could cause forEach to run forever.
      // After a very large number of iterations, produce an error.
      // istanbul ignore if (Too big to actually test)

      if (i > 9999999) {
        throw new TypeError('Near-infinite iteration.');
      }
    }

    return result;
  } // Is Array like?


  var length = obj.length;

  if (typeof length === 'number' && length >= 0 && length % 1 === 0) {
    var _result = [];

    for (var _i = 0; _i < length; ++_i) {
      if (Object.prototype.hasOwnProperty.call(obj, _i)) {
        _result.push(mapFn.call(thisArg, obj[_i], _i));
      }
    }

    return _result;
  }

  return [];
};

var _default = arrayFrom;
exports.default = _default;

},{"./symbols.js":71}],66:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

/* eslint-disable no-redeclare */
// $FlowFixMe[name-already-bound]
var find = Array.prototype.find ? function (list, predicate) {
  return Array.prototype.find.call(list, predicate);
} : function (list, predicate) {
  for (var _i2 = 0; _i2 < list.length; _i2++) {
    var value = list[_i2];

    if (predicate(value)) {
      return value;
    }
  }
};
var _default = find;
exports.default = _default;

},{}],67:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

/* eslint-disable no-redeclare */
// $FlowFixMe[name-already-bound] workaround for: https://github.com/facebook/flow/issues/4441
var isFinitePolyfill = Number.isFinite || function (value) {
  return typeof value === 'number' && isFinite(value);
};

var _default = isFinitePolyfill;
exports.default = _default;

},{}],68:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

/* eslint-disable no-redeclare */
// $FlowFixMe[name-already-bound] workaround for: https://github.com/facebook/flow/issues/4441
var isInteger = Number.isInteger || function (value) {
  return typeof value === 'number' && isFinite(value) && Math.floor(value) === value;
};

var _default = isInteger;
exports.default = _default;

},{}],69:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

/* eslint-disable no-redeclare */
// $FlowFixMe[name-already-bound] workaround for: https://github.com/facebook/flow/issues/4441
var objectEntries = Object.entries || function (obj) {
  return Object.keys(obj).map(function (key) {
    return [key, obj[key]];
  });
};

var _default = objectEntries;
exports.default = _default;

},{}],70:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

/* eslint-disable no-redeclare */
// $FlowFixMe[name-already-bound] workaround for: https://github.com/facebook/flow/issues/4441
var objectValues = Object.values || function (obj) {
  return Object.keys(obj).map(function (key) {
    return obj[key];
  });
};

var _default = objectValues;
exports.default = _default;

},{}],71:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.SYMBOL_TO_STRING_TAG = exports.SYMBOL_ASYNC_ITERATOR = exports.SYMBOL_ITERATOR = void 0;
// In ES2015 (or a polyfilled) environment, this will be Symbol.iterator
// istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2317')
var SYMBOL_ITERATOR = typeof Symbol === 'function' && Symbol.iterator != null ? Symbol.iterator : '@@iterator'; // In ES2017 (or a polyfilled) environment, this will be Symbol.asyncIterator
// istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2317')

exports.SYMBOL_ITERATOR = SYMBOL_ITERATOR;
var SYMBOL_ASYNC_ITERATOR = typeof Symbol === 'function' && Symbol.asyncIterator != null ? Symbol.asyncIterator : '@@asyncIterator'; // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2317')

exports.SYMBOL_ASYNC_ITERATOR = SYMBOL_ASYNC_ITERATOR;
var SYMBOL_TO_STRING_TAG = typeof Symbol === 'function' && Symbol.toStringTag != null ? Symbol.toStringTag : '@@toStringTag';
exports.SYMBOL_TO_STRING_TAG = SYMBOL_TO_STRING_TAG;

},{}],72:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "subscribe", {
  enumerable: true,
  get: function get() {
    return _subscribe.subscribe;
  }
});
Object.defineProperty(exports, "createSourceEventStream", {
  enumerable: true,
  get: function get() {
    return _subscribe.createSourceEventStream;
  }
});

var _subscribe = require("./subscribe.js");

},{"./subscribe.js":74}],73:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = mapAsyncIterator;

var _symbols = require("../polyfills/symbols.js");

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

/**
 * Given an AsyncIterable and a callback function, return an AsyncIterator
 * which produces values mapped via calling the callback function.
 */
function mapAsyncIterator(iterable, callback, rejectCallback) {
  // $FlowFixMe[prop-missing]
  var iteratorMethod = iterable[_symbols.SYMBOL_ASYNC_ITERATOR];
  var iterator = iteratorMethod.call(iterable);
  var $return;
  var abruptClose;

  if (typeof iterator.return === 'function') {
    $return = iterator.return;

    abruptClose = function abruptClose(error) {
      var rethrow = function rethrow() {
        return Promise.reject(error);
      };

      return $return.call(iterator).then(rethrow, rethrow);
    };
  }

  function mapResult(result) {
    return result.done ? result : asyncMapValue(result.value, callback).then(iteratorResult, abruptClose);
  }

  var mapReject;

  if (rejectCallback) {
    // Capture rejectCallback to ensure it cannot be null.
    var reject = rejectCallback;

    mapReject = function mapReject(error) {
      return asyncMapValue(error, reject).then(iteratorResult, abruptClose);
    };
  }
  /* TODO: Flow doesn't support symbols as keys:
     https://github.com/facebook/flow/issues/3258 */


  return _defineProperty({
    next: function next() {
      return iterator.next().then(mapResult, mapReject);
    },
    return: function _return() {
      return $return ? $return.call(iterator).then(mapResult, mapReject) : Promise.resolve({
        value: undefined,
        done: true
      });
    },
    throw: function _throw(error) {
      if (typeof iterator.throw === 'function') {
        return iterator.throw(error).then(mapResult, mapReject);
      }

      return Promise.reject(error).catch(abruptClose);
    }
  }, _symbols.SYMBOL_ASYNC_ITERATOR, function () {
    return this;
  });
}

function asyncMapValue(value, callback) {
  return new Promise(function (resolve) {
    return resolve(callback(value));
  });
}

function iteratorResult(value) {
  return {
    value: value,
    done: false
  };
}

},{"../polyfills/symbols.js":71}],74:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.subscribe = subscribe;
exports.createSourceEventStream = createSourceEventStream;

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _isAsyncIterable = _interopRequireDefault(require("../jsutils/isAsyncIterable.js"));

var _Path = require("../jsutils/Path.js");

var _GraphQLError = require("../error/GraphQLError.js");

var _locatedError = require("../error/locatedError.js");

var _values = require("../execution/values.js");

var _execute = require("../execution/execute.js");

var _getOperationRootType = require("../utilities/getOperationRootType.js");

var _mapAsyncIterator = _interopRequireDefault(require("./mapAsyncIterator.js"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function subscribe(argsOrSchema, document, rootValue, contextValue, variableValues, operationName, fieldResolver, subscribeFieldResolver) {
  /* eslint-enable no-redeclare */
  // Extract arguments from object args if provided.
  return arguments.length === 1 ? subscribeImpl(argsOrSchema) : subscribeImpl({
    schema: argsOrSchema,
    document: document,
    rootValue: rootValue,
    contextValue: contextValue,
    variableValues: variableValues,
    operationName: operationName,
    fieldResolver: fieldResolver,
    subscribeFieldResolver: subscribeFieldResolver
  });
}
/**
 * This function checks if the error is a GraphQLError. If it is, report it as
 * an ExecutionResult, containing only errors and no data. Otherwise treat the
 * error as a system-class error and re-throw it.
 */


function reportGraphQLError(error) {
  if (error instanceof _GraphQLError.GraphQLError) {
    return {
      errors: [error]
    };
  }

  throw error;
}

function subscribeImpl(args) {
  var schema = args.schema,
      document = args.document,
      rootValue = args.rootValue,
      contextValue = args.contextValue,
      variableValues = args.variableValues,
      operationName = args.operationName,
      fieldResolver = args.fieldResolver,
      subscribeFieldResolver = args.subscribeFieldResolver;
  var sourcePromise = createSourceEventStream(schema, document, rootValue, contextValue, variableValues, operationName, subscribeFieldResolver); // For each payload yielded from a subscription, map it over the normal
  // GraphQL `execute` function, with `payload` as the rootValue.
  // This implements the "MapSourceToResponseEvent" algorithm described in
  // the GraphQL specification. The `execute` function provides the
  // "ExecuteSubscriptionEvent" algorithm, as it is nearly identical to the
  // "ExecuteQuery" algorithm, for which `execute` is also used.

  var mapSourceToResponse = function mapSourceToResponse(payload) {
    return (0, _execute.execute)({
      schema: schema,
      document: document,
      rootValue: payload,
      contextValue: contextValue,
      variableValues: variableValues,
      operationName: operationName,
      fieldResolver: fieldResolver
    });
  }; // Resolve the Source Stream, then map every source value to a
  // ExecutionResult value as described above.


  return sourcePromise.then(function (resultOrStream) {
    return (// Note: Flow can't refine isAsyncIterable, so explicit casts are used.
      (0, _isAsyncIterable.default)(resultOrStream) ? (0, _mapAsyncIterator.default)(resultOrStream, mapSourceToResponse, reportGraphQLError) : resultOrStream
    );
  });
}
/**
 * Implements the "CreateSourceEventStream" algorithm described in the
 * GraphQL specification, resolving the subscription source event stream.
 *
 * Returns a Promise which resolves to either an AsyncIterable (if successful)
 * or an ExecutionResult (error). The promise will be rejected if the schema or
 * other arguments to this function are invalid, or if the resolved event stream
 * is not an async iterable.
 *
 * If the client-provided arguments to this function do not result in a
 * compliant subscription, a GraphQL Response (ExecutionResult) with
 * descriptive errors and no data will be returned.
 *
 * If the the source stream could not be created due to faulty subscription
 * resolver logic or underlying systems, the promise will resolve to a single
 * ExecutionResult containing `errors` and no `data`.
 *
 * If the operation succeeded, the promise resolves to the AsyncIterable for the
 * event stream returned by the resolver.
 *
 * A Source Event Stream represents a sequence of events, each of which triggers
 * a GraphQL execution for that event.
 *
 * This may be useful when hosting the stateful subscription service in a
 * different process or machine than the stateless GraphQL execution engine,
 * or otherwise separating these two steps. For more on this, see the
 * "Supporting Subscriptions at Scale" information in the GraphQL specification.
 */


function createSourceEventStream(schema, document, rootValue, contextValue, variableValues, operationName, fieldResolver) {
  // If arguments are missing or incorrectly typed, this is an internal
  // developer mistake which should throw an early error.
  (0, _execute.assertValidExecutionArguments)(schema, document, variableValues);
  return new Promise(function (resolve) {
    // If a valid context cannot be created due to incorrect arguments,
    // this will throw an error.
    var exeContext = (0, _execute.buildExecutionContext)(schema, document, rootValue, contextValue, variableValues, operationName, fieldResolver);
    resolve( // Return early errors if execution context failed.
    Array.isArray(exeContext) ? {
      errors: exeContext
    } : executeSubscription(exeContext));
  }).catch(reportGraphQLError);
}

function executeSubscription(exeContext) {
  var schema = exeContext.schema,
      operation = exeContext.operation,
      variableValues = exeContext.variableValues,
      rootValue = exeContext.rootValue;
  var type = (0, _getOperationRootType.getOperationRootType)(schema, operation);
  var fields = (0, _execute.collectFields)(exeContext, type, operation.selectionSet, Object.create(null), Object.create(null));
  var responseNames = Object.keys(fields);
  var responseName = responseNames[0];
  var fieldNodes = fields[responseName];
  var fieldNode = fieldNodes[0];
  var fieldName = fieldNode.name.value;
  var fieldDef = (0, _execute.getFieldDef)(schema, type, fieldName);

  if (!fieldDef) {
    throw new _GraphQLError.GraphQLError("The subscription field \"".concat(fieldName, "\" is not defined."), fieldNodes);
  }

  var path = (0, _Path.addPath)(undefined, responseName, type.name);
  var info = (0, _execute.buildResolveInfo)(exeContext, fieldDef, fieldNodes, type, path); // Coerce to Promise for easier error handling and consistent return type.

  return new Promise(function (resolveResult) {
    var _fieldDef$subscribe;

    // Implements the "ResolveFieldEventStream" algorithm from GraphQL specification.
    // It differs from "ResolveFieldValue" due to providing a different `resolveFn`.
    // Build a JS object of arguments from the field.arguments AST, using the
    // variables scope to fulfill any variable references.
    var args = (0, _values.getArgumentValues)(fieldDef, fieldNodes[0], variableValues); // The resolve function's optional third argument is a context value that
    // is provided to every resolve function within an execution. It is commonly
    // used to represent an authenticated user, or request-specific caches.

    var contextValue = exeContext.contextValue; // Call the `subscribe()` resolver or the default resolver to produce an
    // AsyncIterable yielding raw payloads.

    var resolveFn = (_fieldDef$subscribe = fieldDef.subscribe) !== null && _fieldDef$subscribe !== void 0 ? _fieldDef$subscribe : exeContext.fieldResolver;
    resolveResult(resolveFn(rootValue, args, contextValue, info));
  }).then(function (eventStream) {
    if (eventStream instanceof Error) {
      throw (0, _locatedError.locatedError)(eventStream, fieldNodes, (0, _Path.pathToArray)(path));
    } // Assert field returned an event stream, otherwise yield an error.


    if (!(0, _isAsyncIterable.default)(eventStream)) {
      throw new Error('Subscription field must return Async Iterable. ' + "Received: ".concat((0, _inspect.default)(eventStream), "."));
    }

    return eventStream;
  }, function (error) {
    throw (0, _locatedError.locatedError)(error, fieldNodes, (0, _Path.pathToArray)(path));
  });
}

},{"../error/GraphQLError.js":18,"../error/locatedError.js":21,"../execution/execute.js":23,"../execution/values.js":25,"../jsutils/Path.js":28,"../jsutils/inspect.js":33,"../jsutils/isAsyncIterable.js":36,"../utilities/getOperationRootType.js":94,"./mapAsyncIterator.js":73}],75:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isType = isType;
exports.assertType = assertType;
exports.isScalarType = isScalarType;
exports.assertScalarType = assertScalarType;
exports.isObjectType = isObjectType;
exports.assertObjectType = assertObjectType;
exports.isInterfaceType = isInterfaceType;
exports.assertInterfaceType = assertInterfaceType;
exports.isUnionType = isUnionType;
exports.assertUnionType = assertUnionType;
exports.isEnumType = isEnumType;
exports.assertEnumType = assertEnumType;
exports.isInputObjectType = isInputObjectType;
exports.assertInputObjectType = assertInputObjectType;
exports.isListType = isListType;
exports.assertListType = assertListType;
exports.isNonNullType = isNonNullType;
exports.assertNonNullType = assertNonNullType;
exports.isInputType = isInputType;
exports.assertInputType = assertInputType;
exports.isOutputType = isOutputType;
exports.assertOutputType = assertOutputType;
exports.isLeafType = isLeafType;
exports.assertLeafType = assertLeafType;
exports.isCompositeType = isCompositeType;
exports.assertCompositeType = assertCompositeType;
exports.isAbstractType = isAbstractType;
exports.assertAbstractType = assertAbstractType;
exports.GraphQLList = GraphQLList;
exports.GraphQLNonNull = GraphQLNonNull;
exports.isWrappingType = isWrappingType;
exports.assertWrappingType = assertWrappingType;
exports.isNullableType = isNullableType;
exports.assertNullableType = assertNullableType;
exports.getNullableType = getNullableType;
exports.isNamedType = isNamedType;
exports.assertNamedType = assertNamedType;
exports.getNamedType = getNamedType;
exports.argsToArgsConfig = argsToArgsConfig;
exports.isRequiredArgument = isRequiredArgument;
exports.isRequiredInputField = isRequiredInputField;
exports.GraphQLInputObjectType = exports.GraphQLEnumType = exports.GraphQLUnionType = exports.GraphQLInterfaceType = exports.GraphQLObjectType = exports.GraphQLScalarType = void 0;

var _objectEntries = _interopRequireDefault(require("../polyfills/objectEntries.js"));

var _symbols = require("../polyfills/symbols.js");

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _keyMap = _interopRequireDefault(require("../jsutils/keyMap.js"));

var _mapValue = _interopRequireDefault(require("../jsutils/mapValue.js"));

var _toObjMap = _interopRequireDefault(require("../jsutils/toObjMap.js"));

var _devAssert = _interopRequireDefault(require("../jsutils/devAssert.js"));

var _keyValMap = _interopRequireDefault(require("../jsutils/keyValMap.js"));

var _instanceOf = _interopRequireDefault(require("../jsutils/instanceOf.js"));

var _didYouMean = _interopRequireDefault(require("../jsutils/didYouMean.js"));

var _isObjectLike = _interopRequireDefault(require("../jsutils/isObjectLike.js"));

var _identityFunc = _interopRequireDefault(require("../jsutils/identityFunc.js"));

var _defineInspect = _interopRequireDefault(require("../jsutils/defineInspect.js"));

var _suggestionList = _interopRequireDefault(require("../jsutils/suggestionList.js"));

var _GraphQLError = require("../error/GraphQLError.js");

var _kinds = require("../language/kinds.js");

var _printer = require("../language/printer.js");

var _valueFromASTUntyped = require("../utilities/valueFromASTUntyped.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

function isType(type) {
  return isScalarType(type) || isObjectType(type) || isInterfaceType(type) || isUnionType(type) || isEnumType(type) || isInputObjectType(type) || isListType(type) || isNonNullType(type);
}

function assertType(type) {
  if (!isType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL type."));
  }

  return type;
}
/**
 * There are predicates for each kind of GraphQL type.
 */


// eslint-disable-next-line no-redeclare
function isScalarType(type) {
  return (0, _instanceOf.default)(type, GraphQLScalarType);
}

function assertScalarType(type) {
  if (!isScalarType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL Scalar type."));
  }

  return type;
}

// eslint-disable-next-line no-redeclare
function isObjectType(type) {
  return (0, _instanceOf.default)(type, GraphQLObjectType);
}

function assertObjectType(type) {
  if (!isObjectType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL Object type."));
  }

  return type;
}

// eslint-disable-next-line no-redeclare
function isInterfaceType(type) {
  return (0, _instanceOf.default)(type, GraphQLInterfaceType);
}

function assertInterfaceType(type) {
  if (!isInterfaceType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL Interface type."));
  }

  return type;
}

// eslint-disable-next-line no-redeclare
function isUnionType(type) {
  return (0, _instanceOf.default)(type, GraphQLUnionType);
}

function assertUnionType(type) {
  if (!isUnionType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL Union type."));
  }

  return type;
}

// eslint-disable-next-line no-redeclare
function isEnumType(type) {
  return (0, _instanceOf.default)(type, GraphQLEnumType);
}

function assertEnumType(type) {
  if (!isEnumType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL Enum type."));
  }

  return type;
}

// eslint-disable-next-line no-redeclare
function isInputObjectType(type) {
  return (0, _instanceOf.default)(type, GraphQLInputObjectType);
}

function assertInputObjectType(type) {
  if (!isInputObjectType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL Input Object type."));
  }

  return type;
}

// eslint-disable-next-line no-redeclare
function isListType(type) {
  return (0, _instanceOf.default)(type, GraphQLList);
}

function assertListType(type) {
  if (!isListType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL List type."));
  }

  return type;
}

// eslint-disable-next-line no-redeclare
function isNonNullType(type) {
  return (0, _instanceOf.default)(type, GraphQLNonNull);
}

function assertNonNullType(type) {
  if (!isNonNullType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL Non-Null type."));
  }

  return type;
}
/**
 * These types may be used as input types for arguments and directives.
 */


function isInputType(type) {
  return isScalarType(type) || isEnumType(type) || isInputObjectType(type) || isWrappingType(type) && isInputType(type.ofType);
}

function assertInputType(type) {
  if (!isInputType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL input type."));
  }

  return type;
}
/**
 * These types may be used as output types as the result of fields.
 */


function isOutputType(type) {
  return isScalarType(type) || isObjectType(type) || isInterfaceType(type) || isUnionType(type) || isEnumType(type) || isWrappingType(type) && isOutputType(type.ofType);
}

function assertOutputType(type) {
  if (!isOutputType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL output type."));
  }

  return type;
}
/**
 * These types may describe types which may be leaf values.
 */


function isLeafType(type) {
  return isScalarType(type) || isEnumType(type);
}

function assertLeafType(type) {
  if (!isLeafType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL leaf type."));
  }

  return type;
}
/**
 * These types may describe the parent context of a selection set.
 */


function isCompositeType(type) {
  return isObjectType(type) || isInterfaceType(type) || isUnionType(type);
}

function assertCompositeType(type) {
  if (!isCompositeType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL composite type."));
  }

  return type;
}
/**
 * These types may describe the parent context of a selection set.
 */


function isAbstractType(type) {
  return isInterfaceType(type) || isUnionType(type);
}

function assertAbstractType(type) {
  if (!isAbstractType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL abstract type."));
  }

  return type;
}
/**
 * List Type Wrapper
 *
 * A list is a wrapping type which points to another type.
 * Lists are often created within the context of defining the fields of
 * an object type.
 *
 * Example:
 *
 *     const PersonType = new GraphQLObjectType({
 *       name: 'Person',
 *       fields: () => ({
 *         parents: { type: new GraphQLList(PersonType) },
 *         children: { type: new GraphQLList(PersonType) },
 *       })
 *     })
 *
 */
// FIXME: workaround to fix issue with Babel parser

/* ::
declare class GraphQLList<+T: GraphQLType> {
  +ofType: T;
  static <T>(ofType: T): GraphQLList<T>;
  // Note: constructors cannot be used for covariant types. Drop the "new".
  constructor(ofType: GraphQLType): void;
}
*/


function GraphQLList(ofType) {
  // istanbul ignore else (to be removed in v16.0.0)
  if (this instanceof GraphQLList) {
    this.ofType = assertType(ofType);
  } else {
    return new GraphQLList(ofType);
  }
} // Need to cast through any to alter the prototype.


GraphQLList.prototype.toString = function toString() {
  return '[' + String(this.ofType) + ']';
};

GraphQLList.prototype.toJSON = function toJSON() {
  return this.toString();
};

Object.defineProperty(GraphQLList.prototype, _symbols.SYMBOL_TO_STRING_TAG, {
  get: function get() {
    return 'GraphQLList';
  }
}); // Print a simplified form when appearing in `inspect` and `util.inspect`.

(0, _defineInspect.default)(GraphQLList);
/**
 * Non-Null Type Wrapper
 *
 * A non-null is a wrapping type which points to another type.
 * Non-null types enforce that their values are never null and can ensure
 * an error is raised if this ever occurs during a request. It is useful for
 * fields which you can make a strong guarantee on non-nullability, for example
 * usually the id field of a database row will never be null.
 *
 * Example:
 *
 *     const RowType = new GraphQLObjectType({
 *       name: 'Row',
 *       fields: () => ({
 *         id: { type: new GraphQLNonNull(GraphQLString) },
 *       })
 *     })
 *
 * Note: the enforcement of non-nullability occurs within the executor.
 */
// FIXME: workaround to fix issue with Babel parser

/* ::
declare class GraphQLNonNull<+T: GraphQLNullableType> {
  +ofType: T;
  static <T>(ofType: T): GraphQLNonNull<T>;
  // Note: constructors cannot be used for covariant types. Drop the "new".
  constructor(ofType: GraphQLType): void;
}
*/

function GraphQLNonNull(ofType) {
  // istanbul ignore else (to be removed in v16.0.0)
  if (this instanceof GraphQLNonNull) {
    this.ofType = assertNullableType(ofType);
  } else {
    return new GraphQLNonNull(ofType);
  }
} // Need to cast through any to alter the prototype.


GraphQLNonNull.prototype.toString = function toString() {
  return String(this.ofType) + '!';
};

GraphQLNonNull.prototype.toJSON = function toJSON() {
  return this.toString();
};

Object.defineProperty(GraphQLNonNull.prototype, _symbols.SYMBOL_TO_STRING_TAG, {
  get: function get() {
    return 'GraphQLNonNull';
  }
}); // Print a simplified form when appearing in `inspect` and `util.inspect`.

(0, _defineInspect.default)(GraphQLNonNull);
/**
 * These types wrap and modify other types
 */

function isWrappingType(type) {
  return isListType(type) || isNonNullType(type);
}

function assertWrappingType(type) {
  if (!isWrappingType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL wrapping type."));
  }

  return type;
}
/**
 * These types can all accept null as a value.
 */


function isNullableType(type) {
  return isType(type) && !isNonNullType(type);
}

function assertNullableType(type) {
  if (!isNullableType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL nullable type."));
  }

  return type;
}
/* eslint-disable no-redeclare */


function getNullableType(type) {
  /* eslint-enable no-redeclare */
  if (type) {
    return isNonNullType(type) ? type.ofType : type;
  }
}
/**
 * These named types do not include modifiers like List or NonNull.
 */


function isNamedType(type) {
  return isScalarType(type) || isObjectType(type) || isInterfaceType(type) || isUnionType(type) || isEnumType(type) || isInputObjectType(type);
}

function assertNamedType(type) {
  if (!isNamedType(type)) {
    throw new Error("Expected ".concat((0, _inspect.default)(type), " to be a GraphQL named type."));
  }

  return type;
}
/* eslint-disable no-redeclare */


function getNamedType(type) {
  /* eslint-enable no-redeclare */
  if (type) {
    var unwrappedType = type;

    while (isWrappingType(unwrappedType)) {
      unwrappedType = unwrappedType.ofType;
    }

    return unwrappedType;
  }
}
/**
 * Used while defining GraphQL types to allow for circular references in
 * otherwise immutable type definitions.
 */


function resolveThunk(thunk) {
  // $FlowFixMe[incompatible-use]
  return typeof thunk === 'function' ? thunk() : thunk;
}

function undefineIfEmpty(arr) {
  return arr && arr.length > 0 ? arr : undefined;
}
/**
 * Scalar Type Definition
 *
 * The leaf values of any request and input values to arguments are
 * Scalars (or Enums) and are defined with a name and a series of functions
 * used to parse input from ast or variables and to ensure validity.
 *
 * If a type's serialize function does not return a value (i.e. it returns
 * `undefined`) then an error will be raised and a `null` value will be returned
 * in the response. If the serialize function returns `null`, then no error will
 * be included in the response.
 *
 * Example:
 *
 *     const OddType = new GraphQLScalarType({
 *       name: 'Odd',
 *       serialize(value) {
 *         if (value % 2 === 1) {
 *           return value;
 *         }
 *       }
 *     });
 *
 */


var GraphQLScalarType = /*#__PURE__*/function () {
  function GraphQLScalarType(config) {
    var _config$parseValue, _config$serialize, _config$parseLiteral;

    var parseValue = (_config$parseValue = config.parseValue) !== null && _config$parseValue !== void 0 ? _config$parseValue : _identityFunc.default;
    this.name = config.name;
    this.description = config.description;
    this.specifiedByUrl = config.specifiedByUrl;
    this.serialize = (_config$serialize = config.serialize) !== null && _config$serialize !== void 0 ? _config$serialize : _identityFunc.default;
    this.parseValue = parseValue;
    this.parseLiteral = (_config$parseLiteral = config.parseLiteral) !== null && _config$parseLiteral !== void 0 ? _config$parseLiteral : function (node, variables) {
      return parseValue((0, _valueFromASTUntyped.valueFromASTUntyped)(node, variables));
    };
    this.extensions = config.extensions && (0, _toObjMap.default)(config.extensions);
    this.astNode = config.astNode;
    this.extensionASTNodes = undefineIfEmpty(config.extensionASTNodes);
    typeof config.name === 'string' || (0, _devAssert.default)(0, 'Must provide name.');
    config.specifiedByUrl == null || typeof config.specifiedByUrl === 'string' || (0, _devAssert.default)(0, "".concat(this.name, " must provide \"specifiedByUrl\" as a string, ") + "but got: ".concat((0, _inspect.default)(config.specifiedByUrl), "."));
    config.serialize == null || typeof config.serialize === 'function' || (0, _devAssert.default)(0, "".concat(this.name, " must provide \"serialize\" function. If this custom Scalar is also used as an input type, ensure \"parseValue\" and \"parseLiteral\" functions are also provided."));

    if (config.parseLiteral) {
      typeof config.parseValue === 'function' && typeof config.parseLiteral === 'function' || (0, _devAssert.default)(0, "".concat(this.name, " must provide both \"parseValue\" and \"parseLiteral\" functions."));
    }
  }

  var _proto = GraphQLScalarType.prototype;

  _proto.toConfig = function toConfig() {
    var _this$extensionASTNod;

    return {
      name: this.name,
      description: this.description,
      specifiedByUrl: this.specifiedByUrl,
      serialize: this.serialize,
      parseValue: this.parseValue,
      parseLiteral: this.parseLiteral,
      extensions: this.extensions,
      astNode: this.astNode,
      extensionASTNodes: (_this$extensionASTNod = this.extensionASTNodes) !== null && _this$extensionASTNod !== void 0 ? _this$extensionASTNod : []
    };
  };

  _proto.toString = function toString() {
    return this.name;
  };

  _proto.toJSON = function toJSON() {
    return this.toString();
  } // $FlowFixMe[unsupported-syntax] Flow doesn't support computed properties yet
  ;

  _createClass(GraphQLScalarType, [{
    key: _symbols.SYMBOL_TO_STRING_TAG,
    get: function get() {
      return 'GraphQLScalarType';
    }
  }]);

  return GraphQLScalarType;
}(); // Print a simplified form when appearing in `inspect` and `util.inspect`.


exports.GraphQLScalarType = GraphQLScalarType;
(0, _defineInspect.default)(GraphQLScalarType);

/**
 * Object Type Definition
 *
 * Almost all of the GraphQL types you define will be object types. Object types
 * have a name, but most importantly describe their fields.
 *
 * Example:
 *
 *     const AddressType = new GraphQLObjectType({
 *       name: 'Address',
 *       fields: {
 *         street: { type: GraphQLString },
 *         number: { type: GraphQLInt },
 *         formatted: {
 *           type: GraphQLString,
 *           resolve(obj) {
 *             return obj.number + ' ' + obj.street
 *           }
 *         }
 *       }
 *     });
 *
 * When two types need to refer to each other, or a type needs to refer to
 * itself in a field, you can use a function expression (aka a closure or a
 * thunk) to supply the fields lazily.
 *
 * Example:
 *
 *     const PersonType = new GraphQLObjectType({
 *       name: 'Person',
 *       fields: () => ({
 *         name: { type: GraphQLString },
 *         bestFriend: { type: PersonType },
 *       })
 *     });
 *
 */
var GraphQLObjectType = /*#__PURE__*/function () {
  function GraphQLObjectType(config) {
    this.name = config.name;
    this.description = config.description;
    this.isTypeOf = config.isTypeOf;
    this.extensions = config.extensions && (0, _toObjMap.default)(config.extensions);
    this.astNode = config.astNode;
    this.extensionASTNodes = undefineIfEmpty(config.extensionASTNodes);
    this._fields = defineFieldMap.bind(undefined, config);
    this._interfaces = defineInterfaces.bind(undefined, config);
    typeof config.name === 'string' || (0, _devAssert.default)(0, 'Must provide name.');
    config.isTypeOf == null || typeof config.isTypeOf === 'function' || (0, _devAssert.default)(0, "".concat(this.name, " must provide \"isTypeOf\" as a function, ") + "but got: ".concat((0, _inspect.default)(config.isTypeOf), "."));
  }

  var _proto2 = GraphQLObjectType.prototype;

  _proto2.getFields = function getFields() {
    if (typeof this._fields === 'function') {
      this._fields = this._fields();
    }

    return this._fields;
  };

  _proto2.getInterfaces = function getInterfaces() {
    if (typeof this._interfaces === 'function') {
      this._interfaces = this._interfaces();
    }

    return this._interfaces;
  };

  _proto2.toConfig = function toConfig() {
    return {
      name: this.name,
      description: this.description,
      interfaces: this.getInterfaces(),
      fields: fieldsToFieldsConfig(this.getFields()),
      isTypeOf: this.isTypeOf,
      extensions: this.extensions,
      astNode: this.astNode,
      extensionASTNodes: this.extensionASTNodes || []
    };
  };

  _proto2.toString = function toString() {
    return this.name;
  };

  _proto2.toJSON = function toJSON() {
    return this.toString();
  } // $FlowFixMe[unsupported-syntax] Flow doesn't support computed properties yet
  ;

  _createClass(GraphQLObjectType, [{
    key: _symbols.SYMBOL_TO_STRING_TAG,
    get: function get() {
      return 'GraphQLObjectType';
    }
  }]);

  return GraphQLObjectType;
}(); // Print a simplified form when appearing in `inspect` and `util.inspect`.


exports.GraphQLObjectType = GraphQLObjectType;
(0, _defineInspect.default)(GraphQLObjectType);

function defineInterfaces(config) {
  var _resolveThunk;

  var interfaces = (_resolveThunk = resolveThunk(config.interfaces)) !== null && _resolveThunk !== void 0 ? _resolveThunk : [];
  Array.isArray(interfaces) || (0, _devAssert.default)(0, "".concat(config.name, " interfaces must be an Array or a function which returns an Array."));
  return interfaces;
}

function defineFieldMap(config) {
  var fieldMap = resolveThunk(config.fields);
  isPlainObj(fieldMap) || (0, _devAssert.default)(0, "".concat(config.name, " fields must be an object with field names as keys or a function which returns such an object."));
  return (0, _mapValue.default)(fieldMap, function (fieldConfig, fieldName) {
    var _fieldConfig$args;

    isPlainObj(fieldConfig) || (0, _devAssert.default)(0, "".concat(config.name, ".").concat(fieldName, " field config must be an object."));
    !('isDeprecated' in fieldConfig) || (0, _devAssert.default)(0, "".concat(config.name, ".").concat(fieldName, " should provide \"deprecationReason\" instead of \"isDeprecated\"."));
    fieldConfig.resolve == null || typeof fieldConfig.resolve === 'function' || (0, _devAssert.default)(0, "".concat(config.name, ".").concat(fieldName, " field resolver must be a function if ") + "provided, but got: ".concat((0, _inspect.default)(fieldConfig.resolve), "."));
    var argsConfig = (_fieldConfig$args = fieldConfig.args) !== null && _fieldConfig$args !== void 0 ? _fieldConfig$args : {};
    isPlainObj(argsConfig) || (0, _devAssert.default)(0, "".concat(config.name, ".").concat(fieldName, " args must be an object with argument names as keys."));
    var args = (0, _objectEntries.default)(argsConfig).map(function (_ref) {
      var argName = _ref[0],
          argConfig = _ref[1];
      return {
        name: argName,
        description: argConfig.description,
        type: argConfig.type,
        defaultValue: argConfig.defaultValue,
        deprecationReason: argConfig.deprecationReason,
        extensions: argConfig.extensions && (0, _toObjMap.default)(argConfig.extensions),
        astNode: argConfig.astNode
      };
    });
    return {
      name: fieldName,
      description: fieldConfig.description,
      type: fieldConfig.type,
      args: args,
      resolve: fieldConfig.resolve,
      subscribe: fieldConfig.subscribe,
      isDeprecated: fieldConfig.deprecationReason != null,
      deprecationReason: fieldConfig.deprecationReason,
      extensions: fieldConfig.extensions && (0, _toObjMap.default)(fieldConfig.extensions),
      astNode: fieldConfig.astNode
    };
  });
}

function isPlainObj(obj) {
  return (0, _isObjectLike.default)(obj) && !Array.isArray(obj);
}

function fieldsToFieldsConfig(fields) {
  return (0, _mapValue.default)(fields, function (field) {
    return {
      description: field.description,
      type: field.type,
      args: argsToArgsConfig(field.args),
      resolve: field.resolve,
      subscribe: field.subscribe,
      deprecationReason: field.deprecationReason,
      extensions: field.extensions,
      astNode: field.astNode
    };
  });
}
/**
 * @internal
 */


function argsToArgsConfig(args) {
  return (0, _keyValMap.default)(args, function (arg) {
    return arg.name;
  }, function (arg) {
    return {
      description: arg.description,
      type: arg.type,
      defaultValue: arg.defaultValue,
      deprecationReason: arg.deprecationReason,
      extensions: arg.extensions,
      astNode: arg.astNode
    };
  });
}

function isRequiredArgument(arg) {
  return isNonNullType(arg.type) && arg.defaultValue === undefined;
}

/**
 * Interface Type Definition
 *
 * When a field can return one of a heterogeneous set of types, a Interface type
 * is used to describe what types are possible, what fields are in common across
 * all types, as well as a function to determine which type is actually used
 * when the field is resolved.
 *
 * Example:
 *
 *     const EntityType = new GraphQLInterfaceType({
 *       name: 'Entity',
 *       fields: {
 *         name: { type: GraphQLString }
 *       }
 *     });
 *
 */
var GraphQLInterfaceType = /*#__PURE__*/function () {
  function GraphQLInterfaceType(config) {
    this.name = config.name;
    this.description = config.description;
    this.resolveType = config.resolveType;
    this.extensions = config.extensions && (0, _toObjMap.default)(config.extensions);
    this.astNode = config.astNode;
    this.extensionASTNodes = undefineIfEmpty(config.extensionASTNodes);
    this._fields = defineFieldMap.bind(undefined, config);
    this._interfaces = defineInterfaces.bind(undefined, config);
    typeof config.name === 'string' || (0, _devAssert.default)(0, 'Must provide name.');
    config.resolveType == null || typeof config.resolveType === 'function' || (0, _devAssert.default)(0, "".concat(this.name, " must provide \"resolveType\" as a function, ") + "but got: ".concat((0, _inspect.default)(config.resolveType), "."));
  }

  var _proto3 = GraphQLInterfaceType.prototype;

  _proto3.getFields = function getFields() {
    if (typeof this._fields === 'function') {
      this._fields = this._fields();
    }

    return this._fields;
  };

  _proto3.getInterfaces = function getInterfaces() {
    if (typeof this._interfaces === 'function') {
      this._interfaces = this._interfaces();
    }

    return this._interfaces;
  };

  _proto3.toConfig = function toConfig() {
    var _this$extensionASTNod2;

    return {
      name: this.name,
      description: this.description,
      interfaces: this.getInterfaces(),
      fields: fieldsToFieldsConfig(this.getFields()),
      resolveType: this.resolveType,
      extensions: this.extensions,
      astNode: this.astNode,
      extensionASTNodes: (_this$extensionASTNod2 = this.extensionASTNodes) !== null && _this$extensionASTNod2 !== void 0 ? _this$extensionASTNod2 : []
    };
  };

  _proto3.toString = function toString() {
    return this.name;
  };

  _proto3.toJSON = function toJSON() {
    return this.toString();
  } // $FlowFixMe[unsupported-syntax] Flow doesn't support computed properties yet
  ;

  _createClass(GraphQLInterfaceType, [{
    key: _symbols.SYMBOL_TO_STRING_TAG,
    get: function get() {
      return 'GraphQLInterfaceType';
    }
  }]);

  return GraphQLInterfaceType;
}(); // Print a simplified form when appearing in `inspect` and `util.inspect`.


exports.GraphQLInterfaceType = GraphQLInterfaceType;
(0, _defineInspect.default)(GraphQLInterfaceType);

/**
 * Union Type Definition
 *
 * When a field can return one of a heterogeneous set of types, a Union type
 * is used to describe what types are possible as well as providing a function
 * to determine which type is actually used when the field is resolved.
 *
 * Example:
 *
 *     const PetType = new GraphQLUnionType({
 *       name: 'Pet',
 *       types: [ DogType, CatType ],
 *       resolveType(value) {
 *         if (value instanceof Dog) {
 *           return DogType;
 *         }
 *         if (value instanceof Cat) {
 *           return CatType;
 *         }
 *       }
 *     });
 *
 */
var GraphQLUnionType = /*#__PURE__*/function () {
  function GraphQLUnionType(config) {
    this.name = config.name;
    this.description = config.description;
    this.resolveType = config.resolveType;
    this.extensions = config.extensions && (0, _toObjMap.default)(config.extensions);
    this.astNode = config.astNode;
    this.extensionASTNodes = undefineIfEmpty(config.extensionASTNodes);
    this._types = defineTypes.bind(undefined, config);
    typeof config.name === 'string' || (0, _devAssert.default)(0, 'Must provide name.');
    config.resolveType == null || typeof config.resolveType === 'function' || (0, _devAssert.default)(0, "".concat(this.name, " must provide \"resolveType\" as a function, ") + "but got: ".concat((0, _inspect.default)(config.resolveType), "."));
  }

  var _proto4 = GraphQLUnionType.prototype;

  _proto4.getTypes = function getTypes() {
    if (typeof this._types === 'function') {
      this._types = this._types();
    }

    return this._types;
  };

  _proto4.toConfig = function toConfig() {
    var _this$extensionASTNod3;

    return {
      name: this.name,
      description: this.description,
      types: this.getTypes(),
      resolveType: this.resolveType,
      extensions: this.extensions,
      astNode: this.astNode,
      extensionASTNodes: (_this$extensionASTNod3 = this.extensionASTNodes) !== null && _this$extensionASTNod3 !== void 0 ? _this$extensionASTNod3 : []
    };
  };

  _proto4.toString = function toString() {
    return this.name;
  };

  _proto4.toJSON = function toJSON() {
    return this.toString();
  } // $FlowFixMe[unsupported-syntax] Flow doesn't support computed properties yet
  ;

  _createClass(GraphQLUnionType, [{
    key: _symbols.SYMBOL_TO_STRING_TAG,
    get: function get() {
      return 'GraphQLUnionType';
    }
  }]);

  return GraphQLUnionType;
}(); // Print a simplified form when appearing in `inspect` and `util.inspect`.


exports.GraphQLUnionType = GraphQLUnionType;
(0, _defineInspect.default)(GraphQLUnionType);

function defineTypes(config) {
  var types = resolveThunk(config.types);
  Array.isArray(types) || (0, _devAssert.default)(0, "Must provide Array of types or a function which returns such an array for Union ".concat(config.name, "."));
  return types;
}

/**
 * Enum Type Definition
 *
 * Some leaf values of requests and input values are Enums. GraphQL serializes
 * Enum values as strings, however internally Enums can be represented by any
 * kind of type, often integers.
 *
 * Example:
 *
 *     const RGBType = new GraphQLEnumType({
 *       name: 'RGB',
 *       values: {
 *         RED: { value: 0 },
 *         GREEN: { value: 1 },
 *         BLUE: { value: 2 }
 *       }
 *     });
 *
 * Note: If a value is not provided in a definition, the name of the enum value
 * will be used as its internal value.
 */
var GraphQLEnumType
/* <T> */
= /*#__PURE__*/function () {
  function GraphQLEnumType(config) {
    this.name = config.name;
    this.description = config.description;
    this.extensions = config.extensions && (0, _toObjMap.default)(config.extensions);
    this.astNode = config.astNode;
    this.extensionASTNodes = undefineIfEmpty(config.extensionASTNodes);
    this._values = defineEnumValues(this.name, config.values);
    this._valueLookup = new Map(this._values.map(function (enumValue) {
      return [enumValue.value, enumValue];
    }));
    this._nameLookup = (0, _keyMap.default)(this._values, function (value) {
      return value.name;
    });
    typeof config.name === 'string' || (0, _devAssert.default)(0, 'Must provide name.');
  }

  var _proto5 = GraphQLEnumType.prototype;

  _proto5.getValues = function getValues() {
    return this._values;
  };

  _proto5.getValue = function getValue(name) {
    return this._nameLookup[name];
  };

  _proto5.serialize = function serialize(outputValue) {
    var enumValue = this._valueLookup.get(outputValue);

    if (enumValue === undefined) {
      throw new _GraphQLError.GraphQLError("Enum \"".concat(this.name, "\" cannot represent value: ").concat((0, _inspect.default)(outputValue)));
    }

    return enumValue.name;
  };

  _proto5.parseValue = function parseValue(inputValue)
  /* T */
  {
    if (typeof inputValue !== 'string') {
      var valueStr = (0, _inspect.default)(inputValue);
      throw new _GraphQLError.GraphQLError("Enum \"".concat(this.name, "\" cannot represent non-string value: ").concat(valueStr, ".") + didYouMeanEnumValue(this, valueStr));
    }

    var enumValue = this.getValue(inputValue);

    if (enumValue == null) {
      throw new _GraphQLError.GraphQLError("Value \"".concat(inputValue, "\" does not exist in \"").concat(this.name, "\" enum.") + didYouMeanEnumValue(this, inputValue));
    }

    return enumValue.value;
  };

  _proto5.parseLiteral = function parseLiteral(valueNode, _variables)
  /* T */
  {
    // Note: variables will be resolved to a value before calling this function.
    if (valueNode.kind !== _kinds.Kind.ENUM) {
      var valueStr = (0, _printer.print)(valueNode);
      throw new _GraphQLError.GraphQLError("Enum \"".concat(this.name, "\" cannot represent non-enum value: ").concat(valueStr, ".") + didYouMeanEnumValue(this, valueStr), valueNode);
    }

    var enumValue = this.getValue(valueNode.value);

    if (enumValue == null) {
      var _valueStr = (0, _printer.print)(valueNode);

      throw new _GraphQLError.GraphQLError("Value \"".concat(_valueStr, "\" does not exist in \"").concat(this.name, "\" enum.") + didYouMeanEnumValue(this, _valueStr), valueNode);
    }

    return enumValue.value;
  };

  _proto5.toConfig = function toConfig() {
    var _this$extensionASTNod4;

    var values = (0, _keyValMap.default)(this.getValues(), function (value) {
      return value.name;
    }, function (value) {
      return {
        description: value.description,
        value: value.value,
        deprecationReason: value.deprecationReason,
        extensions: value.extensions,
        astNode: value.astNode
      };
    });
    return {
      name: this.name,
      description: this.description,
      values: values,
      extensions: this.extensions,
      astNode: this.astNode,
      extensionASTNodes: (_this$extensionASTNod4 = this.extensionASTNodes) !== null && _this$extensionASTNod4 !== void 0 ? _this$extensionASTNod4 : []
    };
  };

  _proto5.toString = function toString() {
    return this.name;
  };

  _proto5.toJSON = function toJSON() {
    return this.toString();
  } // $FlowFixMe[unsupported-syntax] Flow doesn't support computed properties yet
  ;

  _createClass(GraphQLEnumType, [{
    key: _symbols.SYMBOL_TO_STRING_TAG,
    get: function get() {
      return 'GraphQLEnumType';
    }
  }]);

  return GraphQLEnumType;
}(); // Print a simplified form when appearing in `inspect` and `util.inspect`.


exports.GraphQLEnumType = GraphQLEnumType;
(0, _defineInspect.default)(GraphQLEnumType);

function didYouMeanEnumValue(enumType, unknownValueStr) {
  var allNames = enumType.getValues().map(function (value) {
    return value.name;
  });
  var suggestedValues = (0, _suggestionList.default)(unknownValueStr, allNames);
  return (0, _didYouMean.default)('the enum value', suggestedValues);
}

function defineEnumValues(typeName, valueMap) {
  isPlainObj(valueMap) || (0, _devAssert.default)(0, "".concat(typeName, " values must be an object with value names as keys."));
  return (0, _objectEntries.default)(valueMap).map(function (_ref2) {
    var valueName = _ref2[0],
        valueConfig = _ref2[1];
    isPlainObj(valueConfig) || (0, _devAssert.default)(0, "".concat(typeName, ".").concat(valueName, " must refer to an object with a \"value\" key ") + "representing an internal value but got: ".concat((0, _inspect.default)(valueConfig), "."));
    !('isDeprecated' in valueConfig) || (0, _devAssert.default)(0, "".concat(typeName, ".").concat(valueName, " should provide \"deprecationReason\" instead of \"isDeprecated\"."));
    return {
      name: valueName,
      description: valueConfig.description,
      value: valueConfig.value !== undefined ? valueConfig.value : valueName,
      isDeprecated: valueConfig.deprecationReason != null,
      deprecationReason: valueConfig.deprecationReason,
      extensions: valueConfig.extensions && (0, _toObjMap.default)(valueConfig.extensions),
      astNode: valueConfig.astNode
    };
  });
}

/**
 * Input Object Type Definition
 *
 * An input object defines a structured collection of fields which may be
 * supplied to a field argument.
 *
 * Using `NonNull` will ensure that a value must be provided by the query
 *
 * Example:
 *
 *     const GeoPoint = new GraphQLInputObjectType({
 *       name: 'GeoPoint',
 *       fields: {
 *         lat: { type: new GraphQLNonNull(GraphQLFloat) },
 *         lon: { type: new GraphQLNonNull(GraphQLFloat) },
 *         alt: { type: GraphQLFloat, defaultValue: 0 },
 *       }
 *     });
 *
 */
var GraphQLInputObjectType = /*#__PURE__*/function () {
  function GraphQLInputObjectType(config) {
    this.name = config.name;
    this.description = config.description;
    this.extensions = config.extensions && (0, _toObjMap.default)(config.extensions);
    this.astNode = config.astNode;
    this.extensionASTNodes = undefineIfEmpty(config.extensionASTNodes);
    this._fields = defineInputFieldMap.bind(undefined, config);
    typeof config.name === 'string' || (0, _devAssert.default)(0, 'Must provide name.');
  }

  var _proto6 = GraphQLInputObjectType.prototype;

  _proto6.getFields = function getFields() {
    if (typeof this._fields === 'function') {
      this._fields = this._fields();
    }

    return this._fields;
  };

  _proto6.toConfig = function toConfig() {
    var _this$extensionASTNod5;

    var fields = (0, _mapValue.default)(this.getFields(), function (field) {
      return {
        description: field.description,
        type: field.type,
        defaultValue: field.defaultValue,
        extensions: field.extensions,
        astNode: field.astNode
      };
    });
    return {
      name: this.name,
      description: this.description,
      fields: fields,
      extensions: this.extensions,
      astNode: this.astNode,
      extensionASTNodes: (_this$extensionASTNod5 = this.extensionASTNodes) !== null && _this$extensionASTNod5 !== void 0 ? _this$extensionASTNod5 : []
    };
  };

  _proto6.toString = function toString() {
    return this.name;
  };

  _proto6.toJSON = function toJSON() {
    return this.toString();
  } // $FlowFixMe[unsupported-syntax] Flow doesn't support computed properties yet
  ;

  _createClass(GraphQLInputObjectType, [{
    key: _symbols.SYMBOL_TO_STRING_TAG,
    get: function get() {
      return 'GraphQLInputObjectType';
    }
  }]);

  return GraphQLInputObjectType;
}(); // Print a simplified form when appearing in `inspect` and `util.inspect`.


exports.GraphQLInputObjectType = GraphQLInputObjectType;
(0, _defineInspect.default)(GraphQLInputObjectType);

function defineInputFieldMap(config) {
  var fieldMap = resolveThunk(config.fields);
  isPlainObj(fieldMap) || (0, _devAssert.default)(0, "".concat(config.name, " fields must be an object with field names as keys or a function which returns such an object."));
  return (0, _mapValue.default)(fieldMap, function (fieldConfig, fieldName) {
    !('resolve' in fieldConfig) || (0, _devAssert.default)(0, "".concat(config.name, ".").concat(fieldName, " field has a resolve property, but Input Types cannot define resolvers."));
    return {
      name: fieldName,
      description: fieldConfig.description,
      type: fieldConfig.type,
      defaultValue: fieldConfig.defaultValue,
      deprecationReason: fieldConfig.deprecationReason,
      extensions: fieldConfig.extensions && (0, _toObjMap.default)(fieldConfig.extensions),
      astNode: fieldConfig.astNode
    };
  });
}

function isRequiredInputField(field) {
  return isNonNullType(field.type) && field.defaultValue === undefined;
}

},{"../error/GraphQLError.js":18,"../jsutils/defineInspect.js":29,"../jsutils/devAssert.js":30,"../jsutils/didYouMean.js":31,"../jsutils/identityFunc.js":32,"../jsutils/inspect.js":33,"../jsutils/instanceOf.js":34,"../jsutils/isObjectLike.js":37,"../jsutils/keyMap.js":39,"../jsutils/keyValMap.js":40,"../jsutils/mapValue.js":41,"../jsutils/suggestionList.js":49,"../jsutils/toObjMap.js":50,"../language/kinds.js":55,"../language/printer.js":61,"../polyfills/objectEntries.js":69,"../polyfills/symbols.js":71,"../utilities/valueFromASTUntyped.js":104}],76:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isDirective = isDirective;
exports.assertDirective = assertDirective;
exports.isSpecifiedDirective = isSpecifiedDirective;
exports.specifiedDirectives = exports.GraphQLSpecifiedByDirective = exports.GraphQLDeprecatedDirective = exports.DEFAULT_DEPRECATION_REASON = exports.GraphQLSkipDirective = exports.GraphQLIncludeDirective = exports.GraphQLDirective = void 0;

var _objectEntries = _interopRequireDefault(require("../polyfills/objectEntries.js"));

var _symbols = require("../polyfills/symbols.js");

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _toObjMap = _interopRequireDefault(require("../jsutils/toObjMap.js"));

var _devAssert = _interopRequireDefault(require("../jsutils/devAssert.js"));

var _instanceOf = _interopRequireDefault(require("../jsutils/instanceOf.js"));

var _isObjectLike = _interopRequireDefault(require("../jsutils/isObjectLike.js"));

var _defineInspect = _interopRequireDefault(require("../jsutils/defineInspect.js"));

var _directiveLocation = require("../language/directiveLocation.js");

var _scalars = require("./scalars.js");

var _definition = require("./definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

// eslint-disable-next-line no-redeclare
function isDirective(directive) {
  return (0, _instanceOf.default)(directive, GraphQLDirective);
}

function assertDirective(directive) {
  if (!isDirective(directive)) {
    throw new Error("Expected ".concat((0, _inspect.default)(directive), " to be a GraphQL directive."));
  }

  return directive;
}
/**
 * Directives are used by the GraphQL runtime as a way of modifying execution
 * behavior. Type system creators will usually not create these directly.
 */


var GraphQLDirective = /*#__PURE__*/function () {
  function GraphQLDirective(config) {
    var _config$isRepeatable, _config$args;

    this.name = config.name;
    this.description = config.description;
    this.locations = config.locations;
    this.isRepeatable = (_config$isRepeatable = config.isRepeatable) !== null && _config$isRepeatable !== void 0 ? _config$isRepeatable : false;
    this.extensions = config.extensions && (0, _toObjMap.default)(config.extensions);
    this.astNode = config.astNode;
    config.name || (0, _devAssert.default)(0, 'Directive must be named.');
    Array.isArray(config.locations) || (0, _devAssert.default)(0, "@".concat(config.name, " locations must be an Array."));
    var args = (_config$args = config.args) !== null && _config$args !== void 0 ? _config$args : {};
    (0, _isObjectLike.default)(args) && !Array.isArray(args) || (0, _devAssert.default)(0, "@".concat(config.name, " args must be an object with argument names as keys."));
    this.args = (0, _objectEntries.default)(args).map(function (_ref) {
      var argName = _ref[0],
          argConfig = _ref[1];
      return {
        name: argName,
        description: argConfig.description,
        type: argConfig.type,
        defaultValue: argConfig.defaultValue,
        deprecationReason: argConfig.deprecationReason,
        extensions: argConfig.extensions && (0, _toObjMap.default)(argConfig.extensions),
        astNode: argConfig.astNode
      };
    });
  }

  var _proto = GraphQLDirective.prototype;

  _proto.toConfig = function toConfig() {
    return {
      name: this.name,
      description: this.description,
      locations: this.locations,
      args: (0, _definition.argsToArgsConfig)(this.args),
      isRepeatable: this.isRepeatable,
      extensions: this.extensions,
      astNode: this.astNode
    };
  };

  _proto.toString = function toString() {
    return '@' + this.name;
  };

  _proto.toJSON = function toJSON() {
    return this.toString();
  } // $FlowFixMe[unsupported-syntax] Flow doesn't support computed properties yet
  ;

  _createClass(GraphQLDirective, [{
    key: _symbols.SYMBOL_TO_STRING_TAG,
    get: function get() {
      return 'GraphQLDirective';
    }
  }]);

  return GraphQLDirective;
}(); // Print a simplified form when appearing in `inspect` and `util.inspect`.


exports.GraphQLDirective = GraphQLDirective;
(0, _defineInspect.default)(GraphQLDirective);

/**
 * Used to conditionally include fields or fragments.
 */
var GraphQLIncludeDirective = new GraphQLDirective({
  name: 'include',
  description: 'Directs the executor to include this field or fragment only when the `if` argument is true.',
  locations: [_directiveLocation.DirectiveLocation.FIELD, _directiveLocation.DirectiveLocation.FRAGMENT_SPREAD, _directiveLocation.DirectiveLocation.INLINE_FRAGMENT],
  args: {
    if: {
      type: new _definition.GraphQLNonNull(_scalars.GraphQLBoolean),
      description: 'Included when true.'
    }
  }
});
/**
 * Used to conditionally skip (exclude) fields or fragments.
 */

exports.GraphQLIncludeDirective = GraphQLIncludeDirective;
var GraphQLSkipDirective = new GraphQLDirective({
  name: 'skip',
  description: 'Directs the executor to skip this field or fragment when the `if` argument is true.',
  locations: [_directiveLocation.DirectiveLocation.FIELD, _directiveLocation.DirectiveLocation.FRAGMENT_SPREAD, _directiveLocation.DirectiveLocation.INLINE_FRAGMENT],
  args: {
    if: {
      type: new _definition.GraphQLNonNull(_scalars.GraphQLBoolean),
      description: 'Skipped when true.'
    }
  }
});
/**
 * Constant string used for default reason for a deprecation.
 */

exports.GraphQLSkipDirective = GraphQLSkipDirective;
var DEFAULT_DEPRECATION_REASON = 'No longer supported';
/**
 * Used to declare element of a GraphQL schema as deprecated.
 */

exports.DEFAULT_DEPRECATION_REASON = DEFAULT_DEPRECATION_REASON;
var GraphQLDeprecatedDirective = new GraphQLDirective({
  name: 'deprecated',
  description: 'Marks an element of a GraphQL schema as no longer supported.',
  locations: [_directiveLocation.DirectiveLocation.FIELD_DEFINITION, _directiveLocation.DirectiveLocation.ARGUMENT_DEFINITION, _directiveLocation.DirectiveLocation.INPUT_FIELD_DEFINITION, _directiveLocation.DirectiveLocation.ENUM_VALUE],
  args: {
    reason: {
      type: _scalars.GraphQLString,
      description: 'Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted using the Markdown syntax, as specified by [CommonMark](https://commonmark.org/).',
      defaultValue: DEFAULT_DEPRECATION_REASON
    }
  }
});
/**
 * Used to provide a URL for specifying the behaviour of custom scalar definitions.
 */

exports.GraphQLDeprecatedDirective = GraphQLDeprecatedDirective;
var GraphQLSpecifiedByDirective = new GraphQLDirective({
  name: 'specifiedBy',
  description: 'Exposes a URL that specifies the behaviour of this scalar.',
  locations: [_directiveLocation.DirectiveLocation.SCALAR],
  args: {
    url: {
      type: new _definition.GraphQLNonNull(_scalars.GraphQLString),
      description: 'The URL that specifies the behaviour of this scalar.'
    }
  }
});
/**
 * The full list of specified directives.
 */

exports.GraphQLSpecifiedByDirective = GraphQLSpecifiedByDirective;
var specifiedDirectives = Object.freeze([GraphQLIncludeDirective, GraphQLSkipDirective, GraphQLDeprecatedDirective, GraphQLSpecifiedByDirective]);
exports.specifiedDirectives = specifiedDirectives;

function isSpecifiedDirective(directive) {
  return specifiedDirectives.some(function (_ref2) {
    var name = _ref2.name;
    return name === directive.name;
  });
}

},{"../jsutils/defineInspect.js":29,"../jsutils/devAssert.js":30,"../jsutils/inspect.js":33,"../jsutils/instanceOf.js":34,"../jsutils/isObjectLike.js":37,"../jsutils/toObjMap.js":50,"../language/directiveLocation.js":53,"../polyfills/objectEntries.js":69,"../polyfills/symbols.js":71,"./definition.js":75,"./scalars.js":79}],77:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "isSchema", {
  enumerable: true,
  get: function get() {
    return _schema.isSchema;
  }
});
Object.defineProperty(exports, "assertSchema", {
  enumerable: true,
  get: function get() {
    return _schema.assertSchema;
  }
});
Object.defineProperty(exports, "GraphQLSchema", {
  enumerable: true,
  get: function get() {
    return _schema.GraphQLSchema;
  }
});
Object.defineProperty(exports, "isType", {
  enumerable: true,
  get: function get() {
    return _definition.isType;
  }
});
Object.defineProperty(exports, "isScalarType", {
  enumerable: true,
  get: function get() {
    return _definition.isScalarType;
  }
});
Object.defineProperty(exports, "isObjectType", {
  enumerable: true,
  get: function get() {
    return _definition.isObjectType;
  }
});
Object.defineProperty(exports, "isInterfaceType", {
  enumerable: true,
  get: function get() {
    return _definition.isInterfaceType;
  }
});
Object.defineProperty(exports, "isUnionType", {
  enumerable: true,
  get: function get() {
    return _definition.isUnionType;
  }
});
Object.defineProperty(exports, "isEnumType", {
  enumerable: true,
  get: function get() {
    return _definition.isEnumType;
  }
});
Object.defineProperty(exports, "isInputObjectType", {
  enumerable: true,
  get: function get() {
    return _definition.isInputObjectType;
  }
});
Object.defineProperty(exports, "isListType", {
  enumerable: true,
  get: function get() {
    return _definition.isListType;
  }
});
Object.defineProperty(exports, "isNonNullType", {
  enumerable: true,
  get: function get() {
    return _definition.isNonNullType;
  }
});
Object.defineProperty(exports, "isInputType", {
  enumerable: true,
  get: function get() {
    return _definition.isInputType;
  }
});
Object.defineProperty(exports, "isOutputType", {
  enumerable: true,
  get: function get() {
    return _definition.isOutputType;
  }
});
Object.defineProperty(exports, "isLeafType", {
  enumerable: true,
  get: function get() {
    return _definition.isLeafType;
  }
});
Object.defineProperty(exports, "isCompositeType", {
  enumerable: true,
  get: function get() {
    return _definition.isCompositeType;
  }
});
Object.defineProperty(exports, "isAbstractType", {
  enumerable: true,
  get: function get() {
    return _definition.isAbstractType;
  }
});
Object.defineProperty(exports, "isWrappingType", {
  enumerable: true,
  get: function get() {
    return _definition.isWrappingType;
  }
});
Object.defineProperty(exports, "isNullableType", {
  enumerable: true,
  get: function get() {
    return _definition.isNullableType;
  }
});
Object.defineProperty(exports, "isNamedType", {
  enumerable: true,
  get: function get() {
    return _definition.isNamedType;
  }
});
Object.defineProperty(exports, "isRequiredArgument", {
  enumerable: true,
  get: function get() {
    return _definition.isRequiredArgument;
  }
});
Object.defineProperty(exports, "isRequiredInputField", {
  enumerable: true,
  get: function get() {
    return _definition.isRequiredInputField;
  }
});
Object.defineProperty(exports, "assertType", {
  enumerable: true,
  get: function get() {
    return _definition.assertType;
  }
});
Object.defineProperty(exports, "assertScalarType", {
  enumerable: true,
  get: function get() {
    return _definition.assertScalarType;
  }
});
Object.defineProperty(exports, "assertObjectType", {
  enumerable: true,
  get: function get() {
    return _definition.assertObjectType;
  }
});
Object.defineProperty(exports, "assertInterfaceType", {
  enumerable: true,
  get: function get() {
    return _definition.assertInterfaceType;
  }
});
Object.defineProperty(exports, "assertUnionType", {
  enumerable: true,
  get: function get() {
    return _definition.assertUnionType;
  }
});
Object.defineProperty(exports, "assertEnumType", {
  enumerable: true,
  get: function get() {
    return _definition.assertEnumType;
  }
});
Object.defineProperty(exports, "assertInputObjectType", {
  enumerable: true,
  get: function get() {
    return _definition.assertInputObjectType;
  }
});
Object.defineProperty(exports, "assertListType", {
  enumerable: true,
  get: function get() {
    return _definition.assertListType;
  }
});
Object.defineProperty(exports, "assertNonNullType", {
  enumerable: true,
  get: function get() {
    return _definition.assertNonNullType;
  }
});
Object.defineProperty(exports, "assertInputType", {
  enumerable: true,
  get: function get() {
    return _definition.assertInputType;
  }
});
Object.defineProperty(exports, "assertOutputType", {
  enumerable: true,
  get: function get() {
    return _definition.assertOutputType;
  }
});
Object.defineProperty(exports, "assertLeafType", {
  enumerable: true,
  get: function get() {
    return _definition.assertLeafType;
  }
});
Object.defineProperty(exports, "assertCompositeType", {
  enumerable: true,
  get: function get() {
    return _definition.assertCompositeType;
  }
});
Object.defineProperty(exports, "assertAbstractType", {
  enumerable: true,
  get: function get() {
    return _definition.assertAbstractType;
  }
});
Object.defineProperty(exports, "assertWrappingType", {
  enumerable: true,
  get: function get() {
    return _definition.assertWrappingType;
  }
});
Object.defineProperty(exports, "assertNullableType", {
  enumerable: true,
  get: function get() {
    return _definition.assertNullableType;
  }
});
Object.defineProperty(exports, "assertNamedType", {
  enumerable: true,
  get: function get() {
    return _definition.assertNamedType;
  }
});
Object.defineProperty(exports, "getNullableType", {
  enumerable: true,
  get: function get() {
    return _definition.getNullableType;
  }
});
Object.defineProperty(exports, "getNamedType", {
  enumerable: true,
  get: function get() {
    return _definition.getNamedType;
  }
});
Object.defineProperty(exports, "GraphQLScalarType", {
  enumerable: true,
  get: function get() {
    return _definition.GraphQLScalarType;
  }
});
Object.defineProperty(exports, "GraphQLObjectType", {
  enumerable: true,
  get: function get() {
    return _definition.GraphQLObjectType;
  }
});
Object.defineProperty(exports, "GraphQLInterfaceType", {
  enumerable: true,
  get: function get() {
    return _definition.GraphQLInterfaceType;
  }
});
Object.defineProperty(exports, "GraphQLUnionType", {
  enumerable: true,
  get: function get() {
    return _definition.GraphQLUnionType;
  }
});
Object.defineProperty(exports, "GraphQLEnumType", {
  enumerable: true,
  get: function get() {
    return _definition.GraphQLEnumType;
  }
});
Object.defineProperty(exports, "GraphQLInputObjectType", {
  enumerable: true,
  get: function get() {
    return _definition.GraphQLInputObjectType;
  }
});
Object.defineProperty(exports, "GraphQLList", {
  enumerable: true,
  get: function get() {
    return _definition.GraphQLList;
  }
});
Object.defineProperty(exports, "GraphQLNonNull", {
  enumerable: true,
  get: function get() {
    return _definition.GraphQLNonNull;
  }
});
Object.defineProperty(exports, "isDirective", {
  enumerable: true,
  get: function get() {
    return _directives.isDirective;
  }
});
Object.defineProperty(exports, "assertDirective", {
  enumerable: true,
  get: function get() {
    return _directives.assertDirective;
  }
});
Object.defineProperty(exports, "GraphQLDirective", {
  enumerable: true,
  get: function get() {
    return _directives.GraphQLDirective;
  }
});
Object.defineProperty(exports, "isSpecifiedDirective", {
  enumerable: true,
  get: function get() {
    return _directives.isSpecifiedDirective;
  }
});
Object.defineProperty(exports, "specifiedDirectives", {
  enumerable: true,
  get: function get() {
    return _directives.specifiedDirectives;
  }
});
Object.defineProperty(exports, "GraphQLIncludeDirective", {
  enumerable: true,
  get: function get() {
    return _directives.GraphQLIncludeDirective;
  }
});
Object.defineProperty(exports, "GraphQLSkipDirective", {
  enumerable: true,
  get: function get() {
    return _directives.GraphQLSkipDirective;
  }
});
Object.defineProperty(exports, "GraphQLDeprecatedDirective", {
  enumerable: true,
  get: function get() {
    return _directives.GraphQLDeprecatedDirective;
  }
});
Object.defineProperty(exports, "GraphQLSpecifiedByDirective", {
  enumerable: true,
  get: function get() {
    return _directives.GraphQLSpecifiedByDirective;
  }
});
Object.defineProperty(exports, "DEFAULT_DEPRECATION_REASON", {
  enumerable: true,
  get: function get() {
    return _directives.DEFAULT_DEPRECATION_REASON;
  }
});
Object.defineProperty(exports, "isSpecifiedScalarType", {
  enumerable: true,
  get: function get() {
    return _scalars.isSpecifiedScalarType;
  }
});
Object.defineProperty(exports, "specifiedScalarTypes", {
  enumerable: true,
  get: function get() {
    return _scalars.specifiedScalarTypes;
  }
});
Object.defineProperty(exports, "GraphQLInt", {
  enumerable: true,
  get: function get() {
    return _scalars.GraphQLInt;
  }
});
Object.defineProperty(exports, "GraphQLFloat", {
  enumerable: true,
  get: function get() {
    return _scalars.GraphQLFloat;
  }
});
Object.defineProperty(exports, "GraphQLString", {
  enumerable: true,
  get: function get() {
    return _scalars.GraphQLString;
  }
});
Object.defineProperty(exports, "GraphQLBoolean", {
  enumerable: true,
  get: function get() {
    return _scalars.GraphQLBoolean;
  }
});
Object.defineProperty(exports, "GraphQLID", {
  enumerable: true,
  get: function get() {
    return _scalars.GraphQLID;
  }
});
Object.defineProperty(exports, "isIntrospectionType", {
  enumerable: true,
  get: function get() {
    return _introspection.isIntrospectionType;
  }
});
Object.defineProperty(exports, "introspectionTypes", {
  enumerable: true,
  get: function get() {
    return _introspection.introspectionTypes;
  }
});
Object.defineProperty(exports, "__Schema", {
  enumerable: true,
  get: function get() {
    return _introspection.__Schema;
  }
});
Object.defineProperty(exports, "__Directive", {
  enumerable: true,
  get: function get() {
    return _introspection.__Directive;
  }
});
Object.defineProperty(exports, "__DirectiveLocation", {
  enumerable: true,
  get: function get() {
    return _introspection.__DirectiveLocation;
  }
});
Object.defineProperty(exports, "__Type", {
  enumerable: true,
  get: function get() {
    return _introspection.__Type;
  }
});
Object.defineProperty(exports, "__Field", {
  enumerable: true,
  get: function get() {
    return _introspection.__Field;
  }
});
Object.defineProperty(exports, "__InputValue", {
  enumerable: true,
  get: function get() {
    return _introspection.__InputValue;
  }
});
Object.defineProperty(exports, "__EnumValue", {
  enumerable: true,
  get: function get() {
    return _introspection.__EnumValue;
  }
});
Object.defineProperty(exports, "__TypeKind", {
  enumerable: true,
  get: function get() {
    return _introspection.__TypeKind;
  }
});
Object.defineProperty(exports, "TypeKind", {
  enumerable: true,
  get: function get() {
    return _introspection.TypeKind;
  }
});
Object.defineProperty(exports, "SchemaMetaFieldDef", {
  enumerable: true,
  get: function get() {
    return _introspection.SchemaMetaFieldDef;
  }
});
Object.defineProperty(exports, "TypeMetaFieldDef", {
  enumerable: true,
  get: function get() {
    return _introspection.TypeMetaFieldDef;
  }
});
Object.defineProperty(exports, "TypeNameMetaFieldDef", {
  enumerable: true,
  get: function get() {
    return _introspection.TypeNameMetaFieldDef;
  }
});
Object.defineProperty(exports, "validateSchema", {
  enumerable: true,
  get: function get() {
    return _validate.validateSchema;
  }
});
Object.defineProperty(exports, "assertValidSchema", {
  enumerable: true,
  get: function get() {
    return _validate.assertValidSchema;
  }
});

var _schema = require("./schema.js");

var _definition = require("./definition.js");

var _directives = require("./directives.js");

var _scalars = require("./scalars.js");

var _introspection = require("./introspection.js");

var _validate = require("./validate.js");

},{"./definition.js":75,"./directives.js":76,"./introspection.js":78,"./scalars.js":79,"./schema.js":80,"./validate.js":81}],78:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isIntrospectionType = isIntrospectionType;
exports.introspectionTypes = exports.TypeNameMetaFieldDef = exports.TypeMetaFieldDef = exports.SchemaMetaFieldDef = exports.__TypeKind = exports.TypeKind = exports.__EnumValue = exports.__InputValue = exports.__Field = exports.__Type = exports.__DirectiveLocation = exports.__Directive = exports.__Schema = void 0;

var _objectValues = _interopRequireDefault(require("../polyfills/objectValues.js"));

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _invariant = _interopRequireDefault(require("../jsutils/invariant.js"));

var _printer = require("../language/printer.js");

var _directiveLocation = require("../language/directiveLocation.js");

var _astFromValue = require("../utilities/astFromValue.js");

var _scalars = require("./scalars.js");

var _definition = require("./definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var __Schema = new _definition.GraphQLObjectType({
  name: '__Schema',
  description: 'A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations.',
  fields: function fields() {
    return {
      description: {
        type: _scalars.GraphQLString,
        resolve: function resolve(schema) {
          return schema.description;
        }
      },
      types: {
        description: 'A list of all types supported by this server.',
        type: new _definition.GraphQLNonNull(new _definition.GraphQLList(new _definition.GraphQLNonNull(__Type))),
        resolve: function resolve(schema) {
          return (0, _objectValues.default)(schema.getTypeMap());
        }
      },
      queryType: {
        description: 'The type that query operations will be rooted at.',
        type: new _definition.GraphQLNonNull(__Type),
        resolve: function resolve(schema) {
          return schema.getQueryType();
        }
      },
      mutationType: {
        description: 'If this server supports mutation, the type that mutation operations will be rooted at.',
        type: __Type,
        resolve: function resolve(schema) {
          return schema.getMutationType();
        }
      },
      subscriptionType: {
        description: 'If this server support subscription, the type that subscription operations will be rooted at.',
        type: __Type,
        resolve: function resolve(schema) {
          return schema.getSubscriptionType();
        }
      },
      directives: {
        description: 'A list of all directives supported by this server.',
        type: new _definition.GraphQLNonNull(new _definition.GraphQLList(new _definition.GraphQLNonNull(__Directive))),
        resolve: function resolve(schema) {
          return schema.getDirectives();
        }
      }
    };
  }
});

exports.__Schema = __Schema;

var __Directive = new _definition.GraphQLObjectType({
  name: '__Directive',
  description: "A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document.\n\nIn some cases, you need to provide options to alter GraphQL's execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor.",
  fields: function fields() {
    return {
      name: {
        type: new _definition.GraphQLNonNull(_scalars.GraphQLString),
        resolve: function resolve(directive) {
          return directive.name;
        }
      },
      description: {
        type: _scalars.GraphQLString,
        resolve: function resolve(directive) {
          return directive.description;
        }
      },
      isRepeatable: {
        type: new _definition.GraphQLNonNull(_scalars.GraphQLBoolean),
        resolve: function resolve(directive) {
          return directive.isRepeatable;
        }
      },
      locations: {
        type: new _definition.GraphQLNonNull(new _definition.GraphQLList(new _definition.GraphQLNonNull(__DirectiveLocation))),
        resolve: function resolve(directive) {
          return directive.locations;
        }
      },
      args: {
        type: new _definition.GraphQLNonNull(new _definition.GraphQLList(new _definition.GraphQLNonNull(__InputValue))),
        resolve: function resolve(directive) {
          return directive.args;
        }
      }
    };
  }
});

exports.__Directive = __Directive;

var __DirectiveLocation = new _definition.GraphQLEnumType({
  name: '__DirectiveLocation',
  description: 'A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies.',
  values: {
    QUERY: {
      value: _directiveLocation.DirectiveLocation.QUERY,
      description: 'Location adjacent to a query operation.'
    },
    MUTATION: {
      value: _directiveLocation.DirectiveLocation.MUTATION,
      description: 'Location adjacent to a mutation operation.'
    },
    SUBSCRIPTION: {
      value: _directiveLocation.DirectiveLocation.SUBSCRIPTION,
      description: 'Location adjacent to a subscription operation.'
    },
    FIELD: {
      value: _directiveLocation.DirectiveLocation.FIELD,
      description: 'Location adjacent to a field.'
    },
    FRAGMENT_DEFINITION: {
      value: _directiveLocation.DirectiveLocation.FRAGMENT_DEFINITION,
      description: 'Location adjacent to a fragment definition.'
    },
    FRAGMENT_SPREAD: {
      value: _directiveLocation.DirectiveLocation.FRAGMENT_SPREAD,
      description: 'Location adjacent to a fragment spread.'
    },
    INLINE_FRAGMENT: {
      value: _directiveLocation.DirectiveLocation.INLINE_FRAGMENT,
      description: 'Location adjacent to an inline fragment.'
    },
    VARIABLE_DEFINITION: {
      value: _directiveLocation.DirectiveLocation.VARIABLE_DEFINITION,
      description: 'Location adjacent to a variable definition.'
    },
    SCHEMA: {
      value: _directiveLocation.DirectiveLocation.SCHEMA,
      description: 'Location adjacent to a schema definition.'
    },
    SCALAR: {
      value: _directiveLocation.DirectiveLocation.SCALAR,
      description: 'Location adjacent to a scalar definition.'
    },
    OBJECT: {
      value: _directiveLocation.DirectiveLocation.OBJECT,
      description: 'Location adjacent to an object type definition.'
    },
    FIELD_DEFINITION: {
      value: _directiveLocation.DirectiveLocation.FIELD_DEFINITION,
      description: 'Location adjacent to a field definition.'
    },
    ARGUMENT_DEFINITION: {
      value: _directiveLocation.DirectiveLocation.ARGUMENT_DEFINITION,
      description: 'Location adjacent to an argument definition.'
    },
    INTERFACE: {
      value: _directiveLocation.DirectiveLocation.INTERFACE,
      description: 'Location adjacent to an interface definition.'
    },
    UNION: {
      value: _directiveLocation.DirectiveLocation.UNION,
      description: 'Location adjacent to a union definition.'
    },
    ENUM: {
      value: _directiveLocation.DirectiveLocation.ENUM,
      description: 'Location adjacent to an enum definition.'
    },
    ENUM_VALUE: {
      value: _directiveLocation.DirectiveLocation.ENUM_VALUE,
      description: 'Location adjacent to an enum value definition.'
    },
    INPUT_OBJECT: {
      value: _directiveLocation.DirectiveLocation.INPUT_OBJECT,
      description: 'Location adjacent to an input object type definition.'
    },
    INPUT_FIELD_DEFINITION: {
      value: _directiveLocation.DirectiveLocation.INPUT_FIELD_DEFINITION,
      description: 'Location adjacent to an input object field definition.'
    }
  }
});

exports.__DirectiveLocation = __DirectiveLocation;

var __Type = new _definition.GraphQLObjectType({
  name: '__Type',
  description: 'The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.\n\nDepending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name, description and optional `specifiedByUrl`, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types.',
  fields: function fields() {
    return {
      kind: {
        type: new _definition.GraphQLNonNull(__TypeKind),
        resolve: function resolve(type) {
          if ((0, _definition.isScalarType)(type)) {
            return TypeKind.SCALAR;
          }

          if ((0, _definition.isObjectType)(type)) {
            return TypeKind.OBJECT;
          }

          if ((0, _definition.isInterfaceType)(type)) {
            return TypeKind.INTERFACE;
          }

          if ((0, _definition.isUnionType)(type)) {
            return TypeKind.UNION;
          }

          if ((0, _definition.isEnumType)(type)) {
            return TypeKind.ENUM;
          }

          if ((0, _definition.isInputObjectType)(type)) {
            return TypeKind.INPUT_OBJECT;
          }

          if ((0, _definition.isListType)(type)) {
            return TypeKind.LIST;
          } // istanbul ignore else (See: 'https://github.com/graphql/graphql-js/issues/2618')


          if ((0, _definition.isNonNullType)(type)) {
            return TypeKind.NON_NULL;
          } // istanbul ignore next (Not reachable. All possible types have been considered)


          false || (0, _invariant.default)(0, "Unexpected type: \"".concat((0, _inspect.default)(type), "\"."));
        }
      },
      name: {
        type: _scalars.GraphQLString,
        resolve: function resolve(type) {
          return type.name !== undefined ? type.name : undefined;
        }
      },
      description: {
        type: _scalars.GraphQLString,
        resolve: function resolve(type) {
          return type.description !== undefined ? type.description : undefined;
        }
      },
      specifiedByUrl: {
        type: _scalars.GraphQLString,
        resolve: function resolve(obj) {
          return obj.specifiedByUrl !== undefined ? obj.specifiedByUrl : undefined;
        }
      },
      fields: {
        type: new _definition.GraphQLList(new _definition.GraphQLNonNull(__Field)),
        args: {
          includeDeprecated: {
            type: _scalars.GraphQLBoolean,
            defaultValue: false
          }
        },
        resolve: function resolve(type, _ref) {
          var includeDeprecated = _ref.includeDeprecated;

          if ((0, _definition.isObjectType)(type) || (0, _definition.isInterfaceType)(type)) {
            var fields = (0, _objectValues.default)(type.getFields());
            return includeDeprecated ? fields : fields.filter(function (field) {
              return field.deprecationReason == null;
            });
          }
        }
      },
      interfaces: {
        type: new _definition.GraphQLList(new _definition.GraphQLNonNull(__Type)),
        resolve: function resolve(type) {
          if ((0, _definition.isObjectType)(type) || (0, _definition.isInterfaceType)(type)) {
            return type.getInterfaces();
          }
        }
      },
      possibleTypes: {
        type: new _definition.GraphQLList(new _definition.GraphQLNonNull(__Type)),
        resolve: function resolve(type, _args, _context, _ref2) {
          var schema = _ref2.schema;

          if ((0, _definition.isAbstractType)(type)) {
            return schema.getPossibleTypes(type);
          }
        }
      },
      enumValues: {
        type: new _definition.GraphQLList(new _definition.GraphQLNonNull(__EnumValue)),
        args: {
          includeDeprecated: {
            type: _scalars.GraphQLBoolean,
            defaultValue: false
          }
        },
        resolve: function resolve(type, _ref3) {
          var includeDeprecated = _ref3.includeDeprecated;

          if ((0, _definition.isEnumType)(type)) {
            var values = type.getValues();
            return includeDeprecated ? values : values.filter(function (field) {
              return field.deprecationReason == null;
            });
          }
        }
      },
      inputFields: {
        type: new _definition.GraphQLList(new _definition.GraphQLNonNull(__InputValue)),
        args: {
          includeDeprecated: {
            type: _scalars.GraphQLBoolean,
            defaultValue: false
          }
        },
        resolve: function resolve(type, _ref4) {
          var includeDeprecated = _ref4.includeDeprecated;

          if ((0, _definition.isInputObjectType)(type)) {
            var values = (0, _objectValues.default)(type.getFields());
            return includeDeprecated ? values : values.filter(function (field) {
              return field.deprecationReason == null;
            });
          }
        }
      },
      ofType: {
        type: __Type,
        resolve: function resolve(type) {
          return type.ofType !== undefined ? type.ofType : undefined;
        }
      }
    };
  }
});

exports.__Type = __Type;

var __Field = new _definition.GraphQLObjectType({
  name: '__Field',
  description: 'Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type.',
  fields: function fields() {
    return {
      name: {
        type: new _definition.GraphQLNonNull(_scalars.GraphQLString),
        resolve: function resolve(field) {
          return field.name;
        }
      },
      description: {
        type: _scalars.GraphQLString,
        resolve: function resolve(field) {
          return field.description;
        }
      },
      args: {
        type: new _definition.GraphQLNonNull(new _definition.GraphQLList(new _definition.GraphQLNonNull(__InputValue))),
        args: {
          includeDeprecated: {
            type: _scalars.GraphQLBoolean,
            defaultValue: false
          }
        },
        resolve: function resolve(field, _ref5) {
          var includeDeprecated = _ref5.includeDeprecated;
          return includeDeprecated ? field.args : field.args.filter(function (arg) {
            return arg.deprecationReason == null;
          });
        }
      },
      type: {
        type: new _definition.GraphQLNonNull(__Type),
        resolve: function resolve(field) {
          return field.type;
        }
      },
      isDeprecated: {
        type: new _definition.GraphQLNonNull(_scalars.GraphQLBoolean),
        resolve: function resolve(field) {
          return field.deprecationReason != null;
        }
      },
      deprecationReason: {
        type: _scalars.GraphQLString,
        resolve: function resolve(field) {
          return field.deprecationReason;
        }
      }
    };
  }
});

exports.__Field = __Field;

var __InputValue = new _definition.GraphQLObjectType({
  name: '__InputValue',
  description: 'Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value.',
  fields: function fields() {
    return {
      name: {
        type: new _definition.GraphQLNonNull(_scalars.GraphQLString),
        resolve: function resolve(inputValue) {
          return inputValue.name;
        }
      },
      description: {
        type: _scalars.GraphQLString,
        resolve: function resolve(inputValue) {
          return inputValue.description;
        }
      },
      type: {
        type: new _definition.GraphQLNonNull(__Type),
        resolve: function resolve(inputValue) {
          return inputValue.type;
        }
      },
      defaultValue: {
        type: _scalars.GraphQLString,
        description: 'A GraphQL-formatted string representing the default value for this input value.',
        resolve: function resolve(inputValue) {
          var type = inputValue.type,
              defaultValue = inputValue.defaultValue;
          var valueAST = (0, _astFromValue.astFromValue)(defaultValue, type);
          return valueAST ? (0, _printer.print)(valueAST) : null;
        }
      },
      isDeprecated: {
        type: new _definition.GraphQLNonNull(_scalars.GraphQLBoolean),
        resolve: function resolve(field) {
          return field.deprecationReason != null;
        }
      },
      deprecationReason: {
        type: _scalars.GraphQLString,
        resolve: function resolve(obj) {
          return obj.deprecationReason;
        }
      }
    };
  }
});

exports.__InputValue = __InputValue;

var __EnumValue = new _definition.GraphQLObjectType({
  name: '__EnumValue',
  description: 'One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string.',
  fields: function fields() {
    return {
      name: {
        type: new _definition.GraphQLNonNull(_scalars.GraphQLString),
        resolve: function resolve(enumValue) {
          return enumValue.name;
        }
      },
      description: {
        type: _scalars.GraphQLString,
        resolve: function resolve(enumValue) {
          return enumValue.description;
        }
      },
      isDeprecated: {
        type: new _definition.GraphQLNonNull(_scalars.GraphQLBoolean),
        resolve: function resolve(enumValue) {
          return enumValue.deprecationReason != null;
        }
      },
      deprecationReason: {
        type: _scalars.GraphQLString,
        resolve: function resolve(enumValue) {
          return enumValue.deprecationReason;
        }
      }
    };
  }
});

exports.__EnumValue = __EnumValue;
var TypeKind = Object.freeze({
  SCALAR: 'SCALAR',
  OBJECT: 'OBJECT',
  INTERFACE: 'INTERFACE',
  UNION: 'UNION',
  ENUM: 'ENUM',
  INPUT_OBJECT: 'INPUT_OBJECT',
  LIST: 'LIST',
  NON_NULL: 'NON_NULL'
});
exports.TypeKind = TypeKind;

var __TypeKind = new _definition.GraphQLEnumType({
  name: '__TypeKind',
  description: 'An enum describing what kind of type a given `__Type` is.',
  values: {
    SCALAR: {
      value: TypeKind.SCALAR,
      description: 'Indicates this type is a scalar.'
    },
    OBJECT: {
      value: TypeKind.OBJECT,
      description: 'Indicates this type is an object. `fields` and `interfaces` are valid fields.'
    },
    INTERFACE: {
      value: TypeKind.INTERFACE,
      description: 'Indicates this type is an interface. `fields`, `interfaces`, and `possibleTypes` are valid fields.'
    },
    UNION: {
      value: TypeKind.UNION,
      description: 'Indicates this type is a union. `possibleTypes` is a valid field.'
    },
    ENUM: {
      value: TypeKind.ENUM,
      description: 'Indicates this type is an enum. `enumValues` is a valid field.'
    },
    INPUT_OBJECT: {
      value: TypeKind.INPUT_OBJECT,
      description: 'Indicates this type is an input object. `inputFields` is a valid field.'
    },
    LIST: {
      value: TypeKind.LIST,
      description: 'Indicates this type is a list. `ofType` is a valid field.'
    },
    NON_NULL: {
      value: TypeKind.NON_NULL,
      description: 'Indicates this type is a non-null. `ofType` is a valid field.'
    }
  }
});
/**
 * Note that these are GraphQLField and not GraphQLFieldConfig,
 * so the format for args is different.
 */


exports.__TypeKind = __TypeKind;
var SchemaMetaFieldDef = {
  name: '__schema',
  type: new _definition.GraphQLNonNull(__Schema),
  description: 'Access the current type schema of this server.',
  args: [],
  resolve: function resolve(_source, _args, _context, _ref6) {
    var schema = _ref6.schema;
    return schema;
  },
  isDeprecated: false,
  deprecationReason: undefined,
  extensions: undefined,
  astNode: undefined
};
exports.SchemaMetaFieldDef = SchemaMetaFieldDef;
var TypeMetaFieldDef = {
  name: '__type',
  type: __Type,
  description: 'Request the type information of a single type.',
  args: [{
    name: 'name',
    description: undefined,
    type: new _definition.GraphQLNonNull(_scalars.GraphQLString),
    defaultValue: undefined,
    deprecationReason: undefined,
    extensions: undefined,
    astNode: undefined
  }],
  resolve: function resolve(_source, _ref7, _context, _ref8) {
    var name = _ref7.name;
    var schema = _ref8.schema;
    return schema.getType(name);
  },
  isDeprecated: false,
  deprecationReason: undefined,
  extensions: undefined,
  astNode: undefined
};
exports.TypeMetaFieldDef = TypeMetaFieldDef;
var TypeNameMetaFieldDef = {
  name: '__typename',
  type: new _definition.GraphQLNonNull(_scalars.GraphQLString),
  description: 'The name of the current Object type at runtime.',
  args: [],
  resolve: function resolve(_source, _args, _context, _ref9) {
    var parentType = _ref9.parentType;
    return parentType.name;
  },
  isDeprecated: false,
  deprecationReason: undefined,
  extensions: undefined,
  astNode: undefined
};
exports.TypeNameMetaFieldDef = TypeNameMetaFieldDef;
var introspectionTypes = Object.freeze([__Schema, __Directive, __DirectiveLocation, __Type, __Field, __InputValue, __EnumValue, __TypeKind]);
exports.introspectionTypes = introspectionTypes;

function isIntrospectionType(type) {
  return introspectionTypes.some(function (_ref10) {
    var name = _ref10.name;
    return type.name === name;
  });
}

},{"../jsutils/inspect.js":33,"../jsutils/invariant.js":35,"../language/directiveLocation.js":53,"../language/printer.js":61,"../polyfills/objectValues.js":70,"../utilities/astFromValue.js":84,"./definition.js":75,"./scalars.js":79}],79:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isSpecifiedScalarType = isSpecifiedScalarType;
exports.specifiedScalarTypes = exports.GraphQLID = exports.GraphQLBoolean = exports.GraphQLString = exports.GraphQLFloat = exports.GraphQLInt = void 0;

var _isFinite = _interopRequireDefault(require("../polyfills/isFinite.js"));

var _isInteger = _interopRequireDefault(require("../polyfills/isInteger.js"));

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _isObjectLike = _interopRequireDefault(require("../jsutils/isObjectLike.js"));

var _kinds = require("../language/kinds.js");

var _printer = require("../language/printer.js");

var _GraphQLError = require("../error/GraphQLError.js");

var _definition = require("./definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

// As per the GraphQL Spec, Integers are only treated as valid when a valid
// 32-bit signed integer, providing the broadest support across platforms.
//
// n.b. JavaScript's integers are safe between -(2^53 - 1) and 2^53 - 1 because
// they are internally represented as IEEE 754 doubles.
var MAX_INT = 2147483647;
var MIN_INT = -2147483648;

function serializeInt(outputValue) {
  var coercedValue = serializeObject(outputValue);

  if (typeof coercedValue === 'boolean') {
    return coercedValue ? 1 : 0;
  }

  var num = coercedValue;

  if (typeof coercedValue === 'string' && coercedValue !== '') {
    num = Number(coercedValue);
  }

  if (!(0, _isInteger.default)(num)) {
    throw new _GraphQLError.GraphQLError("Int cannot represent non-integer value: ".concat((0, _inspect.default)(coercedValue)));
  }

  if (num > MAX_INT || num < MIN_INT) {
    throw new _GraphQLError.GraphQLError('Int cannot represent non 32-bit signed integer value: ' + (0, _inspect.default)(coercedValue));
  }

  return num;
}

function coerceInt(inputValue) {
  if (!(0, _isInteger.default)(inputValue)) {
    throw new _GraphQLError.GraphQLError("Int cannot represent non-integer value: ".concat((0, _inspect.default)(inputValue)));
  }

  if (inputValue > MAX_INT || inputValue < MIN_INT) {
    throw new _GraphQLError.GraphQLError("Int cannot represent non 32-bit signed integer value: ".concat(inputValue));
  }

  return inputValue;
}

var GraphQLInt = new _definition.GraphQLScalarType({
  name: 'Int',
  description: 'The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1.',
  serialize: serializeInt,
  parseValue: coerceInt,
  parseLiteral: function parseLiteral(valueNode) {
    if (valueNode.kind !== _kinds.Kind.INT) {
      throw new _GraphQLError.GraphQLError("Int cannot represent non-integer value: ".concat((0, _printer.print)(valueNode)), valueNode);
    }

    var num = parseInt(valueNode.value, 10);

    if (num > MAX_INT || num < MIN_INT) {
      throw new _GraphQLError.GraphQLError("Int cannot represent non 32-bit signed integer value: ".concat(valueNode.value), valueNode);
    }

    return num;
  }
});
exports.GraphQLInt = GraphQLInt;

function serializeFloat(outputValue) {
  var coercedValue = serializeObject(outputValue);

  if (typeof coercedValue === 'boolean') {
    return coercedValue ? 1 : 0;
  }

  var num = coercedValue;

  if (typeof coercedValue === 'string' && coercedValue !== '') {
    num = Number(coercedValue);
  }

  if (!(0, _isFinite.default)(num)) {
    throw new _GraphQLError.GraphQLError("Float cannot represent non numeric value: ".concat((0, _inspect.default)(coercedValue)));
  }

  return num;
}

function coerceFloat(inputValue) {
  if (!(0, _isFinite.default)(inputValue)) {
    throw new _GraphQLError.GraphQLError("Float cannot represent non numeric value: ".concat((0, _inspect.default)(inputValue)));
  }

  return inputValue;
}

var GraphQLFloat = new _definition.GraphQLScalarType({
  name: 'Float',
  description: 'The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point).',
  serialize: serializeFloat,
  parseValue: coerceFloat,
  parseLiteral: function parseLiteral(valueNode) {
    if (valueNode.kind !== _kinds.Kind.FLOAT && valueNode.kind !== _kinds.Kind.INT) {
      throw new _GraphQLError.GraphQLError("Float cannot represent non numeric value: ".concat((0, _printer.print)(valueNode)), valueNode);
    }

    return parseFloat(valueNode.value);
  }
}); // Support serializing objects with custom valueOf() or toJSON() functions -
// a common way to represent a complex value which can be represented as
// a string (ex: MongoDB id objects).

exports.GraphQLFloat = GraphQLFloat;

function serializeObject(outputValue) {
  if ((0, _isObjectLike.default)(outputValue)) {
    if (typeof outputValue.valueOf === 'function') {
      var valueOfResult = outputValue.valueOf();

      if (!(0, _isObjectLike.default)(valueOfResult)) {
        return valueOfResult;
      }
    }

    if (typeof outputValue.toJSON === 'function') {
      // $FlowFixMe[incompatible-use]
      return outputValue.toJSON();
    }
  }

  return outputValue;
}

function serializeString(outputValue) {
  var coercedValue = serializeObject(outputValue); // Serialize string, boolean and number values to a string, but do not
  // attempt to coerce object, function, symbol, or other types as strings.

  if (typeof coercedValue === 'string') {
    return coercedValue;
  }

  if (typeof coercedValue === 'boolean') {
    return coercedValue ? 'true' : 'false';
  }

  if ((0, _isFinite.default)(coercedValue)) {
    return coercedValue.toString();
  }

  throw new _GraphQLError.GraphQLError("String cannot represent value: ".concat((0, _inspect.default)(outputValue)));
}

function coerceString(inputValue) {
  if (typeof inputValue !== 'string') {
    throw new _GraphQLError.GraphQLError("String cannot represent a non string value: ".concat((0, _inspect.default)(inputValue)));
  }

  return inputValue;
}

var GraphQLString = new _definition.GraphQLScalarType({
  name: 'String',
  description: 'The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text.',
  serialize: serializeString,
  parseValue: coerceString,
  parseLiteral: function parseLiteral(valueNode) {
    if (valueNode.kind !== _kinds.Kind.STRING) {
      throw new _GraphQLError.GraphQLError("String cannot represent a non string value: ".concat((0, _printer.print)(valueNode)), valueNode);
    }

    return valueNode.value;
  }
});
exports.GraphQLString = GraphQLString;

function serializeBoolean(outputValue) {
  var coercedValue = serializeObject(outputValue);

  if (typeof coercedValue === 'boolean') {
    return coercedValue;
  }

  if ((0, _isFinite.default)(coercedValue)) {
    return coercedValue !== 0;
  }

  throw new _GraphQLError.GraphQLError("Boolean cannot represent a non boolean value: ".concat((0, _inspect.default)(coercedValue)));
}

function coerceBoolean(inputValue) {
  if (typeof inputValue !== 'boolean') {
    throw new _GraphQLError.GraphQLError("Boolean cannot represent a non boolean value: ".concat((0, _inspect.default)(inputValue)));
  }

  return inputValue;
}

var GraphQLBoolean = new _definition.GraphQLScalarType({
  name: 'Boolean',
  description: 'The `Boolean` scalar type represents `true` or `false`.',
  serialize: serializeBoolean,
  parseValue: coerceBoolean,
  parseLiteral: function parseLiteral(valueNode) {
    if (valueNode.kind !== _kinds.Kind.BOOLEAN) {
      throw new _GraphQLError.GraphQLError("Boolean cannot represent a non boolean value: ".concat((0, _printer.print)(valueNode)), valueNode);
    }

    return valueNode.value;
  }
});
exports.GraphQLBoolean = GraphQLBoolean;

function serializeID(outputValue) {
  var coercedValue = serializeObject(outputValue);

  if (typeof coercedValue === 'string') {
    return coercedValue;
  }

  if ((0, _isInteger.default)(coercedValue)) {
    return String(coercedValue);
  }

  throw new _GraphQLError.GraphQLError("ID cannot represent value: ".concat((0, _inspect.default)(outputValue)));
}

function coerceID(inputValue) {
  if (typeof inputValue === 'string') {
    return inputValue;
  }

  if ((0, _isInteger.default)(inputValue)) {
    return inputValue.toString();
  }

  throw new _GraphQLError.GraphQLError("ID cannot represent value: ".concat((0, _inspect.default)(inputValue)));
}

var GraphQLID = new _definition.GraphQLScalarType({
  name: 'ID',
  description: 'The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `"4"`) or integer (such as `4`) input value will be accepted as an ID.',
  serialize: serializeID,
  parseValue: coerceID,
  parseLiteral: function parseLiteral(valueNode) {
    if (valueNode.kind !== _kinds.Kind.STRING && valueNode.kind !== _kinds.Kind.INT) {
      throw new _GraphQLError.GraphQLError('ID cannot represent a non-string and non-integer value: ' + (0, _printer.print)(valueNode), valueNode);
    }

    return valueNode.value;
  }
});
exports.GraphQLID = GraphQLID;
var specifiedScalarTypes = Object.freeze([GraphQLString, GraphQLInt, GraphQLFloat, GraphQLBoolean, GraphQLID]);
exports.specifiedScalarTypes = specifiedScalarTypes;

function isSpecifiedScalarType(type) {
  return specifiedScalarTypes.some(function (_ref) {
    var name = _ref.name;
    return type.name === name;
  });
}

},{"../error/GraphQLError.js":18,"../jsutils/inspect.js":33,"../jsutils/isObjectLike.js":37,"../language/kinds.js":55,"../language/printer.js":61,"../polyfills/isFinite.js":67,"../polyfills/isInteger.js":68,"./definition.js":75}],80:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isSchema = isSchema;
exports.assertSchema = assertSchema;
exports.GraphQLSchema = void 0;

var _find = _interopRequireDefault(require("../polyfills/find.js"));

var _arrayFrom3 = _interopRequireDefault(require("../polyfills/arrayFrom.js"));

var _objectValues5 = _interopRequireDefault(require("../polyfills/objectValues.js"));

var _symbols = require("../polyfills/symbols.js");

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _toObjMap = _interopRequireDefault(require("../jsutils/toObjMap.js"));

var _devAssert = _interopRequireDefault(require("../jsutils/devAssert.js"));

var _instanceOf = _interopRequireDefault(require("../jsutils/instanceOf.js"));

var _isObjectLike = _interopRequireDefault(require("../jsutils/isObjectLike.js"));

var _introspection = require("./introspection.js");

var _directives = require("./directives.js");

var _definition = require("./definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

// eslint-disable-next-line no-redeclare
function isSchema(schema) {
  return (0, _instanceOf.default)(schema, GraphQLSchema);
}

function assertSchema(schema) {
  if (!isSchema(schema)) {
    throw new Error("Expected ".concat((0, _inspect.default)(schema), " to be a GraphQL schema."));
  }

  return schema;
}
/**
 * Schema Definition
 *
 * A Schema is created by supplying the root types of each type of operation,
 * query and mutation (optional). A schema definition is then supplied to the
 * validator and executor.
 *
 * Example:
 *
 *     const MyAppSchema = new GraphQLSchema({
 *       query: MyAppQueryRootType,
 *       mutation: MyAppMutationRootType,
 *     })
 *
 * Note: When the schema is constructed, by default only the types that are
 * reachable by traversing the root types are included, other types must be
 * explicitly referenced.
 *
 * Example:
 *
 *     const characterInterface = new GraphQLInterfaceType({
 *       name: 'Character',
 *       ...
 *     });
 *
 *     const humanType = new GraphQLObjectType({
 *       name: 'Human',
 *       interfaces: [characterInterface],
 *       ...
 *     });
 *
 *     const droidType = new GraphQLObjectType({
 *       name: 'Droid',
 *       interfaces: [characterInterface],
 *       ...
 *     });
 *
 *     const schema = new GraphQLSchema({
 *       query: new GraphQLObjectType({
 *         name: 'Query',
 *         fields: {
 *           hero: { type: characterInterface, ... },
 *         }
 *       }),
 *       ...
 *       // Since this schema references only the `Character` interface it's
 *       // necessary to explicitly list the types that implement it if
 *       // you want them to be included in the final schema.
 *       types: [humanType, droidType],
 *     })
 *
 * Note: If an array of `directives` are provided to GraphQLSchema, that will be
 * the exact list of directives represented and allowed. If `directives` is not
 * provided then a default set of the specified directives (e.g. @include and
 * @skip) will be used. If you wish to provide *additional* directives to these
 * specified directives, you must explicitly declare them. Example:
 *
 *     const MyAppSchema = new GraphQLSchema({
 *       ...
 *       directives: specifiedDirectives.concat([ myCustomDirective ]),
 *     })
 *
 */


var GraphQLSchema = /*#__PURE__*/function () {
  // Used as a cache for validateSchema().
  function GraphQLSchema(config) {
    var _config$directives;

    // If this schema was built from a source known to be valid, then it may be
    // marked with assumeValid to avoid an additional type system validation.
    this.__validationErrors = config.assumeValid === true ? [] : undefined; // Check for common mistakes during construction to produce early errors.

    (0, _isObjectLike.default)(config) || (0, _devAssert.default)(0, 'Must provide configuration object.');
    !config.types || Array.isArray(config.types) || (0, _devAssert.default)(0, "\"types\" must be Array if provided but got: ".concat((0, _inspect.default)(config.types), "."));
    !config.directives || Array.isArray(config.directives) || (0, _devAssert.default)(0, '"directives" must be Array if provided but got: ' + "".concat((0, _inspect.default)(config.directives), "."));
    this.description = config.description;
    this.extensions = config.extensions && (0, _toObjMap.default)(config.extensions);
    this.astNode = config.astNode;
    this.extensionASTNodes = config.extensionASTNodes;
    this._queryType = config.query;
    this._mutationType = config.mutation;
    this._subscriptionType = config.subscription; // Provide specified directives (e.g. @include and @skip) by default.

    this._directives = (_config$directives = config.directives) !== null && _config$directives !== void 0 ? _config$directives : _directives.specifiedDirectives; // To preserve order of user-provided types, we add first to add them to
    // the set of "collected" types, so `collectReferencedTypes` ignore them.

    var allReferencedTypes = new Set(config.types);

    if (config.types != null) {
      for (var _i2 = 0, _config$types2 = config.types; _i2 < _config$types2.length; _i2++) {
        var type = _config$types2[_i2];
        // When we ready to process this type, we remove it from "collected" types
        // and then add it together with all dependent types in the correct position.
        allReferencedTypes.delete(type);
        collectReferencedTypes(type, allReferencedTypes);
      }
    }

    if (this._queryType != null) {
      collectReferencedTypes(this._queryType, allReferencedTypes);
    }

    if (this._mutationType != null) {
      collectReferencedTypes(this._mutationType, allReferencedTypes);
    }

    if (this._subscriptionType != null) {
      collectReferencedTypes(this._subscriptionType, allReferencedTypes);
    }

    for (var _i4 = 0, _this$_directives2 = this._directives; _i4 < _this$_directives2.length; _i4++) {
      var directive = _this$_directives2[_i4];

      // Directives are not validated until validateSchema() is called.
      if ((0, _directives.isDirective)(directive)) {
        for (var _i6 = 0, _directive$args2 = directive.args; _i6 < _directive$args2.length; _i6++) {
          var arg = _directive$args2[_i6];
          collectReferencedTypes(arg.type, allReferencedTypes);
        }
      }
    }

    collectReferencedTypes(_introspection.__Schema, allReferencedTypes); // Storing the resulting map for reference by the schema.

    this._typeMap = Object.create(null);
    this._subTypeMap = Object.create(null); // Keep track of all implementations by interface name.

    this._implementationsMap = Object.create(null);

    for (var _i8 = 0, _arrayFrom2 = (0, _arrayFrom3.default)(allReferencedTypes); _i8 < _arrayFrom2.length; _i8++) {
      var namedType = _arrayFrom2[_i8];

      if (namedType == null) {
        continue;
      }

      var typeName = namedType.name;
      typeName || (0, _devAssert.default)(0, 'One of the provided types for building the Schema is missing a name.');

      if (this._typeMap[typeName] !== undefined) {
        throw new Error("Schema must contain uniquely named types but contains multiple types named \"".concat(typeName, "\"."));
      }

      this._typeMap[typeName] = namedType;

      if ((0, _definition.isInterfaceType)(namedType)) {
        // Store implementations by interface.
        for (var _i10 = 0, _namedType$getInterfa2 = namedType.getInterfaces(); _i10 < _namedType$getInterfa2.length; _i10++) {
          var iface = _namedType$getInterfa2[_i10];

          if ((0, _definition.isInterfaceType)(iface)) {
            var implementations = this._implementationsMap[iface.name];

            if (implementations === undefined) {
              implementations = this._implementationsMap[iface.name] = {
                objects: [],
                interfaces: []
              };
            }

            implementations.interfaces.push(namedType);
          }
        }
      } else if ((0, _definition.isObjectType)(namedType)) {
        // Store implementations by objects.
        for (var _i12 = 0, _namedType$getInterfa4 = namedType.getInterfaces(); _i12 < _namedType$getInterfa4.length; _i12++) {
          var _iface = _namedType$getInterfa4[_i12];

          if ((0, _definition.isInterfaceType)(_iface)) {
            var _implementations = this._implementationsMap[_iface.name];

            if (_implementations === undefined) {
              _implementations = this._implementationsMap[_iface.name] = {
                objects: [],
                interfaces: []
              };
            }

            _implementations.objects.push(namedType);
          }
        }
      }
    }
  }

  var _proto = GraphQLSchema.prototype;

  _proto.getQueryType = function getQueryType() {
    return this._queryType;
  };

  _proto.getMutationType = function getMutationType() {
    return this._mutationType;
  };

  _proto.getSubscriptionType = function getSubscriptionType() {
    return this._subscriptionType;
  };

  _proto.getTypeMap = function getTypeMap() {
    return this._typeMap;
  };

  _proto.getType = function getType(name) {
    return this.getTypeMap()[name];
  };

  _proto.getPossibleTypes = function getPossibleTypes(abstractType) {
    return (0, _definition.isUnionType)(abstractType) ? abstractType.getTypes() : this.getImplementations(abstractType).objects;
  };

  _proto.getImplementations = function getImplementations(interfaceType) {
    var implementations = this._implementationsMap[interfaceType.name];
    return implementations !== null && implementations !== void 0 ? implementations : {
      objects: [],
      interfaces: []
    };
  } // @deprecated: use isSubType instead - will be removed in v16.
  ;

  _proto.isPossibleType = function isPossibleType(abstractType, possibleType) {
    return this.isSubType(abstractType, possibleType);
  };

  _proto.isSubType = function isSubType(abstractType, maybeSubType) {
    var map = this._subTypeMap[abstractType.name];

    if (map === undefined) {
      map = Object.create(null);

      if ((0, _definition.isUnionType)(abstractType)) {
        for (var _i14 = 0, _abstractType$getType2 = abstractType.getTypes(); _i14 < _abstractType$getType2.length; _i14++) {
          var type = _abstractType$getType2[_i14];
          map[type.name] = true;
        }
      } else {
        var implementations = this.getImplementations(abstractType);

        for (var _i16 = 0, _implementations$obje2 = implementations.objects; _i16 < _implementations$obje2.length; _i16++) {
          var _type = _implementations$obje2[_i16];
          map[_type.name] = true;
        }

        for (var _i18 = 0, _implementations$inte2 = implementations.interfaces; _i18 < _implementations$inte2.length; _i18++) {
          var _type2 = _implementations$inte2[_i18];
          map[_type2.name] = true;
        }
      }

      this._subTypeMap[abstractType.name] = map;
    }

    return map[maybeSubType.name] !== undefined;
  };

  _proto.getDirectives = function getDirectives() {
    return this._directives;
  };

  _proto.getDirective = function getDirective(name) {
    return (0, _find.default)(this.getDirectives(), function (directive) {
      return directive.name === name;
    });
  };

  _proto.toConfig = function toConfig() {
    var _this$extensionASTNod;

    return {
      description: this.description,
      query: this.getQueryType(),
      mutation: this.getMutationType(),
      subscription: this.getSubscriptionType(),
      types: (0, _objectValues5.default)(this.getTypeMap()),
      directives: this.getDirectives().slice(),
      extensions: this.extensions,
      astNode: this.astNode,
      extensionASTNodes: (_this$extensionASTNod = this.extensionASTNodes) !== null && _this$extensionASTNod !== void 0 ? _this$extensionASTNod : [],
      assumeValid: this.__validationErrors !== undefined
    };
  } // $FlowFixMe[unsupported-syntax] Flow doesn't support computed properties yet
  ;

  _createClass(GraphQLSchema, [{
    key: _symbols.SYMBOL_TO_STRING_TAG,
    get: function get() {
      return 'GraphQLSchema';
    }
  }]);

  return GraphQLSchema;
}();

exports.GraphQLSchema = GraphQLSchema;

function collectReferencedTypes(type, typeSet) {
  var namedType = (0, _definition.getNamedType)(type);

  if (!typeSet.has(namedType)) {
    typeSet.add(namedType);

    if ((0, _definition.isUnionType)(namedType)) {
      for (var _i20 = 0, _namedType$getTypes2 = namedType.getTypes(); _i20 < _namedType$getTypes2.length; _i20++) {
        var memberType = _namedType$getTypes2[_i20];
        collectReferencedTypes(memberType, typeSet);
      }
    } else if ((0, _definition.isObjectType)(namedType) || (0, _definition.isInterfaceType)(namedType)) {
      for (var _i22 = 0, _namedType$getInterfa6 = namedType.getInterfaces(); _i22 < _namedType$getInterfa6.length; _i22++) {
        var interfaceType = _namedType$getInterfa6[_i22];
        collectReferencedTypes(interfaceType, typeSet);
      }

      for (var _i24 = 0, _objectValues2 = (0, _objectValues5.default)(namedType.getFields()); _i24 < _objectValues2.length; _i24++) {
        var field = _objectValues2[_i24];
        collectReferencedTypes(field.type, typeSet);

        for (var _i26 = 0, _field$args2 = field.args; _i26 < _field$args2.length; _i26++) {
          var arg = _field$args2[_i26];
          collectReferencedTypes(arg.type, typeSet);
        }
      }
    } else if ((0, _definition.isInputObjectType)(namedType)) {
      for (var _i28 = 0, _objectValues4 = (0, _objectValues5.default)(namedType.getFields()); _i28 < _objectValues4.length; _i28++) {
        var _field = _objectValues4[_i28];
        collectReferencedTypes(_field.type, typeSet);
      }
    }
  }

  return typeSet;
}

},{"../jsutils/devAssert.js":30,"../jsutils/inspect.js":33,"../jsutils/instanceOf.js":34,"../jsutils/isObjectLike.js":37,"../jsutils/toObjMap.js":50,"../polyfills/arrayFrom.js":65,"../polyfills/find.js":66,"../polyfills/objectValues.js":70,"../polyfills/symbols.js":71,"./definition.js":75,"./directives.js":76,"./introspection.js":78}],81:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.validateSchema = validateSchema;
exports.assertValidSchema = assertValidSchema;

var _find = _interopRequireDefault(require("../polyfills/find.js"));

var _objectValues5 = _interopRequireDefault(require("../polyfills/objectValues.js"));

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _GraphQLError = require("../error/GraphQLError.js");

var _locatedError = require("../error/locatedError.js");

var _assertValidName = require("../utilities/assertValidName.js");

var _typeComparators = require("../utilities/typeComparators.js");

var _schema = require("./schema.js");

var _introspection = require("./introspection.js");

var _directives = require("./directives.js");

var _definition = require("./definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Implements the "Type Validation" sub-sections of the specification's
 * "Type System" section.
 *
 * Validation runs synchronously, returning an array of encountered errors, or
 * an empty array if no errors were encountered and the Schema is valid.
 */
function validateSchema(schema) {
  // First check to ensure the provided value is in fact a GraphQLSchema.
  (0, _schema.assertSchema)(schema); // If this Schema has already been validated, return the previous results.

  if (schema.__validationErrors) {
    return schema.__validationErrors;
  } // Validate the schema, producing a list of errors.


  var context = new SchemaValidationContext(schema);
  validateRootTypes(context);
  validateDirectives(context);
  validateTypes(context); // Persist the results of validation before returning to ensure validation
  // does not run multiple times for this schema.

  var errors = context.getErrors();
  schema.__validationErrors = errors;
  return errors;
}
/**
 * Utility function which asserts a schema is valid by throwing an error if
 * it is invalid.
 */


function assertValidSchema(schema) {
  var errors = validateSchema(schema);

  if (errors.length !== 0) {
    throw new Error(errors.map(function (error) {
      return error.message;
    }).join('\n\n'));
  }
}

var SchemaValidationContext = /*#__PURE__*/function () {
  function SchemaValidationContext(schema) {
    this._errors = [];
    this.schema = schema;
  }

  var _proto = SchemaValidationContext.prototype;

  _proto.reportError = function reportError(message, nodes) {
    var _nodes = Array.isArray(nodes) ? nodes.filter(Boolean) : nodes;

    this.addError(new _GraphQLError.GraphQLError(message, _nodes));
  };

  _proto.addError = function addError(error) {
    this._errors.push(error);
  };

  _proto.getErrors = function getErrors() {
    return this._errors;
  };

  return SchemaValidationContext;
}();

function validateRootTypes(context) {
  var schema = context.schema;
  var queryType = schema.getQueryType();

  if (!queryType) {
    context.reportError('Query root type must be provided.', schema.astNode);
  } else if (!(0, _definition.isObjectType)(queryType)) {
    var _getOperationTypeNode;

    context.reportError("Query root type must be Object type, it cannot be ".concat((0, _inspect.default)(queryType), "."), (_getOperationTypeNode = getOperationTypeNode(schema, 'query')) !== null && _getOperationTypeNode !== void 0 ? _getOperationTypeNode : queryType.astNode);
  }

  var mutationType = schema.getMutationType();

  if (mutationType && !(0, _definition.isObjectType)(mutationType)) {
    var _getOperationTypeNode2;

    context.reportError('Mutation root type must be Object type if provided, it cannot be ' + "".concat((0, _inspect.default)(mutationType), "."), (_getOperationTypeNode2 = getOperationTypeNode(schema, 'mutation')) !== null && _getOperationTypeNode2 !== void 0 ? _getOperationTypeNode2 : mutationType.astNode);
  }

  var subscriptionType = schema.getSubscriptionType();

  if (subscriptionType && !(0, _definition.isObjectType)(subscriptionType)) {
    var _getOperationTypeNode3;

    context.reportError('Subscription root type must be Object type if provided, it cannot be ' + "".concat((0, _inspect.default)(subscriptionType), "."), (_getOperationTypeNode3 = getOperationTypeNode(schema, 'subscription')) !== null && _getOperationTypeNode3 !== void 0 ? _getOperationTypeNode3 : subscriptionType.astNode);
  }
}

function getOperationTypeNode(schema, operation) {
  var operationNodes = getAllSubNodes(schema, function (node) {
    return node.operationTypes;
  });

  for (var _i2 = 0; _i2 < operationNodes.length; _i2++) {
    var node = operationNodes[_i2];

    if (node.operation === operation) {
      return node.type;
    }
  }

  return undefined;
}

function validateDirectives(context) {
  for (var _i4 = 0, _context$schema$getDi2 = context.schema.getDirectives(); _i4 < _context$schema$getDi2.length; _i4++) {
    var directive = _context$schema$getDi2[_i4];

    // Ensure all directives are in fact GraphQL directives.
    if (!(0, _directives.isDirective)(directive)) {
      context.reportError("Expected directive but got: ".concat((0, _inspect.default)(directive), "."), directive === null || directive === void 0 ? void 0 : directive.astNode);
      continue;
    } // Ensure they are named correctly.


    validateName(context, directive); // TODO: Ensure proper locations.
    // Ensure the arguments are valid.

    for (var _i6 = 0, _directive$args2 = directive.args; _i6 < _directive$args2.length; _i6++) {
      var arg = _directive$args2[_i6];
      // Ensure they are named correctly.
      validateName(context, arg); // Ensure the type is an input type.

      if (!(0, _definition.isInputType)(arg.type)) {
        context.reportError("The type of @".concat(directive.name, "(").concat(arg.name, ":) must be Input Type ") + "but got: ".concat((0, _inspect.default)(arg.type), "."), arg.astNode);
      }

      if ((0, _definition.isRequiredArgument)(arg) && arg.deprecationReason != null) {
        var _arg$astNode;

        context.reportError("Required argument @".concat(directive.name, "(").concat(arg.name, ":) cannot be deprecated."), [getDeprecatedDirectiveNode(arg.astNode), // istanbul ignore next (TODO need to write coverage tests)
        (_arg$astNode = arg.astNode) === null || _arg$astNode === void 0 ? void 0 : _arg$astNode.type]);
      }
    }
  }
}

function validateName(context, node) {
  // Ensure names are valid, however introspection types opt out.
  var error = (0, _assertValidName.isValidNameError)(node.name);

  if (error) {
    context.addError((0, _locatedError.locatedError)(error, node.astNode));
  }
}

function validateTypes(context) {
  var validateInputObjectCircularRefs = createInputObjectCircularRefsValidator(context);
  var typeMap = context.schema.getTypeMap();

  for (var _i8 = 0, _objectValues2 = (0, _objectValues5.default)(typeMap); _i8 < _objectValues2.length; _i8++) {
    var type = _objectValues2[_i8];

    // Ensure all provided types are in fact GraphQL type.
    if (!(0, _definition.isNamedType)(type)) {
      context.reportError("Expected GraphQL named type but got: ".concat((0, _inspect.default)(type), "."), type.astNode);
      continue;
    } // Ensure it is named correctly (excluding introspection types).


    if (!(0, _introspection.isIntrospectionType)(type)) {
      validateName(context, type);
    }

    if ((0, _definition.isObjectType)(type)) {
      // Ensure fields are valid
      validateFields(context, type); // Ensure objects implement the interfaces they claim to.

      validateInterfaces(context, type);
    } else if ((0, _definition.isInterfaceType)(type)) {
      // Ensure fields are valid.
      validateFields(context, type); // Ensure interfaces implement the interfaces they claim to.

      validateInterfaces(context, type);
    } else if ((0, _definition.isUnionType)(type)) {
      // Ensure Unions include valid member types.
      validateUnionMembers(context, type);
    } else if ((0, _definition.isEnumType)(type)) {
      // Ensure Enums have valid values.
      validateEnumValues(context, type);
    } else if ((0, _definition.isInputObjectType)(type)) {
      // Ensure Input Object fields are valid.
      validateInputFields(context, type); // Ensure Input Objects do not contain non-nullable circular references

      validateInputObjectCircularRefs(type);
    }
  }
}

function validateFields(context, type) {
  var fields = (0, _objectValues5.default)(type.getFields()); // Objects and Interfaces both must define one or more fields.

  if (fields.length === 0) {
    context.reportError("Type ".concat(type.name, " must define one or more fields."), getAllNodes(type));
  }

  for (var _i10 = 0; _i10 < fields.length; _i10++) {
    var field = fields[_i10];
    // Ensure they are named correctly.
    validateName(context, field); // Ensure the type is an output type

    if (!(0, _definition.isOutputType)(field.type)) {
      var _field$astNode;

      context.reportError("The type of ".concat(type.name, ".").concat(field.name, " must be Output Type ") + "but got: ".concat((0, _inspect.default)(field.type), "."), (_field$astNode = field.astNode) === null || _field$astNode === void 0 ? void 0 : _field$astNode.type);
    } // Ensure the arguments are valid


    for (var _i12 = 0, _field$args2 = field.args; _i12 < _field$args2.length; _i12++) {
      var arg = _field$args2[_i12];
      var argName = arg.name; // Ensure they are named correctly.

      validateName(context, arg); // Ensure the type is an input type

      if (!(0, _definition.isInputType)(arg.type)) {
        var _arg$astNode2;

        context.reportError("The type of ".concat(type.name, ".").concat(field.name, "(").concat(argName, ":) must be Input ") + "Type but got: ".concat((0, _inspect.default)(arg.type), "."), (_arg$astNode2 = arg.astNode) === null || _arg$astNode2 === void 0 ? void 0 : _arg$astNode2.type);
      }

      if ((0, _definition.isRequiredArgument)(arg) && arg.deprecationReason != null) {
        var _arg$astNode3;

        context.reportError("Required argument ".concat(type.name, ".").concat(field.name, "(").concat(argName, ":) cannot be deprecated."), [getDeprecatedDirectiveNode(arg.astNode), // istanbul ignore next (TODO need to write coverage tests)
        (_arg$astNode3 = arg.astNode) === null || _arg$astNode3 === void 0 ? void 0 : _arg$astNode3.type]);
      }
    }
  }
}

function validateInterfaces(context, type) {
  var ifaceTypeNames = Object.create(null);

  for (var _i14 = 0, _type$getInterfaces2 = type.getInterfaces(); _i14 < _type$getInterfaces2.length; _i14++) {
    var iface = _type$getInterfaces2[_i14];

    if (!(0, _definition.isInterfaceType)(iface)) {
      context.reportError("Type ".concat((0, _inspect.default)(type), " must only implement Interface types, ") + "it cannot implement ".concat((0, _inspect.default)(iface), "."), getAllImplementsInterfaceNodes(type, iface));
      continue;
    }

    if (type === iface) {
      context.reportError("Type ".concat(type.name, " cannot implement itself because it would create a circular reference."), getAllImplementsInterfaceNodes(type, iface));
      continue;
    }

    if (ifaceTypeNames[iface.name]) {
      context.reportError("Type ".concat(type.name, " can only implement ").concat(iface.name, " once."), getAllImplementsInterfaceNodes(type, iface));
      continue;
    }

    ifaceTypeNames[iface.name] = true;
    validateTypeImplementsAncestors(context, type, iface);
    validateTypeImplementsInterface(context, type, iface);
  }
}

function validateTypeImplementsInterface(context, type, iface) {
  var typeFieldMap = type.getFields(); // Assert each interface field is implemented.

  for (var _i16 = 0, _objectValues4 = (0, _objectValues5.default)(iface.getFields()); _i16 < _objectValues4.length; _i16++) {
    var ifaceField = _objectValues4[_i16];
    var fieldName = ifaceField.name;
    var typeField = typeFieldMap[fieldName]; // Assert interface field exists on type.

    if (!typeField) {
      context.reportError("Interface field ".concat(iface.name, ".").concat(fieldName, " expected but ").concat(type.name, " does not provide it."), [ifaceField.astNode].concat(getAllNodes(type)));
      continue;
    } // Assert interface field type is satisfied by type field type, by being
    // a valid subtype. (covariant)


    if (!(0, _typeComparators.isTypeSubTypeOf)(context.schema, typeField.type, ifaceField.type)) {
      var _ifaceField$astNode, _typeField$astNode;

      context.reportError("Interface field ".concat(iface.name, ".").concat(fieldName, " expects type ") + "".concat((0, _inspect.default)(ifaceField.type), " but ").concat(type.name, ".").concat(fieldName, " ") + "is type ".concat((0, _inspect.default)(typeField.type), "."), [// istanbul ignore next (TODO need to write coverage tests)
      (_ifaceField$astNode = ifaceField.astNode) === null || _ifaceField$astNode === void 0 ? void 0 : _ifaceField$astNode.type, // istanbul ignore next (TODO need to write coverage tests)
      (_typeField$astNode = typeField.astNode) === null || _typeField$astNode === void 0 ? void 0 : _typeField$astNode.type]);
    } // Assert each interface field arg is implemented.


    var _loop = function _loop(_i18, _ifaceField$args2) {
      var ifaceArg = _ifaceField$args2[_i18];
      var argName = ifaceArg.name;
      var typeArg = (0, _find.default)(typeField.args, function (arg) {
        return arg.name === argName;
      }); // Assert interface field arg exists on object field.

      if (!typeArg) {
        context.reportError("Interface field argument ".concat(iface.name, ".").concat(fieldName, "(").concat(argName, ":) expected but ").concat(type.name, ".").concat(fieldName, " does not provide it."), [ifaceArg.astNode, typeField.astNode]);
        return "continue";
      } // Assert interface field arg type matches object field arg type.
      // (invariant)
      // TODO: change to contravariant?


      if (!(0, _typeComparators.isEqualType)(ifaceArg.type, typeArg.type)) {
        var _ifaceArg$astNode, _typeArg$astNode;

        context.reportError("Interface field argument ".concat(iface.name, ".").concat(fieldName, "(").concat(argName, ":) ") + "expects type ".concat((0, _inspect.default)(ifaceArg.type), " but ") + "".concat(type.name, ".").concat(fieldName, "(").concat(argName, ":) is type ") + "".concat((0, _inspect.default)(typeArg.type), "."), [// istanbul ignore next (TODO need to write coverage tests)
        (_ifaceArg$astNode = ifaceArg.astNode) === null || _ifaceArg$astNode === void 0 ? void 0 : _ifaceArg$astNode.type, // istanbul ignore next (TODO need to write coverage tests)
        (_typeArg$astNode = typeArg.astNode) === null || _typeArg$astNode === void 0 ? void 0 : _typeArg$astNode.type]);
      } // TODO: validate default values?

    };

    for (var _i18 = 0, _ifaceField$args2 = ifaceField.args; _i18 < _ifaceField$args2.length; _i18++) {
      var _ret = _loop(_i18, _ifaceField$args2);

      if (_ret === "continue") continue;
    } // Assert additional arguments must not be required.


    var _loop2 = function _loop2(_i20, _typeField$args2) {
      var typeArg = _typeField$args2[_i20];
      var argName = typeArg.name;
      var ifaceArg = (0, _find.default)(ifaceField.args, function (arg) {
        return arg.name === argName;
      });

      if (!ifaceArg && (0, _definition.isRequiredArgument)(typeArg)) {
        context.reportError("Object field ".concat(type.name, ".").concat(fieldName, " includes required argument ").concat(argName, " that is missing from the Interface field ").concat(iface.name, ".").concat(fieldName, "."), [typeArg.astNode, ifaceField.astNode]);
      }
    };

    for (var _i20 = 0, _typeField$args2 = typeField.args; _i20 < _typeField$args2.length; _i20++) {
      _loop2(_i20, _typeField$args2);
    }
  }
}

function validateTypeImplementsAncestors(context, type, iface) {
  var ifaceInterfaces = type.getInterfaces();

  for (var _i22 = 0, _iface$getInterfaces2 = iface.getInterfaces(); _i22 < _iface$getInterfaces2.length; _i22++) {
    var transitive = _iface$getInterfaces2[_i22];

    if (ifaceInterfaces.indexOf(transitive) === -1) {
      context.reportError(transitive === type ? "Type ".concat(type.name, " cannot implement ").concat(iface.name, " because it would create a circular reference.") : "Type ".concat(type.name, " must implement ").concat(transitive.name, " because it is implemented by ").concat(iface.name, "."), [].concat(getAllImplementsInterfaceNodes(iface, transitive), getAllImplementsInterfaceNodes(type, iface)));
    }
  }
}

function validateUnionMembers(context, union) {
  var memberTypes = union.getTypes();

  if (memberTypes.length === 0) {
    context.reportError("Union type ".concat(union.name, " must define one or more member types."), getAllNodes(union));
  }

  var includedTypeNames = Object.create(null);

  for (var _i24 = 0; _i24 < memberTypes.length; _i24++) {
    var memberType = memberTypes[_i24];

    if (includedTypeNames[memberType.name]) {
      context.reportError("Union type ".concat(union.name, " can only include type ").concat(memberType.name, " once."), getUnionMemberTypeNodes(union, memberType.name));
      continue;
    }

    includedTypeNames[memberType.name] = true;

    if (!(0, _definition.isObjectType)(memberType)) {
      context.reportError("Union type ".concat(union.name, " can only include Object types, ") + "it cannot include ".concat((0, _inspect.default)(memberType), "."), getUnionMemberTypeNodes(union, String(memberType)));
    }
  }
}

function validateEnumValues(context, enumType) {
  var enumValues = enumType.getValues();

  if (enumValues.length === 0) {
    context.reportError("Enum type ".concat(enumType.name, " must define one or more values."), getAllNodes(enumType));
  }

  for (var _i26 = 0; _i26 < enumValues.length; _i26++) {
    var enumValue = enumValues[_i26];
    var valueName = enumValue.name; // Ensure valid name.

    validateName(context, enumValue);

    if (valueName === 'true' || valueName === 'false' || valueName === 'null') {
      context.reportError("Enum type ".concat(enumType.name, " cannot include value: ").concat(valueName, "."), enumValue.astNode);
    }
  }
}

function validateInputFields(context, inputObj) {
  var fields = (0, _objectValues5.default)(inputObj.getFields());

  if (fields.length === 0) {
    context.reportError("Input Object type ".concat(inputObj.name, " must define one or more fields."), getAllNodes(inputObj));
  } // Ensure the arguments are valid


  for (var _i28 = 0; _i28 < fields.length; _i28++) {
    var field = fields[_i28];
    // Ensure they are named correctly.
    validateName(context, field); // Ensure the type is an input type

    if (!(0, _definition.isInputType)(field.type)) {
      var _field$astNode2;

      context.reportError("The type of ".concat(inputObj.name, ".").concat(field.name, " must be Input Type ") + "but got: ".concat((0, _inspect.default)(field.type), "."), (_field$astNode2 = field.astNode) === null || _field$astNode2 === void 0 ? void 0 : _field$astNode2.type);
    }

    if ((0, _definition.isRequiredInputField)(field) && field.deprecationReason != null) {
      var _field$astNode3;

      context.reportError("Required input field ".concat(inputObj.name, ".").concat(field.name, " cannot be deprecated."), [getDeprecatedDirectiveNode(field.astNode), // istanbul ignore next (TODO need to write coverage tests)
      (_field$astNode3 = field.astNode) === null || _field$astNode3 === void 0 ? void 0 : _field$astNode3.type]);
    }
  }
}

function createInputObjectCircularRefsValidator(context) {
  // Modified copy of algorithm from 'src/validation/rules/NoFragmentCycles.js'.
  // Tracks already visited types to maintain O(N) and to ensure that cycles
  // are not redundantly reported.
  var visitedTypes = Object.create(null); // Array of types nodes used to produce meaningful errors

  var fieldPath = []; // Position in the type path

  var fieldPathIndexByTypeName = Object.create(null);
  return detectCycleRecursive; // This does a straight-forward DFS to find cycles.
  // It does not terminate when a cycle was found but continues to explore
  // the graph to find all possible cycles.

  function detectCycleRecursive(inputObj) {
    if (visitedTypes[inputObj.name]) {
      return;
    }

    visitedTypes[inputObj.name] = true;
    fieldPathIndexByTypeName[inputObj.name] = fieldPath.length;
    var fields = (0, _objectValues5.default)(inputObj.getFields());

    for (var _i30 = 0; _i30 < fields.length; _i30++) {
      var field = fields[_i30];

      if ((0, _definition.isNonNullType)(field.type) && (0, _definition.isInputObjectType)(field.type.ofType)) {
        var fieldType = field.type.ofType;
        var cycleIndex = fieldPathIndexByTypeName[fieldType.name];
        fieldPath.push(field);

        if (cycleIndex === undefined) {
          detectCycleRecursive(fieldType);
        } else {
          var cyclePath = fieldPath.slice(cycleIndex);
          var pathStr = cyclePath.map(function (fieldObj) {
            return fieldObj.name;
          }).join('.');
          context.reportError("Cannot reference Input Object \"".concat(fieldType.name, "\" within itself through a series of non-null fields: \"").concat(pathStr, "\"."), cyclePath.map(function (fieldObj) {
            return fieldObj.astNode;
          }));
        }

        fieldPath.pop();
      }
    }

    fieldPathIndexByTypeName[inputObj.name] = undefined;
  }
}

function getAllNodes(object) {
  var astNode = object.astNode,
      extensionASTNodes = object.extensionASTNodes;
  return astNode ? extensionASTNodes ? [astNode].concat(extensionASTNodes) : [astNode] : extensionASTNodes !== null && extensionASTNodes !== void 0 ? extensionASTNodes : [];
}

function getAllSubNodes(object, getter) {
  var subNodes = [];

  for (var _i32 = 0, _getAllNodes2 = getAllNodes(object); _i32 < _getAllNodes2.length; _i32++) {
    var _getter;

    var node = _getAllNodes2[_i32];
    // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')
    subNodes = subNodes.concat((_getter = getter(node)) !== null && _getter !== void 0 ? _getter : []);
  }

  return subNodes;
}

function getAllImplementsInterfaceNodes(type, iface) {
  return getAllSubNodes(type, function (typeNode) {
    return typeNode.interfaces;
  }).filter(function (ifaceNode) {
    return ifaceNode.name.value === iface.name;
  });
}

function getUnionMemberTypeNodes(union, typeName) {
  return getAllSubNodes(union, function (unionNode) {
    return unionNode.types;
  }).filter(function (typeNode) {
    return typeNode.name.value === typeName;
  });
}

function getDeprecatedDirectiveNode(definitionNode) {
  var _definitionNode$direc;

  // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')
  return definitionNode === null || definitionNode === void 0 ? void 0 : (_definitionNode$direc = definitionNode.directives) === null || _definitionNode$direc === void 0 ? void 0 : _definitionNode$direc.find(function (node) {
    return node.name.value === _directives.GraphQLDeprecatedDirective.name;
  });
}

},{"../error/GraphQLError.js":18,"../error/locatedError.js":21,"../jsutils/inspect.js":33,"../polyfills/find.js":66,"../polyfills/objectValues.js":70,"../utilities/assertValidName.js":83,"../utilities/typeComparators.js":101,"./definition.js":75,"./directives.js":76,"./introspection.js":78,"./schema.js":80}],82:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.visitWithTypeInfo = visitWithTypeInfo;
exports.TypeInfo = void 0;

var _find = _interopRequireDefault(require("../polyfills/find.js"));

var _kinds = require("../language/kinds.js");

var _ast = require("../language/ast.js");

var _visitor = require("../language/visitor.js");

var _definition = require("../type/definition.js");

var _introspection = require("../type/introspection.js");

var _typeFromAST = require("./typeFromAST.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * TypeInfo is a utility class which, given a GraphQL schema, can keep track
 * of the current field and type definitions at any point in a GraphQL document
 * AST during a recursive descent by calling `enter(node)` and `leave(node)`.
 */
var TypeInfo = /*#__PURE__*/function () {
  function TypeInfo(schema, // NOTE: this experimental optional second parameter is only needed in order
  // to support non-spec-compliant code bases. You should never need to use it.
  // It may disappear in the future.
  getFieldDefFn, // Initial type may be provided in rare cases to facilitate traversals
  // beginning somewhere other than documents.
  initialType) {
    this._schema = schema;
    this._typeStack = [];
    this._parentTypeStack = [];
    this._inputTypeStack = [];
    this._fieldDefStack = [];
    this._defaultValueStack = [];
    this._directive = null;
    this._argument = null;
    this._enumValue = null;
    this._getFieldDef = getFieldDefFn !== null && getFieldDefFn !== void 0 ? getFieldDefFn : getFieldDef;

    if (initialType) {
      if ((0, _definition.isInputType)(initialType)) {
        this._inputTypeStack.push(initialType);
      }

      if ((0, _definition.isCompositeType)(initialType)) {
        this._parentTypeStack.push(initialType);
      }

      if ((0, _definition.isOutputType)(initialType)) {
        this._typeStack.push(initialType);
      }
    }
  }

  var _proto = TypeInfo.prototype;

  _proto.getType = function getType() {
    if (this._typeStack.length > 0) {
      return this._typeStack[this._typeStack.length - 1];
    }
  };

  _proto.getParentType = function getParentType() {
    if (this._parentTypeStack.length > 0) {
      return this._parentTypeStack[this._parentTypeStack.length - 1];
    }
  };

  _proto.getInputType = function getInputType() {
    if (this._inputTypeStack.length > 0) {
      return this._inputTypeStack[this._inputTypeStack.length - 1];
    }
  };

  _proto.getParentInputType = function getParentInputType() {
    if (this._inputTypeStack.length > 1) {
      return this._inputTypeStack[this._inputTypeStack.length - 2];
    }
  };

  _proto.getFieldDef = function getFieldDef() {
    if (this._fieldDefStack.length > 0) {
      return this._fieldDefStack[this._fieldDefStack.length - 1];
    }
  };

  _proto.getDefaultValue = function getDefaultValue() {
    if (this._defaultValueStack.length > 0) {
      return this._defaultValueStack[this._defaultValueStack.length - 1];
    }
  };

  _proto.getDirective = function getDirective() {
    return this._directive;
  };

  _proto.getArgument = function getArgument() {
    return this._argument;
  };

  _proto.getEnumValue = function getEnumValue() {
    return this._enumValue;
  };

  _proto.enter = function enter(node) {
    var schema = this._schema; // Note: many of the types below are explicitly typed as "mixed" to drop
    // any assumptions of a valid schema to ensure runtime types are properly
    // checked before continuing since TypeInfo is used as part of validation
    // which occurs before guarantees of schema and document validity.

    switch (node.kind) {
      case _kinds.Kind.SELECTION_SET:
        {
          var namedType = (0, _definition.getNamedType)(this.getType());

          this._parentTypeStack.push((0, _definition.isCompositeType)(namedType) ? namedType : undefined);

          break;
        }

      case _kinds.Kind.FIELD:
        {
          var parentType = this.getParentType();
          var fieldDef;
          var fieldType;

          if (parentType) {
            fieldDef = this._getFieldDef(schema, parentType, node);

            if (fieldDef) {
              fieldType = fieldDef.type;
            }
          }

          this._fieldDefStack.push(fieldDef);

          this._typeStack.push((0, _definition.isOutputType)(fieldType) ? fieldType : undefined);

          break;
        }

      case _kinds.Kind.DIRECTIVE:
        this._directive = schema.getDirective(node.name.value);
        break;

      case _kinds.Kind.OPERATION_DEFINITION:
        {
          var type;

          switch (node.operation) {
            case 'query':
              type = schema.getQueryType();
              break;

            case 'mutation':
              type = schema.getMutationType();
              break;

            case 'subscription':
              type = schema.getSubscriptionType();
              break;
          }

          this._typeStack.push((0, _definition.isObjectType)(type) ? type : undefined);

          break;
        }

      case _kinds.Kind.INLINE_FRAGMENT:
      case _kinds.Kind.FRAGMENT_DEFINITION:
        {
          var typeConditionAST = node.typeCondition;
          var outputType = typeConditionAST ? (0, _typeFromAST.typeFromAST)(schema, typeConditionAST) : (0, _definition.getNamedType)(this.getType());

          this._typeStack.push((0, _definition.isOutputType)(outputType) ? outputType : undefined);

          break;
        }

      case _kinds.Kind.VARIABLE_DEFINITION:
        {
          var inputType = (0, _typeFromAST.typeFromAST)(schema, node.type);

          this._inputTypeStack.push((0, _definition.isInputType)(inputType) ? inputType : undefined);

          break;
        }

      case _kinds.Kind.ARGUMENT:
        {
          var _this$getDirective;

          var argDef;
          var argType;
          var fieldOrDirective = (_this$getDirective = this.getDirective()) !== null && _this$getDirective !== void 0 ? _this$getDirective : this.getFieldDef();

          if (fieldOrDirective) {
            argDef = (0, _find.default)(fieldOrDirective.args, function (arg) {
              return arg.name === node.name.value;
            });

            if (argDef) {
              argType = argDef.type;
            }
          }

          this._argument = argDef;

          this._defaultValueStack.push(argDef ? argDef.defaultValue : undefined);

          this._inputTypeStack.push((0, _definition.isInputType)(argType) ? argType : undefined);

          break;
        }

      case _kinds.Kind.LIST:
        {
          var listType = (0, _definition.getNullableType)(this.getInputType());
          var itemType = (0, _definition.isListType)(listType) ? listType.ofType : listType; // List positions never have a default value.

          this._defaultValueStack.push(undefined);

          this._inputTypeStack.push((0, _definition.isInputType)(itemType) ? itemType : undefined);

          break;
        }

      case _kinds.Kind.OBJECT_FIELD:
        {
          var objectType = (0, _definition.getNamedType)(this.getInputType());
          var inputFieldType;
          var inputField;

          if ((0, _definition.isInputObjectType)(objectType)) {
            inputField = objectType.getFields()[node.name.value];

            if (inputField) {
              inputFieldType = inputField.type;
            }
          }

          this._defaultValueStack.push(inputField ? inputField.defaultValue : undefined);

          this._inputTypeStack.push((0, _definition.isInputType)(inputFieldType) ? inputFieldType : undefined);

          break;
        }

      case _kinds.Kind.ENUM:
        {
          var enumType = (0, _definition.getNamedType)(this.getInputType());
          var enumValue;

          if ((0, _definition.isEnumType)(enumType)) {
            enumValue = enumType.getValue(node.value);
          }

          this._enumValue = enumValue;
          break;
        }
    }
  };

  _proto.leave = function leave(node) {
    switch (node.kind) {
      case _kinds.Kind.SELECTION_SET:
        this._parentTypeStack.pop();

        break;

      case _kinds.Kind.FIELD:
        this._fieldDefStack.pop();

        this._typeStack.pop();

        break;

      case _kinds.Kind.DIRECTIVE:
        this._directive = null;
        break;

      case _kinds.Kind.OPERATION_DEFINITION:
      case _kinds.Kind.INLINE_FRAGMENT:
      case _kinds.Kind.FRAGMENT_DEFINITION:
        this._typeStack.pop();

        break;

      case _kinds.Kind.VARIABLE_DEFINITION:
        this._inputTypeStack.pop();

        break;

      case _kinds.Kind.ARGUMENT:
        this._argument = null;

        this._defaultValueStack.pop();

        this._inputTypeStack.pop();

        break;

      case _kinds.Kind.LIST:
      case _kinds.Kind.OBJECT_FIELD:
        this._defaultValueStack.pop();

        this._inputTypeStack.pop();

        break;

      case _kinds.Kind.ENUM:
        this._enumValue = null;
        break;
    }
  };

  return TypeInfo;
}();
/**
 * Not exactly the same as the executor's definition of getFieldDef, in this
 * statically evaluated environment we do not always have an Object type,
 * and need to handle Interface and Union types.
 */


exports.TypeInfo = TypeInfo;

function getFieldDef(schema, parentType, fieldNode) {
  var name = fieldNode.name.value;

  if (name === _introspection.SchemaMetaFieldDef.name && schema.getQueryType() === parentType) {
    return _introspection.SchemaMetaFieldDef;
  }

  if (name === _introspection.TypeMetaFieldDef.name && schema.getQueryType() === parentType) {
    return _introspection.TypeMetaFieldDef;
  }

  if (name === _introspection.TypeNameMetaFieldDef.name && (0, _definition.isCompositeType)(parentType)) {
    return _introspection.TypeNameMetaFieldDef;
  }

  if ((0, _definition.isObjectType)(parentType) || (0, _definition.isInterfaceType)(parentType)) {
    return parentType.getFields()[name];
  }
}
/**
 * Creates a new visitor instance which maintains a provided TypeInfo instance
 * along with visiting visitor.
 */


function visitWithTypeInfo(typeInfo, visitor) {
  return {
    enter: function enter(node) {
      typeInfo.enter(node);
      var fn = (0, _visitor.getVisitFn)(visitor, node.kind,
      /* isLeaving */
      false);

      if (fn) {
        var result = fn.apply(visitor, arguments);

        if (result !== undefined) {
          typeInfo.leave(node);

          if ((0, _ast.isNode)(result)) {
            typeInfo.enter(result);
          }
        }

        return result;
      }
    },
    leave: function leave(node) {
      var fn = (0, _visitor.getVisitFn)(visitor, node.kind,
      /* isLeaving */
      true);
      var result;

      if (fn) {
        result = fn.apply(visitor, arguments);
      }

      typeInfo.leave(node);
      return result;
    }
  };
}

},{"../language/ast.js":51,"../language/kinds.js":55,"../language/visitor.js":64,"../polyfills/find.js":66,"../type/definition.js":75,"../type/introspection.js":78,"./typeFromAST.js":102}],83:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.assertValidName = assertValidName;
exports.isValidNameError = isValidNameError;

var _devAssert = _interopRequireDefault(require("../jsutils/devAssert.js"));

var _GraphQLError = require("../error/GraphQLError.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var NAME_RX = /^[_a-zA-Z][_a-zA-Z0-9]*$/;
/**
 * Upholds the spec rules about naming.
 */

function assertValidName(name) {
  var error = isValidNameError(name);

  if (error) {
    throw error;
  }

  return name;
}
/**
 * Returns an Error if a name is invalid.
 */


function isValidNameError(name) {
  typeof name === 'string' || (0, _devAssert.default)(0, 'Expected name to be a string.');

  if (name.length > 1 && name[0] === '_' && name[1] === '_') {
    return new _GraphQLError.GraphQLError("Name \"".concat(name, "\" must not begin with \"__\", which is reserved by GraphQL introspection."));
  }

  if (!NAME_RX.test(name)) {
    return new _GraphQLError.GraphQLError("Names must match /^[_a-zA-Z][_a-zA-Z0-9]*$/ but \"".concat(name, "\" does not."));
  }
}

},{"../error/GraphQLError.js":18,"../jsutils/devAssert.js":30}],84:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.astFromValue = astFromValue;

var _isFinite = _interopRequireDefault(require("../polyfills/isFinite.js"));

var _objectValues3 = _interopRequireDefault(require("../polyfills/objectValues.js"));

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _invariant = _interopRequireDefault(require("../jsutils/invariant.js"));

var _isObjectLike = _interopRequireDefault(require("../jsutils/isObjectLike.js"));

var _safeArrayFrom = _interopRequireDefault(require("../jsutils/safeArrayFrom.js"));

var _kinds = require("../language/kinds.js");

var _scalars = require("../type/scalars.js");

var _definition = require("../type/definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Produces a GraphQL Value AST given a JavaScript object.
 * Function will match JavaScript/JSON values to GraphQL AST schema format
 * by using suggested GraphQLInputType. For example:
 *
 *     astFromValue("value", GraphQLString)
 *
 * A GraphQL type must be provided, which will be used to interpret different
 * JavaScript values.
 *
 * | JSON Value    | GraphQL Value        |
 * | ------------- | -------------------- |
 * | Object        | Input Object         |
 * | Array         | List                 |
 * | Boolean       | Boolean              |
 * | String        | String / Enum Value  |
 * | Number        | Int / Float          |
 * | Mixed         | Enum Value           |
 * | null          | NullValue            |
 *
 */
function astFromValue(value, type) {
  if ((0, _definition.isNonNullType)(type)) {
    var astValue = astFromValue(value, type.ofType);

    if ((astValue === null || astValue === void 0 ? void 0 : astValue.kind) === _kinds.Kind.NULL) {
      return null;
    }

    return astValue;
  } // only explicit null, not undefined, NaN


  if (value === null) {
    return {
      kind: _kinds.Kind.NULL
    };
  } // undefined


  if (value === undefined) {
    return null;
  } // Convert JavaScript array to GraphQL list. If the GraphQLType is a list, but
  // the value is not an array, convert the value using the list's item type.


  if ((0, _definition.isListType)(type)) {
    var itemType = type.ofType;
    var items = (0, _safeArrayFrom.default)(value);

    if (items != null) {
      var valuesNodes = [];

      for (var _i2 = 0; _i2 < items.length; _i2++) {
        var item = items[_i2];
        var itemNode = astFromValue(item, itemType);

        if (itemNode != null) {
          valuesNodes.push(itemNode);
        }
      }

      return {
        kind: _kinds.Kind.LIST,
        values: valuesNodes
      };
    }

    return astFromValue(value, itemType);
  } // Populate the fields of the input object by creating ASTs from each value
  // in the JavaScript object according to the fields in the input type.


  if ((0, _definition.isInputObjectType)(type)) {
    if (!(0, _isObjectLike.default)(value)) {
      return null;
    }

    var fieldNodes = [];

    for (var _i4 = 0, _objectValues2 = (0, _objectValues3.default)(type.getFields()); _i4 < _objectValues2.length; _i4++) {
      var field = _objectValues2[_i4];
      var fieldValue = astFromValue(value[field.name], field.type);

      if (fieldValue) {
        fieldNodes.push({
          kind: _kinds.Kind.OBJECT_FIELD,
          name: {
            kind: _kinds.Kind.NAME,
            value: field.name
          },
          value: fieldValue
        });
      }
    }

    return {
      kind: _kinds.Kind.OBJECT,
      fields: fieldNodes
    };
  } // istanbul ignore else (See: 'https://github.com/graphql/graphql-js/issues/2618')


  if ((0, _definition.isLeafType)(type)) {
    // Since value is an internally represented value, it must be serialized
    // to an externally represented value before converting into an AST.
    var serialized = type.serialize(value);

    if (serialized == null) {
      return null;
    } // Others serialize based on their corresponding JavaScript scalar types.


    if (typeof serialized === 'boolean') {
      return {
        kind: _kinds.Kind.BOOLEAN,
        value: serialized
      };
    } // JavaScript numbers can be Int or Float values.


    if (typeof serialized === 'number' && (0, _isFinite.default)(serialized)) {
      var stringNum = String(serialized);
      return integerStringRegExp.test(stringNum) ? {
        kind: _kinds.Kind.INT,
        value: stringNum
      } : {
        kind: _kinds.Kind.FLOAT,
        value: stringNum
      };
    }

    if (typeof serialized === 'string') {
      // Enum types use Enum literals.
      if ((0, _definition.isEnumType)(type)) {
        return {
          kind: _kinds.Kind.ENUM,
          value: serialized
        };
      } // ID types can use Int literals.


      if (type === _scalars.GraphQLID && integerStringRegExp.test(serialized)) {
        return {
          kind: _kinds.Kind.INT,
          value: serialized
        };
      }

      return {
        kind: _kinds.Kind.STRING,
        value: serialized
      };
    }

    throw new TypeError("Cannot convert value to AST: ".concat((0, _inspect.default)(serialized), "."));
  } // istanbul ignore next (Not reachable. All possible input types have been considered)


  false || (0, _invariant.default)(0, 'Unexpected input type: ' + (0, _inspect.default)(type));
}
/**
 * IntValue:
 *   - NegativeSign? 0
 *   - NegativeSign? NonZeroDigit ( Digit+ )?
 */


var integerStringRegExp = /^-?(?:0|[1-9][0-9]*)$/;

},{"../jsutils/inspect.js":33,"../jsutils/invariant.js":35,"../jsutils/isObjectLike.js":37,"../jsutils/safeArrayFrom.js":48,"../language/kinds.js":55,"../polyfills/isFinite.js":67,"../polyfills/objectValues.js":70,"../type/definition.js":75,"../type/scalars.js":79}],85:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.buildASTSchema = buildASTSchema;
exports.buildSchema = buildSchema;

var _devAssert = _interopRequireDefault(require("../jsutils/devAssert.js"));

var _kinds = require("../language/kinds.js");

var _parser = require("../language/parser.js");

var _validate = require("../validation/validate.js");

var _schema = require("../type/schema.js");

var _directives = require("../type/directives.js");

var _extendSchema = require("./extendSchema.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * This takes the ast of a schema document produced by the parse function in
 * src/language/parser.js.
 *
 * If no schema definition is provided, then it will look for types named Query
 * and Mutation.
 *
 * Given that AST it constructs a GraphQLSchema. The resulting schema
 * has no resolve methods, so execution will use default resolvers.
 *
 * Accepts options as a second argument:
 *
 *    - commentDescriptions:
 *        Provide true to use preceding comments as the description.
 *
 */
function buildASTSchema(documentAST, options) {
  documentAST != null && documentAST.kind === _kinds.Kind.DOCUMENT || (0, _devAssert.default)(0, 'Must provide valid Document AST.');

  if ((options === null || options === void 0 ? void 0 : options.assumeValid) !== true && (options === null || options === void 0 ? void 0 : options.assumeValidSDL) !== true) {
    (0, _validate.assertValidSDL)(documentAST);
  }

  var emptySchemaConfig = {
    description: undefined,
    types: [],
    directives: [],
    extensions: undefined,
    extensionASTNodes: [],
    assumeValid: false
  };
  var config = (0, _extendSchema.extendSchemaImpl)(emptySchemaConfig, documentAST, options);

  if (config.astNode == null) {
    for (var _i2 = 0, _config$types2 = config.types; _i2 < _config$types2.length; _i2++) {
      var type = _config$types2[_i2];

      switch (type.name) {
        // Note: While this could make early assertions to get the correctly
        // typed values below, that would throw immediately while type system
        // validation with validateSchema() will produce more actionable results.
        case 'Query':
          config.query = type;
          break;

        case 'Mutation':
          config.mutation = type;
          break;

        case 'Subscription':
          config.subscription = type;
          break;
      }
    }
  }

  var directives = config.directives; // If specified directives were not explicitly declared, add them.

  var _loop = function _loop(_i4) {
    var stdDirective = _directives.specifiedDirectives[_i4];

    if (directives.every(function (directive) {
      return directive.name !== stdDirective.name;
    })) {
      directives.push(stdDirective);
    }
  };

  for (var _i4 = 0; _i4 < _directives.specifiedDirectives.length; _i4++) {
    _loop(_i4);
  }

  return new _schema.GraphQLSchema(config);
}
/**
 * A helper function to build a GraphQLSchema directly from a source
 * document.
 */


function buildSchema(source, options) {
  var document = (0, _parser.parse)(source, {
    noLocation: options === null || options === void 0 ? void 0 : options.noLocation,
    allowLegacySDLEmptyFields: options === null || options === void 0 ? void 0 : options.allowLegacySDLEmptyFields,
    allowLegacySDLImplementsInterfaces: options === null || options === void 0 ? void 0 : options.allowLegacySDLImplementsInterfaces,
    experimentalFragmentVariables: options === null || options === void 0 ? void 0 : options.experimentalFragmentVariables
  });
  return buildASTSchema(document, {
    commentDescriptions: options === null || options === void 0 ? void 0 : options.commentDescriptions,
    assumeValidSDL: options === null || options === void 0 ? void 0 : options.assumeValidSDL,
    assumeValid: options === null || options === void 0 ? void 0 : options.assumeValid
  });
}

},{"../jsutils/devAssert.js":30,"../language/kinds.js":55,"../language/parser.js":58,"../type/directives.js":76,"../type/schema.js":80,"../validation/validate.js":143,"./extendSchema.js":89}],86:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.buildClientSchema = buildClientSchema;

var _objectValues = _interopRequireDefault(require("../polyfills/objectValues.js"));

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _devAssert = _interopRequireDefault(require("../jsutils/devAssert.js"));

var _keyValMap = _interopRequireDefault(require("../jsutils/keyValMap.js"));

var _isObjectLike = _interopRequireDefault(require("../jsutils/isObjectLike.js"));

var _parser = require("../language/parser.js");

var _schema = require("../type/schema.js");

var _directives = require("../type/directives.js");

var _scalars = require("../type/scalars.js");

var _introspection = require("../type/introspection.js");

var _definition = require("../type/definition.js");

var _valueFromAST = require("./valueFromAST.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Build a GraphQLSchema for use by client tools.
 *
 * Given the result of a client running the introspection query, creates and
 * returns a GraphQLSchema instance which can be then used with all graphql-js
 * tools, but cannot be used to execute a query, as introspection does not
 * represent the "resolver", "parse" or "serialize" functions or any other
 * server-internal mechanisms.
 *
 * This function expects a complete introspection result. Don't forget to check
 * the "errors" field of a server response before calling this function.
 */
function buildClientSchema(introspection, options) {
  (0, _isObjectLike.default)(introspection) && (0, _isObjectLike.default)(introspection.__schema) || (0, _devAssert.default)(0, "Invalid or incomplete introspection result. Ensure that you are passing \"data\" property of introspection response and no \"errors\" was returned alongside: ".concat((0, _inspect.default)(introspection), ".")); // Get the schema from the introspection result.

  var schemaIntrospection = introspection.__schema; // Iterate through all types, getting the type definition for each.

  var typeMap = (0, _keyValMap.default)(schemaIntrospection.types, function (typeIntrospection) {
    return typeIntrospection.name;
  }, function (typeIntrospection) {
    return buildType(typeIntrospection);
  }); // Include standard types only if they are used.

  for (var _i2 = 0, _ref2 = [].concat(_scalars.specifiedScalarTypes, _introspection.introspectionTypes); _i2 < _ref2.length; _i2++) {
    var stdType = _ref2[_i2];

    if (typeMap[stdType.name]) {
      typeMap[stdType.name] = stdType;
    }
  } // Get the root Query, Mutation, and Subscription types.


  var queryType = schemaIntrospection.queryType ? getObjectType(schemaIntrospection.queryType) : null;
  var mutationType = schemaIntrospection.mutationType ? getObjectType(schemaIntrospection.mutationType) : null;
  var subscriptionType = schemaIntrospection.subscriptionType ? getObjectType(schemaIntrospection.subscriptionType) : null; // Get the directives supported by Introspection, assuming empty-set if
  // directives were not queried for.

  var directives = schemaIntrospection.directives ? schemaIntrospection.directives.map(buildDirective) : []; // Then produce and return a Schema with these types.

  return new _schema.GraphQLSchema({
    description: schemaIntrospection.description,
    query: queryType,
    mutation: mutationType,
    subscription: subscriptionType,
    types: (0, _objectValues.default)(typeMap),
    directives: directives,
    assumeValid: options === null || options === void 0 ? void 0 : options.assumeValid
  }); // Given a type reference in introspection, return the GraphQLType instance.
  // preferring cached instances before building new instances.

  function getType(typeRef) {
    if (typeRef.kind === _introspection.TypeKind.LIST) {
      var itemRef = typeRef.ofType;

      if (!itemRef) {
        throw new Error('Decorated type deeper than introspection query.');
      }

      return new _definition.GraphQLList(getType(itemRef));
    }

    if (typeRef.kind === _introspection.TypeKind.NON_NULL) {
      var nullableRef = typeRef.ofType;

      if (!nullableRef) {
        throw new Error('Decorated type deeper than introspection query.');
      }

      var nullableType = getType(nullableRef);
      return new _definition.GraphQLNonNull((0, _definition.assertNullableType)(nullableType));
    }

    return getNamedType(typeRef);
  }

  function getNamedType(typeRef) {
    var typeName = typeRef.name;

    if (!typeName) {
      throw new Error("Unknown type reference: ".concat((0, _inspect.default)(typeRef), "."));
    }

    var type = typeMap[typeName];

    if (!type) {
      throw new Error("Invalid or incomplete schema, unknown type: ".concat(typeName, ". Ensure that a full introspection query is used in order to build a client schema."));
    }

    return type;
  }

  function getObjectType(typeRef) {
    return (0, _definition.assertObjectType)(getNamedType(typeRef));
  }

  function getInterfaceType(typeRef) {
    return (0, _definition.assertInterfaceType)(getNamedType(typeRef));
  } // Given a type's introspection result, construct the correct
  // GraphQLType instance.


  function buildType(type) {
    if (type != null && type.name != null && type.kind != null) {
      switch (type.kind) {
        case _introspection.TypeKind.SCALAR:
          return buildScalarDef(type);

        case _introspection.TypeKind.OBJECT:
          return buildObjectDef(type);

        case _introspection.TypeKind.INTERFACE:
          return buildInterfaceDef(type);

        case _introspection.TypeKind.UNION:
          return buildUnionDef(type);

        case _introspection.TypeKind.ENUM:
          return buildEnumDef(type);

        case _introspection.TypeKind.INPUT_OBJECT:
          return buildInputObjectDef(type);
      }
    }

    var typeStr = (0, _inspect.default)(type);
    throw new Error("Invalid or incomplete introspection result. Ensure that a full introspection query is used in order to build a client schema: ".concat(typeStr, "."));
  }

  function buildScalarDef(scalarIntrospection) {
    return new _definition.GraphQLScalarType({
      name: scalarIntrospection.name,
      description: scalarIntrospection.description,
      specifiedByUrl: scalarIntrospection.specifiedByUrl
    });
  }

  function buildImplementationsList(implementingIntrospection) {
    // TODO: Temporary workaround until GraphQL ecosystem will fully support
    // 'interfaces' on interface types.
    if (implementingIntrospection.interfaces === null && implementingIntrospection.kind === _introspection.TypeKind.INTERFACE) {
      return [];
    }

    if (!implementingIntrospection.interfaces) {
      var implementingIntrospectionStr = (0, _inspect.default)(implementingIntrospection);
      throw new Error("Introspection result missing interfaces: ".concat(implementingIntrospectionStr, "."));
    }

    return implementingIntrospection.interfaces.map(getInterfaceType);
  }

  function buildObjectDef(objectIntrospection) {
    return new _definition.GraphQLObjectType({
      name: objectIntrospection.name,
      description: objectIntrospection.description,
      interfaces: function interfaces() {
        return buildImplementationsList(objectIntrospection);
      },
      fields: function fields() {
        return buildFieldDefMap(objectIntrospection);
      }
    });
  }

  function buildInterfaceDef(interfaceIntrospection) {
    return new _definition.GraphQLInterfaceType({
      name: interfaceIntrospection.name,
      description: interfaceIntrospection.description,
      interfaces: function interfaces() {
        return buildImplementationsList(interfaceIntrospection);
      },
      fields: function fields() {
        return buildFieldDefMap(interfaceIntrospection);
      }
    });
  }

  function buildUnionDef(unionIntrospection) {
    if (!unionIntrospection.possibleTypes) {
      var unionIntrospectionStr = (0, _inspect.default)(unionIntrospection);
      throw new Error("Introspection result missing possibleTypes: ".concat(unionIntrospectionStr, "."));
    }

    return new _definition.GraphQLUnionType({
      name: unionIntrospection.name,
      description: unionIntrospection.description,
      types: function types() {
        return unionIntrospection.possibleTypes.map(getObjectType);
      }
    });
  }

  function buildEnumDef(enumIntrospection) {
    if (!enumIntrospection.enumValues) {
      var enumIntrospectionStr = (0, _inspect.default)(enumIntrospection);
      throw new Error("Introspection result missing enumValues: ".concat(enumIntrospectionStr, "."));
    }

    return new _definition.GraphQLEnumType({
      name: enumIntrospection.name,
      description: enumIntrospection.description,
      values: (0, _keyValMap.default)(enumIntrospection.enumValues, function (valueIntrospection) {
        return valueIntrospection.name;
      }, function (valueIntrospection) {
        return {
          description: valueIntrospection.description,
          deprecationReason: valueIntrospection.deprecationReason
        };
      })
    });
  }

  function buildInputObjectDef(inputObjectIntrospection) {
    if (!inputObjectIntrospection.inputFields) {
      var inputObjectIntrospectionStr = (0, _inspect.default)(inputObjectIntrospection);
      throw new Error("Introspection result missing inputFields: ".concat(inputObjectIntrospectionStr, "."));
    }

    return new _definition.GraphQLInputObjectType({
      name: inputObjectIntrospection.name,
      description: inputObjectIntrospection.description,
      fields: function fields() {
        return buildInputValueDefMap(inputObjectIntrospection.inputFields);
      }
    });
  }

  function buildFieldDefMap(typeIntrospection) {
    if (!typeIntrospection.fields) {
      throw new Error("Introspection result missing fields: ".concat((0, _inspect.default)(typeIntrospection), "."));
    }

    return (0, _keyValMap.default)(typeIntrospection.fields, function (fieldIntrospection) {
      return fieldIntrospection.name;
    }, buildField);
  }

  function buildField(fieldIntrospection) {
    var type = getType(fieldIntrospection.type);

    if (!(0, _definition.isOutputType)(type)) {
      var typeStr = (0, _inspect.default)(type);
      throw new Error("Introspection must provide output type for fields, but received: ".concat(typeStr, "."));
    }

    if (!fieldIntrospection.args) {
      var fieldIntrospectionStr = (0, _inspect.default)(fieldIntrospection);
      throw new Error("Introspection result missing field args: ".concat(fieldIntrospectionStr, "."));
    }

    return {
      description: fieldIntrospection.description,
      deprecationReason: fieldIntrospection.deprecationReason,
      type: type,
      args: buildInputValueDefMap(fieldIntrospection.args)
    };
  }

  function buildInputValueDefMap(inputValueIntrospections) {
    return (0, _keyValMap.default)(inputValueIntrospections, function (inputValue) {
      return inputValue.name;
    }, buildInputValue);
  }

  function buildInputValue(inputValueIntrospection) {
    var type = getType(inputValueIntrospection.type);

    if (!(0, _definition.isInputType)(type)) {
      var typeStr = (0, _inspect.default)(type);
      throw new Error("Introspection must provide input type for arguments, but received: ".concat(typeStr, "."));
    }

    var defaultValue = inputValueIntrospection.defaultValue != null ? (0, _valueFromAST.valueFromAST)((0, _parser.parseValue)(inputValueIntrospection.defaultValue), type) : undefined;
    return {
      description: inputValueIntrospection.description,
      type: type,
      defaultValue: defaultValue,
      deprecationReason: inputValueIntrospection.deprecationReason
    };
  }

  function buildDirective(directiveIntrospection) {
    if (!directiveIntrospection.args) {
      var directiveIntrospectionStr = (0, _inspect.default)(directiveIntrospection);
      throw new Error("Introspection result missing directive args: ".concat(directiveIntrospectionStr, "."));
    }

    if (!directiveIntrospection.locations) {
      var _directiveIntrospectionStr = (0, _inspect.default)(directiveIntrospection);

      throw new Error("Introspection result missing directive locations: ".concat(_directiveIntrospectionStr, "."));
    }

    return new _directives.GraphQLDirective({
      name: directiveIntrospection.name,
      description: directiveIntrospection.description,
      isRepeatable: directiveIntrospection.isRepeatable,
      locations: directiveIntrospection.locations.slice(),
      args: buildInputValueDefMap(directiveIntrospection.args)
    });
  }
}

},{"../jsutils/devAssert.js":30,"../jsutils/inspect.js":33,"../jsutils/isObjectLike.js":37,"../jsutils/keyValMap.js":40,"../language/parser.js":58,"../polyfills/objectValues.js":70,"../type/definition.js":75,"../type/directives.js":76,"../type/introspection.js":78,"../type/scalars.js":79,"../type/schema.js":80,"./valueFromAST.js":103}],87:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.coerceInputValue = coerceInputValue;

var _objectValues3 = _interopRequireDefault(require("../polyfills/objectValues.js"));

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _invariant = _interopRequireDefault(require("../jsutils/invariant.js"));

var _didYouMean = _interopRequireDefault(require("../jsutils/didYouMean.js"));

var _isObjectLike = _interopRequireDefault(require("../jsutils/isObjectLike.js"));

var _safeArrayFrom = _interopRequireDefault(require("../jsutils/safeArrayFrom.js"));

var _suggestionList = _interopRequireDefault(require("../jsutils/suggestionList.js"));

var _printPathArray = _interopRequireDefault(require("../jsutils/printPathArray.js"));

var _Path = require("../jsutils/Path.js");

var _GraphQLError = require("../error/GraphQLError.js");

var _definition = require("../type/definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Coerces a JavaScript value given a GraphQL Input Type.
 */
function coerceInputValue(inputValue, type) {
  var onError = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : defaultOnError;
  return coerceInputValueImpl(inputValue, type, onError);
}

function defaultOnError(path, invalidValue, error) {
  var errorPrefix = 'Invalid value ' + (0, _inspect.default)(invalidValue);

  if (path.length > 0) {
    errorPrefix += " at \"value".concat((0, _printPathArray.default)(path), "\"");
  }

  error.message = errorPrefix + ': ' + error.message;
  throw error;
}

function coerceInputValueImpl(inputValue, type, onError, path) {
  if ((0, _definition.isNonNullType)(type)) {
    if (inputValue != null) {
      return coerceInputValueImpl(inputValue, type.ofType, onError, path);
    }

    onError((0, _Path.pathToArray)(path), inputValue, new _GraphQLError.GraphQLError("Expected non-nullable type \"".concat((0, _inspect.default)(type), "\" not to be null.")));
    return;
  }

  if (inputValue == null) {
    // Explicitly return the value null.
    return null;
  }

  if ((0, _definition.isListType)(type)) {
    var itemType = type.ofType;
    var coercedList = (0, _safeArrayFrom.default)(inputValue, function (itemValue, index) {
      var itemPath = (0, _Path.addPath)(path, index, undefined);
      return coerceInputValueImpl(itemValue, itemType, onError, itemPath);
    });

    if (coercedList != null) {
      return coercedList;
    } // Lists accept a non-list value as a list of one.


    return [coerceInputValueImpl(inputValue, itemType, onError, path)];
  }

  if ((0, _definition.isInputObjectType)(type)) {
    if (!(0, _isObjectLike.default)(inputValue)) {
      onError((0, _Path.pathToArray)(path), inputValue, new _GraphQLError.GraphQLError("Expected type \"".concat(type.name, "\" to be an object.")));
      return;
    }

    var coercedValue = {};
    var fieldDefs = type.getFields();

    for (var _i2 = 0, _objectValues2 = (0, _objectValues3.default)(fieldDefs); _i2 < _objectValues2.length; _i2++) {
      var field = _objectValues2[_i2];
      var fieldValue = inputValue[field.name];

      if (fieldValue === undefined) {
        if (field.defaultValue !== undefined) {
          coercedValue[field.name] = field.defaultValue;
        } else if ((0, _definition.isNonNullType)(field.type)) {
          var typeStr = (0, _inspect.default)(field.type);
          onError((0, _Path.pathToArray)(path), inputValue, new _GraphQLError.GraphQLError("Field \"".concat(field.name, "\" of required type \"").concat(typeStr, "\" was not provided.")));
        }

        continue;
      }

      coercedValue[field.name] = coerceInputValueImpl(fieldValue, field.type, onError, (0, _Path.addPath)(path, field.name, type.name));
    } // Ensure every provided field is defined.


    for (var _i4 = 0, _Object$keys2 = Object.keys(inputValue); _i4 < _Object$keys2.length; _i4++) {
      var fieldName = _Object$keys2[_i4];

      if (!fieldDefs[fieldName]) {
        var suggestions = (0, _suggestionList.default)(fieldName, Object.keys(type.getFields()));
        onError((0, _Path.pathToArray)(path), inputValue, new _GraphQLError.GraphQLError("Field \"".concat(fieldName, "\" is not defined by type \"").concat(type.name, "\".") + (0, _didYouMean.default)(suggestions)));
      }
    }

    return coercedValue;
  } // istanbul ignore else (See: 'https://github.com/graphql/graphql-js/issues/2618')


  if ((0, _definition.isLeafType)(type)) {
    var parseResult; // Scalars and Enums determine if a input value is valid via parseValue(),
    // which can throw to indicate failure. If it throws, maintain a reference
    // to the original error.

    try {
      parseResult = type.parseValue(inputValue);
    } catch (error) {
      if (error instanceof _GraphQLError.GraphQLError) {
        onError((0, _Path.pathToArray)(path), inputValue, error);
      } else {
        onError((0, _Path.pathToArray)(path), inputValue, new _GraphQLError.GraphQLError("Expected type \"".concat(type.name, "\". ") + error.message, undefined, undefined, undefined, undefined, error));
      }

      return;
    }

    if (parseResult === undefined) {
      onError((0, _Path.pathToArray)(path), inputValue, new _GraphQLError.GraphQLError("Expected type \"".concat(type.name, "\".")));
    }

    return parseResult;
  } // istanbul ignore next (Not reachable. All possible input types have been considered)


  false || (0, _invariant.default)(0, 'Unexpected input type: ' + (0, _inspect.default)(type));
}

},{"../error/GraphQLError.js":18,"../jsutils/Path.js":28,"../jsutils/didYouMean.js":31,"../jsutils/inspect.js":33,"../jsutils/invariant.js":35,"../jsutils/isObjectLike.js":37,"../jsutils/printPathArray.js":45,"../jsutils/safeArrayFrom.js":48,"../jsutils/suggestionList.js":49,"../polyfills/objectValues.js":70,"../type/definition.js":75}],88:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.concatAST = concatAST;

/**
 * Provided a collection of ASTs, presumably each from different files,
 * concatenate the ASTs together into batched AST, useful for validating many
 * GraphQL source files which together represent one conceptual application.
 */
function concatAST(documents) {
  var definitions = [];

  for (var _i2 = 0; _i2 < documents.length; _i2++) {
    var doc = documents[_i2];
    definitions = definitions.concat(doc.definitions);
  }

  return {
    kind: 'Document',
    definitions: definitions
  };
}

},{}],89:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.extendSchema = extendSchema;
exports.extendSchemaImpl = extendSchemaImpl;
exports.getDescription = getDescription;

var _objectValues = _interopRequireDefault(require("../polyfills/objectValues.js"));

var _keyMap = _interopRequireDefault(require("../jsutils/keyMap.js"));

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _mapValue = _interopRequireDefault(require("../jsutils/mapValue.js"));

var _invariant = _interopRequireDefault(require("../jsutils/invariant.js"));

var _devAssert = _interopRequireDefault(require("../jsutils/devAssert.js"));

var _kinds = require("../language/kinds.js");

var _tokenKind = require("../language/tokenKind.js");

var _blockString = require("../language/blockString.js");

var _predicates = require("../language/predicates.js");

var _validate = require("../validation/validate.js");

var _values = require("../execution/values.js");

var _schema = require("../type/schema.js");

var _scalars = require("../type/scalars.js");

var _introspection = require("../type/introspection.js");

var _directives = require("../type/directives.js");

var _definition = require("../type/definition.js");

var _valueFromAST = require("./valueFromAST.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function ownKeys(object, enumerableOnly) { var keys = Object.keys(object); if (Object.getOwnPropertySymbols) { var symbols = Object.getOwnPropertySymbols(object); if (enumerableOnly) symbols = symbols.filter(function (sym) { return Object.getOwnPropertyDescriptor(object, sym).enumerable; }); keys.push.apply(keys, symbols); } return keys; }

function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; if (i % 2) { ownKeys(Object(source), true).forEach(function (key) { _defineProperty(target, key, source[key]); }); } else if (Object.getOwnPropertyDescriptors) { Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)); } else { ownKeys(Object(source)).forEach(function (key) { Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key)); }); } } return target; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

/**
 * Produces a new schema given an existing schema and a document which may
 * contain GraphQL type extensions and definitions. The original schema will
 * remain unaltered.
 *
 * Because a schema represents a graph of references, a schema cannot be
 * extended without effectively making an entire copy. We do not know until it's
 * too late if subgraphs remain unchanged.
 *
 * This algorithm copies the provided schema, applying extensions while
 * producing the copy. The original schema remains unaltered.
 *
 * Accepts options as a third argument:
 *
 *    - commentDescriptions:
 *        Provide true to use preceding comments as the description.
 *
 */
function extendSchema(schema, documentAST, options) {
  (0, _schema.assertSchema)(schema);
  documentAST != null && documentAST.kind === _kinds.Kind.DOCUMENT || (0, _devAssert.default)(0, 'Must provide valid Document AST.');

  if ((options === null || options === void 0 ? void 0 : options.assumeValid) !== true && (options === null || options === void 0 ? void 0 : options.assumeValidSDL) !== true) {
    (0, _validate.assertValidSDLExtension)(documentAST, schema);
  }

  var schemaConfig = schema.toConfig();
  var extendedConfig = extendSchemaImpl(schemaConfig, documentAST, options);
  return schemaConfig === extendedConfig ? schema : new _schema.GraphQLSchema(extendedConfig);
}
/**
 * @internal
 */


function extendSchemaImpl(schemaConfig, documentAST, options) {
  var _schemaDef, _schemaDef$descriptio, _schemaDef2, _options$assumeValid;

  // Collect the type definitions and extensions found in the document.
  var typeDefs = [];
  var typeExtensionsMap = Object.create(null); // New directives and types are separate because a directives and types can
  // have the same name. For example, a type named "skip".

  var directiveDefs = [];
  var schemaDef; // Schema extensions are collected which may add additional operation types.

  var schemaExtensions = [];

  for (var _i2 = 0, _documentAST$definiti2 = documentAST.definitions; _i2 < _documentAST$definiti2.length; _i2++) {
    var def = _documentAST$definiti2[_i2];

    if (def.kind === _kinds.Kind.SCHEMA_DEFINITION) {
      schemaDef = def;
    } else if (def.kind === _kinds.Kind.SCHEMA_EXTENSION) {
      schemaExtensions.push(def);
    } else if ((0, _predicates.isTypeDefinitionNode)(def)) {
      typeDefs.push(def);
    } else if ((0, _predicates.isTypeExtensionNode)(def)) {
      var extendedTypeName = def.name.value;
      var existingTypeExtensions = typeExtensionsMap[extendedTypeName];
      typeExtensionsMap[extendedTypeName] = existingTypeExtensions ? existingTypeExtensions.concat([def]) : [def];
    } else if (def.kind === _kinds.Kind.DIRECTIVE_DEFINITION) {
      directiveDefs.push(def);
    }
  } // If this document contains no new types, extensions, or directives then
  // return the same unmodified GraphQLSchema instance.


  if (Object.keys(typeExtensionsMap).length === 0 && typeDefs.length === 0 && directiveDefs.length === 0 && schemaExtensions.length === 0 && schemaDef == null) {
    return schemaConfig;
  }

  var typeMap = Object.create(null);

  for (var _i4 = 0, _schemaConfig$types2 = schemaConfig.types; _i4 < _schemaConfig$types2.length; _i4++) {
    var existingType = _schemaConfig$types2[_i4];
    typeMap[existingType.name] = extendNamedType(existingType);
  }

  for (var _i6 = 0; _i6 < typeDefs.length; _i6++) {
    var _stdTypeMap$name;

    var typeNode = typeDefs[_i6];
    var name = typeNode.name.value;
    typeMap[name] = (_stdTypeMap$name = stdTypeMap[name]) !== null && _stdTypeMap$name !== void 0 ? _stdTypeMap$name : buildType(typeNode);
  }

  var operationTypes = _objectSpread(_objectSpread({
    // Get the extended root operation types.
    query: schemaConfig.query && replaceNamedType(schemaConfig.query),
    mutation: schemaConfig.mutation && replaceNamedType(schemaConfig.mutation),
    subscription: schemaConfig.subscription && replaceNamedType(schemaConfig.subscription)
  }, schemaDef && getOperationTypes([schemaDef])), getOperationTypes(schemaExtensions)); // Then produce and return a Schema config with these types.


  return _objectSpread(_objectSpread({
    description: (_schemaDef = schemaDef) === null || _schemaDef === void 0 ? void 0 : (_schemaDef$descriptio = _schemaDef.description) === null || _schemaDef$descriptio === void 0 ? void 0 : _schemaDef$descriptio.value
  }, operationTypes), {}, {
    types: (0, _objectValues.default)(typeMap),
    directives: [].concat(schemaConfig.directives.map(replaceDirective), directiveDefs.map(buildDirective)),
    extensions: undefined,
    astNode: (_schemaDef2 = schemaDef) !== null && _schemaDef2 !== void 0 ? _schemaDef2 : schemaConfig.astNode,
    extensionASTNodes: schemaConfig.extensionASTNodes.concat(schemaExtensions),
    assumeValid: (_options$assumeValid = options === null || options === void 0 ? void 0 : options.assumeValid) !== null && _options$assumeValid !== void 0 ? _options$assumeValid : false
  }); // Below are functions used for producing this schema that have closed over
  // this scope and have access to the schema, cache, and newly defined types.

  function replaceType(type) {
    if ((0, _definition.isListType)(type)) {
      // $FlowFixMe[incompatible-return]
      return new _definition.GraphQLList(replaceType(type.ofType));
    }

    if ((0, _definition.isNonNullType)(type)) {
      // $FlowFixMe[incompatible-return]
      return new _definition.GraphQLNonNull(replaceType(type.ofType));
    }

    return replaceNamedType(type);
  }

  function replaceNamedType(type) {
    // Note: While this could make early assertions to get the correctly
    // typed values, that would throw immediately while type system
    // validation with validateSchema() will produce more actionable results.
    return typeMap[type.name];
  }

  function replaceDirective(directive) {
    var config = directive.toConfig();
    return new _directives.GraphQLDirective(_objectSpread(_objectSpread({}, config), {}, {
      args: (0, _mapValue.default)(config.args, extendArg)
    }));
  }

  function extendNamedType(type) {
    if ((0, _introspection.isIntrospectionType)(type) || (0, _scalars.isSpecifiedScalarType)(type)) {
      // Builtin types are not extended.
      return type;
    }

    if ((0, _definition.isScalarType)(type)) {
      return extendScalarType(type);
    }

    if ((0, _definition.isObjectType)(type)) {
      return extendObjectType(type);
    }

    if ((0, _definition.isInterfaceType)(type)) {
      return extendInterfaceType(type);
    }

    if ((0, _definition.isUnionType)(type)) {
      return extendUnionType(type);
    }

    if ((0, _definition.isEnumType)(type)) {
      return extendEnumType(type);
    } // istanbul ignore else (See: 'https://github.com/graphql/graphql-js/issues/2618')


    if ((0, _definition.isInputObjectType)(type)) {
      return extendInputObjectType(type);
    } // istanbul ignore next (Not reachable. All possible types have been considered)


    false || (0, _invariant.default)(0, 'Unexpected type: ' + (0, _inspect.default)(type));
  }

  function extendInputObjectType(type) {
    var _typeExtensionsMap$co;

    var config = type.toConfig();
    var extensions = (_typeExtensionsMap$co = typeExtensionsMap[config.name]) !== null && _typeExtensionsMap$co !== void 0 ? _typeExtensionsMap$co : [];
    return new _definition.GraphQLInputObjectType(_objectSpread(_objectSpread({}, config), {}, {
      fields: function fields() {
        return _objectSpread(_objectSpread({}, (0, _mapValue.default)(config.fields, function (field) {
          return _objectSpread(_objectSpread({}, field), {}, {
            type: replaceType(field.type)
          });
        })), buildInputFieldMap(extensions));
      },
      extensionASTNodes: config.extensionASTNodes.concat(extensions)
    }));
  }

  function extendEnumType(type) {
    var _typeExtensionsMap$ty;

    var config = type.toConfig();
    var extensions = (_typeExtensionsMap$ty = typeExtensionsMap[type.name]) !== null && _typeExtensionsMap$ty !== void 0 ? _typeExtensionsMap$ty : [];
    return new _definition.GraphQLEnumType(_objectSpread(_objectSpread({}, config), {}, {
      values: _objectSpread(_objectSpread({}, config.values), buildEnumValueMap(extensions)),
      extensionASTNodes: config.extensionASTNodes.concat(extensions)
    }));
  }

  function extendScalarType(type) {
    var _typeExtensionsMap$co2;

    var config = type.toConfig();
    var extensions = (_typeExtensionsMap$co2 = typeExtensionsMap[config.name]) !== null && _typeExtensionsMap$co2 !== void 0 ? _typeExtensionsMap$co2 : [];
    var specifiedByUrl = config.specifiedByUrl;

    for (var _i8 = 0; _i8 < extensions.length; _i8++) {
      var _getSpecifiedByUrl;

      var extensionNode = extensions[_i8];
      specifiedByUrl = (_getSpecifiedByUrl = getSpecifiedByUrl(extensionNode)) !== null && _getSpecifiedByUrl !== void 0 ? _getSpecifiedByUrl : specifiedByUrl;
    }

    return new _definition.GraphQLScalarType(_objectSpread(_objectSpread({}, config), {}, {
      specifiedByUrl: specifiedByUrl,
      extensionASTNodes: config.extensionASTNodes.concat(extensions)
    }));
  }

  function extendObjectType(type) {
    var _typeExtensionsMap$co3;

    var config = type.toConfig();
    var extensions = (_typeExtensionsMap$co3 = typeExtensionsMap[config.name]) !== null && _typeExtensionsMap$co3 !== void 0 ? _typeExtensionsMap$co3 : [];
    return new _definition.GraphQLObjectType(_objectSpread(_objectSpread({}, config), {}, {
      interfaces: function interfaces() {
        return [].concat(type.getInterfaces().map(replaceNamedType), buildInterfaces(extensions));
      },
      fields: function fields() {
        return _objectSpread(_objectSpread({}, (0, _mapValue.default)(config.fields, extendField)), buildFieldMap(extensions));
      },
      extensionASTNodes: config.extensionASTNodes.concat(extensions)
    }));
  }

  function extendInterfaceType(type) {
    var _typeExtensionsMap$co4;

    var config = type.toConfig();
    var extensions = (_typeExtensionsMap$co4 = typeExtensionsMap[config.name]) !== null && _typeExtensionsMap$co4 !== void 0 ? _typeExtensionsMap$co4 : [];
    return new _definition.GraphQLInterfaceType(_objectSpread(_objectSpread({}, config), {}, {
      interfaces: function interfaces() {
        return [].concat(type.getInterfaces().map(replaceNamedType), buildInterfaces(extensions));
      },
      fields: function fields() {
        return _objectSpread(_objectSpread({}, (0, _mapValue.default)(config.fields, extendField)), buildFieldMap(extensions));
      },
      extensionASTNodes: config.extensionASTNodes.concat(extensions)
    }));
  }

  function extendUnionType(type) {
    var _typeExtensionsMap$co5;

    var config = type.toConfig();
    var extensions = (_typeExtensionsMap$co5 = typeExtensionsMap[config.name]) !== null && _typeExtensionsMap$co5 !== void 0 ? _typeExtensionsMap$co5 : [];
    return new _definition.GraphQLUnionType(_objectSpread(_objectSpread({}, config), {}, {
      types: function types() {
        return [].concat(type.getTypes().map(replaceNamedType), buildUnionTypes(extensions));
      },
      extensionASTNodes: config.extensionASTNodes.concat(extensions)
    }));
  }

  function extendField(field) {
    return _objectSpread(_objectSpread({}, field), {}, {
      type: replaceType(field.type),
      // $FlowFixMe[incompatible-call]
      args: (0, _mapValue.default)(field.args, extendArg)
    });
  }

  function extendArg(arg) {
    return _objectSpread(_objectSpread({}, arg), {}, {
      type: replaceType(arg.type)
    });
  }

  function getOperationTypes(nodes) {
    var opTypes = {};

    for (var _i10 = 0; _i10 < nodes.length; _i10++) {
      var _node$operationTypes;

      var node = nodes[_i10];
      // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')
      var operationTypesNodes = (_node$operationTypes = node.operationTypes) !== null && _node$operationTypes !== void 0 ? _node$operationTypes : [];

      for (var _i12 = 0; _i12 < operationTypesNodes.length; _i12++) {
        var operationType = operationTypesNodes[_i12];
        opTypes[operationType.operation] = getNamedType(operationType.type);
      }
    } // Note: While this could make early assertions to get the correctly
    // typed values below, that would throw immediately while type system
    // validation with validateSchema() will produce more actionable results.


    return opTypes;
  }

  function getNamedType(node) {
    var _stdTypeMap$name2;

    var name = node.name.value;
    var type = (_stdTypeMap$name2 = stdTypeMap[name]) !== null && _stdTypeMap$name2 !== void 0 ? _stdTypeMap$name2 : typeMap[name];

    if (type === undefined) {
      throw new Error("Unknown type: \"".concat(name, "\"."));
    }

    return type;
  }

  function getWrappedType(node) {
    if (node.kind === _kinds.Kind.LIST_TYPE) {
      return new _definition.GraphQLList(getWrappedType(node.type));
    }

    if (node.kind === _kinds.Kind.NON_NULL_TYPE) {
      return new _definition.GraphQLNonNull(getWrappedType(node.type));
    }

    return getNamedType(node);
  }

  function buildDirective(node) {
    var locations = node.locations.map(function (_ref) {
      var value = _ref.value;
      return value;
    });
    return new _directives.GraphQLDirective({
      name: node.name.value,
      description: getDescription(node, options),
      locations: locations,
      isRepeatable: node.repeatable,
      args: buildArgumentMap(node.arguments),
      astNode: node
    });
  }

  function buildFieldMap(nodes) {
    var fieldConfigMap = Object.create(null);

    for (var _i14 = 0; _i14 < nodes.length; _i14++) {
      var _node$fields;

      var node = nodes[_i14];
      // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')
      var nodeFields = (_node$fields = node.fields) !== null && _node$fields !== void 0 ? _node$fields : [];

      for (var _i16 = 0; _i16 < nodeFields.length; _i16++) {
        var field = nodeFields[_i16];
        fieldConfigMap[field.name.value] = {
          // Note: While this could make assertions to get the correctly typed
          // value, that would throw immediately while type system validation
          // with validateSchema() will produce more actionable results.
          type: getWrappedType(field.type),
          description: getDescription(field, options),
          args: buildArgumentMap(field.arguments),
          deprecationReason: getDeprecationReason(field),
          astNode: field
        };
      }
    }

    return fieldConfigMap;
  }

  function buildArgumentMap(args) {
    // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')
    var argsNodes = args !== null && args !== void 0 ? args : [];
    var argConfigMap = Object.create(null);

    for (var _i18 = 0; _i18 < argsNodes.length; _i18++) {
      var arg = argsNodes[_i18];
      // Note: While this could make assertions to get the correctly typed
      // value, that would throw immediately while type system validation
      // with validateSchema() will produce more actionable results.
      var type = getWrappedType(arg.type);
      argConfigMap[arg.name.value] = {
        type: type,
        description: getDescription(arg, options),
        defaultValue: (0, _valueFromAST.valueFromAST)(arg.defaultValue, type),
        deprecationReason: getDeprecationReason(arg),
        astNode: arg
      };
    }

    return argConfigMap;
  }

  function buildInputFieldMap(nodes) {
    var inputFieldMap = Object.create(null);

    for (var _i20 = 0; _i20 < nodes.length; _i20++) {
      var _node$fields2;

      var node = nodes[_i20];
      // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')
      var fieldsNodes = (_node$fields2 = node.fields) !== null && _node$fields2 !== void 0 ? _node$fields2 : [];

      for (var _i22 = 0; _i22 < fieldsNodes.length; _i22++) {
        var field = fieldsNodes[_i22];
        // Note: While this could make assertions to get the correctly typed
        // value, that would throw immediately while type system validation
        // with validateSchema() will produce more actionable results.
        var type = getWrappedType(field.type);
        inputFieldMap[field.name.value] = {
          type: type,
          description: getDescription(field, options),
          defaultValue: (0, _valueFromAST.valueFromAST)(field.defaultValue, type),
          deprecationReason: getDeprecationReason(field),
          astNode: field
        };
      }
    }

    return inputFieldMap;
  }

  function buildEnumValueMap(nodes) {
    var enumValueMap = Object.create(null);

    for (var _i24 = 0; _i24 < nodes.length; _i24++) {
      var _node$values;

      var node = nodes[_i24];
      // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')
      var valuesNodes = (_node$values = node.values) !== null && _node$values !== void 0 ? _node$values : [];

      for (var _i26 = 0; _i26 < valuesNodes.length; _i26++) {
        var value = valuesNodes[_i26];
        enumValueMap[value.name.value] = {
          description: getDescription(value, options),
          deprecationReason: getDeprecationReason(value),
          astNode: value
        };
      }
    }

    return enumValueMap;
  }

  function buildInterfaces(nodes) {
    var interfaces = [];

    for (var _i28 = 0; _i28 < nodes.length; _i28++) {
      var _node$interfaces;

      var node = nodes[_i28];
      // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')
      var interfacesNodes = (_node$interfaces = node.interfaces) !== null && _node$interfaces !== void 0 ? _node$interfaces : [];

      for (var _i30 = 0; _i30 < interfacesNodes.length; _i30++) {
        var type = interfacesNodes[_i30];
        // Note: While this could make assertions to get the correctly typed
        // values below, that would throw immediately while type system
        // validation with validateSchema() will produce more actionable
        // results.
        interfaces.push(getNamedType(type));
      }
    }

    return interfaces;
  }

  function buildUnionTypes(nodes) {
    var types = [];

    for (var _i32 = 0; _i32 < nodes.length; _i32++) {
      var _node$types;

      var node = nodes[_i32];
      // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')
      var typeNodes = (_node$types = node.types) !== null && _node$types !== void 0 ? _node$types : [];

      for (var _i34 = 0; _i34 < typeNodes.length; _i34++) {
        var type = typeNodes[_i34];
        // Note: While this could make assertions to get the correctly typed
        // values below, that would throw immediately while type system
        // validation with validateSchema() will produce more actionable
        // results.
        types.push(getNamedType(type));
      }
    }

    return types;
  }

  function buildType(astNode) {
    var _typeExtensionsMap$na;

    var name = astNode.name.value;
    var description = getDescription(astNode, options);
    var extensionNodes = (_typeExtensionsMap$na = typeExtensionsMap[name]) !== null && _typeExtensionsMap$na !== void 0 ? _typeExtensionsMap$na : [];

    switch (astNode.kind) {
      case _kinds.Kind.OBJECT_TYPE_DEFINITION:
        {
          var extensionASTNodes = extensionNodes;
          var allNodes = [astNode].concat(extensionASTNodes);
          return new _definition.GraphQLObjectType({
            name: name,
            description: description,
            interfaces: function interfaces() {
              return buildInterfaces(allNodes);
            },
            fields: function fields() {
              return buildFieldMap(allNodes);
            },
            astNode: astNode,
            extensionASTNodes: extensionASTNodes
          });
        }

      case _kinds.Kind.INTERFACE_TYPE_DEFINITION:
        {
          var _extensionASTNodes = extensionNodes;

          var _allNodes = [astNode].concat(_extensionASTNodes);

          return new _definition.GraphQLInterfaceType({
            name: name,
            description: description,
            interfaces: function interfaces() {
              return buildInterfaces(_allNodes);
            },
            fields: function fields() {
              return buildFieldMap(_allNodes);
            },
            astNode: astNode,
            extensionASTNodes: _extensionASTNodes
          });
        }

      case _kinds.Kind.ENUM_TYPE_DEFINITION:
        {
          var _extensionASTNodes2 = extensionNodes;

          var _allNodes2 = [astNode].concat(_extensionASTNodes2);

          return new _definition.GraphQLEnumType({
            name: name,
            description: description,
            values: buildEnumValueMap(_allNodes2),
            astNode: astNode,
            extensionASTNodes: _extensionASTNodes2
          });
        }

      case _kinds.Kind.UNION_TYPE_DEFINITION:
        {
          var _extensionASTNodes3 = extensionNodes;

          var _allNodes3 = [astNode].concat(_extensionASTNodes3);

          return new _definition.GraphQLUnionType({
            name: name,
            description: description,
            types: function types() {
              return buildUnionTypes(_allNodes3);
            },
            astNode: astNode,
            extensionASTNodes: _extensionASTNodes3
          });
        }

      case _kinds.Kind.SCALAR_TYPE_DEFINITION:
        {
          var _extensionASTNodes4 = extensionNodes;
          return new _definition.GraphQLScalarType({
            name: name,
            description: description,
            specifiedByUrl: getSpecifiedByUrl(astNode),
            astNode: astNode,
            extensionASTNodes: _extensionASTNodes4
          });
        }

      case _kinds.Kind.INPUT_OBJECT_TYPE_DEFINITION:
        {
          var _extensionASTNodes5 = extensionNodes;

          var _allNodes4 = [astNode].concat(_extensionASTNodes5);

          return new _definition.GraphQLInputObjectType({
            name: name,
            description: description,
            fields: function fields() {
              return buildInputFieldMap(_allNodes4);
            },
            astNode: astNode,
            extensionASTNodes: _extensionASTNodes5
          });
        }
    } // istanbul ignore next (Not reachable. All possible type definition nodes have been considered)


    false || (0, _invariant.default)(0, 'Unexpected type definition node: ' + (0, _inspect.default)(astNode));
  }
}

var stdTypeMap = (0, _keyMap.default)(_scalars.specifiedScalarTypes.concat(_introspection.introspectionTypes), function (type) {
  return type.name;
});
/**
 * Given a field or enum value node, returns the string value for the
 * deprecation reason.
 */

function getDeprecationReason(node) {
  var deprecated = (0, _values.getDirectiveValues)(_directives.GraphQLDeprecatedDirective, node);
  return deprecated === null || deprecated === void 0 ? void 0 : deprecated.reason;
}
/**
 * Given a scalar node, returns the string value for the specifiedByUrl.
 */


function getSpecifiedByUrl(node) {
  var specifiedBy = (0, _values.getDirectiveValues)(_directives.GraphQLSpecifiedByDirective, node);
  return specifiedBy === null || specifiedBy === void 0 ? void 0 : specifiedBy.url;
}
/**
 * Given an ast node, returns its string description.
 * @deprecated: provided to ease adoption and will be removed in v16.
 *
 * Accepts options as a second argument:
 *
 *    - commentDescriptions:
 *        Provide true to use preceding comments as the description.
 *
 */


function getDescription(node, options) {
  if (node.description) {
    return node.description.value;
  }

  if ((options === null || options === void 0 ? void 0 : options.commentDescriptions) === true) {
    var rawValue = getLeadingCommentBlock(node);

    if (rawValue !== undefined) {
      return (0, _blockString.dedentBlockStringValue)('\n' + rawValue);
    }
  }
}

function getLeadingCommentBlock(node) {
  var loc = node.loc;

  if (!loc) {
    return;
  }

  var comments = [];
  var token = loc.startToken.prev;

  while (token != null && token.kind === _tokenKind.TokenKind.COMMENT && token.next && token.prev && token.line + 1 === token.next.line && token.line !== token.prev.line) {
    var value = String(token.value);
    comments.push(value);
    token = token.prev;
  }

  return comments.length > 0 ? comments.reverse().join('\n') : undefined;
}

},{"../execution/values.js":25,"../jsutils/devAssert.js":30,"../jsutils/inspect.js":33,"../jsutils/invariant.js":35,"../jsutils/keyMap.js":39,"../jsutils/mapValue.js":41,"../language/blockString.js":52,"../language/kinds.js":55,"../language/predicates.js":59,"../language/tokenKind.js":63,"../polyfills/objectValues.js":70,"../type/definition.js":75,"../type/directives.js":76,"../type/introspection.js":78,"../type/scalars.js":79,"../type/schema.js":80,"../validation/validate.js":143,"./valueFromAST.js":103}],90:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.findBreakingChanges = findBreakingChanges;
exports.findDangerousChanges = findDangerousChanges;
exports.DangerousChangeType = exports.BreakingChangeType = void 0;

var _objectValues = _interopRequireDefault(require("../polyfills/objectValues.js"));

var _keyMap = _interopRequireDefault(require("../jsutils/keyMap.js"));

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _invariant = _interopRequireDefault(require("../jsutils/invariant.js"));

var _naturalCompare = _interopRequireDefault(require("../jsutils/naturalCompare.js"));

var _printer = require("../language/printer.js");

var _visitor = require("../language/visitor.js");

var _scalars = require("../type/scalars.js");

var _definition = require("../type/definition.js");

var _astFromValue = require("./astFromValue.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function ownKeys(object, enumerableOnly) { var keys = Object.keys(object); if (Object.getOwnPropertySymbols) { var symbols = Object.getOwnPropertySymbols(object); if (enumerableOnly) symbols = symbols.filter(function (sym) { return Object.getOwnPropertyDescriptor(object, sym).enumerable; }); keys.push.apply(keys, symbols); } return keys; }

function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; if (i % 2) { ownKeys(Object(source), true).forEach(function (key) { _defineProperty(target, key, source[key]); }); } else if (Object.getOwnPropertyDescriptors) { Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)); } else { ownKeys(Object(source)).forEach(function (key) { Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key)); }); } } return target; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

var BreakingChangeType = Object.freeze({
  TYPE_REMOVED: 'TYPE_REMOVED',
  TYPE_CHANGED_KIND: 'TYPE_CHANGED_KIND',
  TYPE_REMOVED_FROM_UNION: 'TYPE_REMOVED_FROM_UNION',
  VALUE_REMOVED_FROM_ENUM: 'VALUE_REMOVED_FROM_ENUM',
  REQUIRED_INPUT_FIELD_ADDED: 'REQUIRED_INPUT_FIELD_ADDED',
  IMPLEMENTED_INTERFACE_REMOVED: 'IMPLEMENTED_INTERFACE_REMOVED',
  FIELD_REMOVED: 'FIELD_REMOVED',
  FIELD_CHANGED_KIND: 'FIELD_CHANGED_KIND',
  REQUIRED_ARG_ADDED: 'REQUIRED_ARG_ADDED',
  ARG_REMOVED: 'ARG_REMOVED',
  ARG_CHANGED_KIND: 'ARG_CHANGED_KIND',
  DIRECTIVE_REMOVED: 'DIRECTIVE_REMOVED',
  DIRECTIVE_ARG_REMOVED: 'DIRECTIVE_ARG_REMOVED',
  REQUIRED_DIRECTIVE_ARG_ADDED: 'REQUIRED_DIRECTIVE_ARG_ADDED',
  DIRECTIVE_REPEATABLE_REMOVED: 'DIRECTIVE_REPEATABLE_REMOVED',
  DIRECTIVE_LOCATION_REMOVED: 'DIRECTIVE_LOCATION_REMOVED'
});
exports.BreakingChangeType = BreakingChangeType;
var DangerousChangeType = Object.freeze({
  VALUE_ADDED_TO_ENUM: 'VALUE_ADDED_TO_ENUM',
  TYPE_ADDED_TO_UNION: 'TYPE_ADDED_TO_UNION',
  OPTIONAL_INPUT_FIELD_ADDED: 'OPTIONAL_INPUT_FIELD_ADDED',
  OPTIONAL_ARG_ADDED: 'OPTIONAL_ARG_ADDED',
  IMPLEMENTED_INTERFACE_ADDED: 'IMPLEMENTED_INTERFACE_ADDED',
  ARG_DEFAULT_VALUE_CHANGE: 'ARG_DEFAULT_VALUE_CHANGE'
});
exports.DangerousChangeType = DangerousChangeType;

/**
 * Given two schemas, returns an Array containing descriptions of all the types
 * of breaking changes covered by the other functions down below.
 */
function findBreakingChanges(oldSchema, newSchema) {
  var breakingChanges = findSchemaChanges(oldSchema, newSchema).filter(function (change) {
    return change.type in BreakingChangeType;
  });
  return breakingChanges;
}
/**
 * Given two schemas, returns an Array containing descriptions of all the types
 * of potentially dangerous changes covered by the other functions down below.
 */


function findDangerousChanges(oldSchema, newSchema) {
  var dangerousChanges = findSchemaChanges(oldSchema, newSchema).filter(function (change) {
    return change.type in DangerousChangeType;
  });
  return dangerousChanges;
}

function findSchemaChanges(oldSchema, newSchema) {
  return [].concat(findTypeChanges(oldSchema, newSchema), findDirectiveChanges(oldSchema, newSchema));
}

function findDirectiveChanges(oldSchema, newSchema) {
  var schemaChanges = [];
  var directivesDiff = diff(oldSchema.getDirectives(), newSchema.getDirectives());

  for (var _i2 = 0, _directivesDiff$remov2 = directivesDiff.removed; _i2 < _directivesDiff$remov2.length; _i2++) {
    var oldDirective = _directivesDiff$remov2[_i2];
    schemaChanges.push({
      type: BreakingChangeType.DIRECTIVE_REMOVED,
      description: "".concat(oldDirective.name, " was removed.")
    });
  }

  for (var _i4 = 0, _directivesDiff$persi2 = directivesDiff.persisted; _i4 < _directivesDiff$persi2.length; _i4++) {
    var _ref2 = _directivesDiff$persi2[_i4];
    var _oldDirective = _ref2[0];
    var newDirective = _ref2[1];
    var argsDiff = diff(_oldDirective.args, newDirective.args);

    for (var _i6 = 0, _argsDiff$added2 = argsDiff.added; _i6 < _argsDiff$added2.length; _i6++) {
      var newArg = _argsDiff$added2[_i6];

      if ((0, _definition.isRequiredArgument)(newArg)) {
        schemaChanges.push({
          type: BreakingChangeType.REQUIRED_DIRECTIVE_ARG_ADDED,
          description: "A required arg ".concat(newArg.name, " on directive ").concat(_oldDirective.name, " was added.")
        });
      }
    }

    for (var _i8 = 0, _argsDiff$removed2 = argsDiff.removed; _i8 < _argsDiff$removed2.length; _i8++) {
      var oldArg = _argsDiff$removed2[_i8];
      schemaChanges.push({
        type: BreakingChangeType.DIRECTIVE_ARG_REMOVED,
        description: "".concat(oldArg.name, " was removed from ").concat(_oldDirective.name, ".")
      });
    }

    if (_oldDirective.isRepeatable && !newDirective.isRepeatable) {
      schemaChanges.push({
        type: BreakingChangeType.DIRECTIVE_REPEATABLE_REMOVED,
        description: "Repeatable flag was removed from ".concat(_oldDirective.name, ".")
      });
    }

    for (var _i10 = 0, _oldDirective$locatio2 = _oldDirective.locations; _i10 < _oldDirective$locatio2.length; _i10++) {
      var location = _oldDirective$locatio2[_i10];

      if (newDirective.locations.indexOf(location) === -1) {
        schemaChanges.push({
          type: BreakingChangeType.DIRECTIVE_LOCATION_REMOVED,
          description: "".concat(location, " was removed from ").concat(_oldDirective.name, ".")
        });
      }
    }
  }

  return schemaChanges;
}

function findTypeChanges(oldSchema, newSchema) {
  var schemaChanges = [];
  var typesDiff = diff((0, _objectValues.default)(oldSchema.getTypeMap()), (0, _objectValues.default)(newSchema.getTypeMap()));

  for (var _i12 = 0, _typesDiff$removed2 = typesDiff.removed; _i12 < _typesDiff$removed2.length; _i12++) {
    var oldType = _typesDiff$removed2[_i12];
    schemaChanges.push({
      type: BreakingChangeType.TYPE_REMOVED,
      description: (0, _scalars.isSpecifiedScalarType)(oldType) ? "Standard scalar ".concat(oldType.name, " was removed because it is not referenced anymore.") : "".concat(oldType.name, " was removed.")
    });
  }

  for (var _i14 = 0, _typesDiff$persisted2 = typesDiff.persisted; _i14 < _typesDiff$persisted2.length; _i14++) {
    var _ref4 = _typesDiff$persisted2[_i14];
    var _oldType = _ref4[0];
    var newType = _ref4[1];

    if ((0, _definition.isEnumType)(_oldType) && (0, _definition.isEnumType)(newType)) {
      schemaChanges.push.apply(schemaChanges, findEnumTypeChanges(_oldType, newType));
    } else if ((0, _definition.isUnionType)(_oldType) && (0, _definition.isUnionType)(newType)) {
      schemaChanges.push.apply(schemaChanges, findUnionTypeChanges(_oldType, newType));
    } else if ((0, _definition.isInputObjectType)(_oldType) && (0, _definition.isInputObjectType)(newType)) {
      schemaChanges.push.apply(schemaChanges, findInputObjectTypeChanges(_oldType, newType));
    } else if ((0, _definition.isObjectType)(_oldType) && (0, _definition.isObjectType)(newType)) {
      schemaChanges.push.apply(schemaChanges, findFieldChanges(_oldType, newType).concat(findImplementedInterfacesChanges(_oldType, newType)));
    } else if ((0, _definition.isInterfaceType)(_oldType) && (0, _definition.isInterfaceType)(newType)) {
      schemaChanges.push.apply(schemaChanges, findFieldChanges(_oldType, newType).concat(findImplementedInterfacesChanges(_oldType, newType)));
    } else if (_oldType.constructor !== newType.constructor) {
      schemaChanges.push({
        type: BreakingChangeType.TYPE_CHANGED_KIND,
        description: "".concat(_oldType.name, " changed from ") + "".concat(typeKindName(_oldType), " to ").concat(typeKindName(newType), ".")
      });
    }
  }

  return schemaChanges;
}

function findInputObjectTypeChanges(oldType, newType) {
  var schemaChanges = [];
  var fieldsDiff = diff((0, _objectValues.default)(oldType.getFields()), (0, _objectValues.default)(newType.getFields()));

  for (var _i16 = 0, _fieldsDiff$added2 = fieldsDiff.added; _i16 < _fieldsDiff$added2.length; _i16++) {
    var newField = _fieldsDiff$added2[_i16];

    if ((0, _definition.isRequiredInputField)(newField)) {
      schemaChanges.push({
        type: BreakingChangeType.REQUIRED_INPUT_FIELD_ADDED,
        description: "A required field ".concat(newField.name, " on input type ").concat(oldType.name, " was added.")
      });
    } else {
      schemaChanges.push({
        type: DangerousChangeType.OPTIONAL_INPUT_FIELD_ADDED,
        description: "An optional field ".concat(newField.name, " on input type ").concat(oldType.name, " was added.")
      });
    }
  }

  for (var _i18 = 0, _fieldsDiff$removed2 = fieldsDiff.removed; _i18 < _fieldsDiff$removed2.length; _i18++) {
    var oldField = _fieldsDiff$removed2[_i18];
    schemaChanges.push({
      type: BreakingChangeType.FIELD_REMOVED,
      description: "".concat(oldType.name, ".").concat(oldField.name, " was removed.")
    });
  }

  for (var _i20 = 0, _fieldsDiff$persisted2 = fieldsDiff.persisted; _i20 < _fieldsDiff$persisted2.length; _i20++) {
    var _ref6 = _fieldsDiff$persisted2[_i20];
    var _oldField = _ref6[0];
    var _newField = _ref6[1];
    var isSafe = isChangeSafeForInputObjectFieldOrFieldArg(_oldField.type, _newField.type);

    if (!isSafe) {
      schemaChanges.push({
        type: BreakingChangeType.FIELD_CHANGED_KIND,
        description: "".concat(oldType.name, ".").concat(_oldField.name, " changed type from ") + "".concat(String(_oldField.type), " to ").concat(String(_newField.type), ".")
      });
    }
  }

  return schemaChanges;
}

function findUnionTypeChanges(oldType, newType) {
  var schemaChanges = [];
  var possibleTypesDiff = diff(oldType.getTypes(), newType.getTypes());

  for (var _i22 = 0, _possibleTypesDiff$ad2 = possibleTypesDiff.added; _i22 < _possibleTypesDiff$ad2.length; _i22++) {
    var newPossibleType = _possibleTypesDiff$ad2[_i22];
    schemaChanges.push({
      type: DangerousChangeType.TYPE_ADDED_TO_UNION,
      description: "".concat(newPossibleType.name, " was added to union type ").concat(oldType.name, ".")
    });
  }

  for (var _i24 = 0, _possibleTypesDiff$re2 = possibleTypesDiff.removed; _i24 < _possibleTypesDiff$re2.length; _i24++) {
    var oldPossibleType = _possibleTypesDiff$re2[_i24];
    schemaChanges.push({
      type: BreakingChangeType.TYPE_REMOVED_FROM_UNION,
      description: "".concat(oldPossibleType.name, " was removed from union type ").concat(oldType.name, ".")
    });
  }

  return schemaChanges;
}

function findEnumTypeChanges(oldType, newType) {
  var schemaChanges = [];
  var valuesDiff = diff(oldType.getValues(), newType.getValues());

  for (var _i26 = 0, _valuesDiff$added2 = valuesDiff.added; _i26 < _valuesDiff$added2.length; _i26++) {
    var newValue = _valuesDiff$added2[_i26];
    schemaChanges.push({
      type: DangerousChangeType.VALUE_ADDED_TO_ENUM,
      description: "".concat(newValue.name, " was added to enum type ").concat(oldType.name, ".")
    });
  }

  for (var _i28 = 0, _valuesDiff$removed2 = valuesDiff.removed; _i28 < _valuesDiff$removed2.length; _i28++) {
    var oldValue = _valuesDiff$removed2[_i28];
    schemaChanges.push({
      type: BreakingChangeType.VALUE_REMOVED_FROM_ENUM,
      description: "".concat(oldValue.name, " was removed from enum type ").concat(oldType.name, ".")
    });
  }

  return schemaChanges;
}

function findImplementedInterfacesChanges(oldType, newType) {
  var schemaChanges = [];
  var interfacesDiff = diff(oldType.getInterfaces(), newType.getInterfaces());

  for (var _i30 = 0, _interfacesDiff$added2 = interfacesDiff.added; _i30 < _interfacesDiff$added2.length; _i30++) {
    var newInterface = _interfacesDiff$added2[_i30];
    schemaChanges.push({
      type: DangerousChangeType.IMPLEMENTED_INTERFACE_ADDED,
      description: "".concat(newInterface.name, " added to interfaces implemented by ").concat(oldType.name, ".")
    });
  }

  for (var _i32 = 0, _interfacesDiff$remov2 = interfacesDiff.removed; _i32 < _interfacesDiff$remov2.length; _i32++) {
    var oldInterface = _interfacesDiff$remov2[_i32];
    schemaChanges.push({
      type: BreakingChangeType.IMPLEMENTED_INTERFACE_REMOVED,
      description: "".concat(oldType.name, " no longer implements interface ").concat(oldInterface.name, ".")
    });
  }

  return schemaChanges;
}

function findFieldChanges(oldType, newType) {
  var schemaChanges = [];
  var fieldsDiff = diff((0, _objectValues.default)(oldType.getFields()), (0, _objectValues.default)(newType.getFields()));

  for (var _i34 = 0, _fieldsDiff$removed4 = fieldsDiff.removed; _i34 < _fieldsDiff$removed4.length; _i34++) {
    var oldField = _fieldsDiff$removed4[_i34];
    schemaChanges.push({
      type: BreakingChangeType.FIELD_REMOVED,
      description: "".concat(oldType.name, ".").concat(oldField.name, " was removed.")
    });
  }

  for (var _i36 = 0, _fieldsDiff$persisted4 = fieldsDiff.persisted; _i36 < _fieldsDiff$persisted4.length; _i36++) {
    var _ref8 = _fieldsDiff$persisted4[_i36];
    var _oldField2 = _ref8[0];
    var newField = _ref8[1];
    schemaChanges.push.apply(schemaChanges, findArgChanges(oldType, _oldField2, newField));
    var isSafe = isChangeSafeForObjectOrInterfaceField(_oldField2.type, newField.type);

    if (!isSafe) {
      schemaChanges.push({
        type: BreakingChangeType.FIELD_CHANGED_KIND,
        description: "".concat(oldType.name, ".").concat(_oldField2.name, " changed type from ") + "".concat(String(_oldField2.type), " to ").concat(String(newField.type), ".")
      });
    }
  }

  return schemaChanges;
}

function findArgChanges(oldType, oldField, newField) {
  var schemaChanges = [];
  var argsDiff = diff(oldField.args, newField.args);

  for (var _i38 = 0, _argsDiff$removed4 = argsDiff.removed; _i38 < _argsDiff$removed4.length; _i38++) {
    var oldArg = _argsDiff$removed4[_i38];
    schemaChanges.push({
      type: BreakingChangeType.ARG_REMOVED,
      description: "".concat(oldType.name, ".").concat(oldField.name, " arg ").concat(oldArg.name, " was removed.")
    });
  }

  for (var _i40 = 0, _argsDiff$persisted2 = argsDiff.persisted; _i40 < _argsDiff$persisted2.length; _i40++) {
    var _ref10 = _argsDiff$persisted2[_i40];
    var _oldArg = _ref10[0];
    var newArg = _ref10[1];
    var isSafe = isChangeSafeForInputObjectFieldOrFieldArg(_oldArg.type, newArg.type);

    if (!isSafe) {
      schemaChanges.push({
        type: BreakingChangeType.ARG_CHANGED_KIND,
        description: "".concat(oldType.name, ".").concat(oldField.name, " arg ").concat(_oldArg.name, " has changed type from ") + "".concat(String(_oldArg.type), " to ").concat(String(newArg.type), ".")
      });
    } else if (_oldArg.defaultValue !== undefined) {
      if (newArg.defaultValue === undefined) {
        schemaChanges.push({
          type: DangerousChangeType.ARG_DEFAULT_VALUE_CHANGE,
          description: "".concat(oldType.name, ".").concat(oldField.name, " arg ").concat(_oldArg.name, " defaultValue was removed.")
        });
      } else {
        // Since we looking only for client's observable changes we should
        // compare default values in the same representation as they are
        // represented inside introspection.
        var oldValueStr = stringifyValue(_oldArg.defaultValue, _oldArg.type);
        var newValueStr = stringifyValue(newArg.defaultValue, newArg.type);

        if (oldValueStr !== newValueStr) {
          schemaChanges.push({
            type: DangerousChangeType.ARG_DEFAULT_VALUE_CHANGE,
            description: "".concat(oldType.name, ".").concat(oldField.name, " arg ").concat(_oldArg.name, " has changed defaultValue from ").concat(oldValueStr, " to ").concat(newValueStr, ".")
          });
        }
      }
    }
  }

  for (var _i42 = 0, _argsDiff$added4 = argsDiff.added; _i42 < _argsDiff$added4.length; _i42++) {
    var _newArg = _argsDiff$added4[_i42];

    if ((0, _definition.isRequiredArgument)(_newArg)) {
      schemaChanges.push({
        type: BreakingChangeType.REQUIRED_ARG_ADDED,
        description: "A required arg ".concat(_newArg.name, " on ").concat(oldType.name, ".").concat(oldField.name, " was added.")
      });
    } else {
      schemaChanges.push({
        type: DangerousChangeType.OPTIONAL_ARG_ADDED,
        description: "An optional arg ".concat(_newArg.name, " on ").concat(oldType.name, ".").concat(oldField.name, " was added.")
      });
    }
  }

  return schemaChanges;
}

function isChangeSafeForObjectOrInterfaceField(oldType, newType) {
  if ((0, _definition.isListType)(oldType)) {
    return (// if they're both lists, make sure the underlying types are compatible
      (0, _definition.isListType)(newType) && isChangeSafeForObjectOrInterfaceField(oldType.ofType, newType.ofType) || // moving from nullable to non-null of the same underlying type is safe
      (0, _definition.isNonNullType)(newType) && isChangeSafeForObjectOrInterfaceField(oldType, newType.ofType)
    );
  }

  if ((0, _definition.isNonNullType)(oldType)) {
    // if they're both non-null, make sure the underlying types are compatible
    return (0, _definition.isNonNullType)(newType) && isChangeSafeForObjectOrInterfaceField(oldType.ofType, newType.ofType);
  }

  return (// if they're both named types, see if their names are equivalent
    (0, _definition.isNamedType)(newType) && oldType.name === newType.name || // moving from nullable to non-null of the same underlying type is safe
    (0, _definition.isNonNullType)(newType) && isChangeSafeForObjectOrInterfaceField(oldType, newType.ofType)
  );
}

function isChangeSafeForInputObjectFieldOrFieldArg(oldType, newType) {
  if ((0, _definition.isListType)(oldType)) {
    // if they're both lists, make sure the underlying types are compatible
    return (0, _definition.isListType)(newType) && isChangeSafeForInputObjectFieldOrFieldArg(oldType.ofType, newType.ofType);
  }

  if ((0, _definition.isNonNullType)(oldType)) {
    return (// if they're both non-null, make sure the underlying types are
      // compatible
      (0, _definition.isNonNullType)(newType) && isChangeSafeForInputObjectFieldOrFieldArg(oldType.ofType, newType.ofType) || // moving from non-null to nullable of the same underlying type is safe
      !(0, _definition.isNonNullType)(newType) && isChangeSafeForInputObjectFieldOrFieldArg(oldType.ofType, newType)
    );
  } // if they're both named types, see if their names are equivalent


  return (0, _definition.isNamedType)(newType) && oldType.name === newType.name;
}

function typeKindName(type) {
  if ((0, _definition.isScalarType)(type)) {
    return 'a Scalar type';
  }

  if ((0, _definition.isObjectType)(type)) {
    return 'an Object type';
  }

  if ((0, _definition.isInterfaceType)(type)) {
    return 'an Interface type';
  }

  if ((0, _definition.isUnionType)(type)) {
    return 'a Union type';
  }

  if ((0, _definition.isEnumType)(type)) {
    return 'an Enum type';
  } // istanbul ignore else (See: 'https://github.com/graphql/graphql-js/issues/2618')


  if ((0, _definition.isInputObjectType)(type)) {
    return 'an Input type';
  } // istanbul ignore next (Not reachable. All possible named types have been considered)


  false || (0, _invariant.default)(0, 'Unexpected type: ' + (0, _inspect.default)(type));
}

function stringifyValue(value, type) {
  var ast = (0, _astFromValue.astFromValue)(value, type);
  ast != null || (0, _invariant.default)(0);
  var sortedAST = (0, _visitor.visit)(ast, {
    ObjectValue: function ObjectValue(objectNode) {
      // Make a copy since sort mutates array
      var fields = [].concat(objectNode.fields);
      fields.sort(function (fieldA, fieldB) {
        return (0, _naturalCompare.default)(fieldA.name.value, fieldB.name.value);
      });
      return _objectSpread(_objectSpread({}, objectNode), {}, {
        fields: fields
      });
    }
  });
  return (0, _printer.print)(sortedAST);
}

function diff(oldArray, newArray) {
  var added = [];
  var removed = [];
  var persisted = [];
  var oldMap = (0, _keyMap.default)(oldArray, function (_ref11) {
    var name = _ref11.name;
    return name;
  });
  var newMap = (0, _keyMap.default)(newArray, function (_ref12) {
    var name = _ref12.name;
    return name;
  });

  for (var _i44 = 0; _i44 < oldArray.length; _i44++) {
    var oldItem = oldArray[_i44];
    var newItem = newMap[oldItem.name];

    if (newItem === undefined) {
      removed.push(oldItem);
    } else {
      persisted.push([oldItem, newItem]);
    }
  }

  for (var _i46 = 0; _i46 < newArray.length; _i46++) {
    var _newItem = newArray[_i46];

    if (oldMap[_newItem.name] === undefined) {
      added.push(_newItem);
    }
  }

  return {
    added: added,
    persisted: persisted,
    removed: removed
  };
}

},{"../jsutils/inspect.js":33,"../jsutils/invariant.js":35,"../jsutils/keyMap.js":39,"../jsutils/naturalCompare.js":43,"../language/printer.js":61,"../language/visitor.js":64,"../polyfills/objectValues.js":70,"../type/definition.js":75,"../type/scalars.js":79,"./astFromValue.js":84}],91:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.findDeprecatedUsages = findDeprecatedUsages;

var _validate = require("../validation/validate.js");

var _NoDeprecatedCustomRule = require("../validation/rules/custom/NoDeprecatedCustomRule.js");

/**
 * A validation rule which reports deprecated usages.
 *
 * Returns a list of GraphQLError instances describing each deprecated use.
 *
 * @deprecated Please use `validate` with `NoDeprecatedCustomRule` instead:
 *
 * ```
 * import { validate, NoDeprecatedCustomRule } from 'graphql'
 *
 * const errors = validate(schema, document, [NoDeprecatedCustomRule])
 * ```
 */
function findDeprecatedUsages(schema, ast) {
  return (0, _validate.validate)(schema, ast, [_NoDeprecatedCustomRule.NoDeprecatedCustomRule]);
}

},{"../validation/rules/custom/NoDeprecatedCustomRule.js":140,"../validation/validate.js":143}],92:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.getIntrospectionQuery = getIntrospectionQuery;

function ownKeys(object, enumerableOnly) { var keys = Object.keys(object); if (Object.getOwnPropertySymbols) { var symbols = Object.getOwnPropertySymbols(object); if (enumerableOnly) symbols = symbols.filter(function (sym) { return Object.getOwnPropertyDescriptor(object, sym).enumerable; }); keys.push.apply(keys, symbols); } return keys; }

function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; if (i % 2) { ownKeys(Object(source), true).forEach(function (key) { _defineProperty(target, key, source[key]); }); } else if (Object.getOwnPropertyDescriptors) { Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)); } else { ownKeys(Object(source)).forEach(function (key) { Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key)); }); } } return target; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

function getIntrospectionQuery(options) {
  var optionsWithDefault = _objectSpread({
    descriptions: true,
    specifiedByUrl: false,
    directiveIsRepeatable: false,
    schemaDescription: false,
    inputValueDeprecation: false
  }, options);

  var descriptions = optionsWithDefault.descriptions ? 'description' : '';
  var specifiedByUrl = optionsWithDefault.specifiedByUrl ? 'specifiedByUrl' : '';
  var directiveIsRepeatable = optionsWithDefault.directiveIsRepeatable ? 'isRepeatable' : '';
  var schemaDescription = optionsWithDefault.schemaDescription ? descriptions : '';

  function inputDeprecation(str) {
    return optionsWithDefault.inputValueDeprecation ? str : '';
  }

  return "\n    query IntrospectionQuery {\n      __schema {\n        ".concat(schemaDescription, "\n        queryType { name }\n        mutationType { name }\n        subscriptionType { name }\n        types {\n          ...FullType\n        }\n        directives {\n          name\n          ").concat(descriptions, "\n          ").concat(directiveIsRepeatable, "\n          locations\n          args").concat(inputDeprecation('(includeDeprecated: true)'), " {\n            ...InputValue\n          }\n        }\n      }\n    }\n\n    fragment FullType on __Type {\n      kind\n      name\n      ").concat(descriptions, "\n      ").concat(specifiedByUrl, "\n      fields(includeDeprecated: true) {\n        name\n        ").concat(descriptions, "\n        args").concat(inputDeprecation('(includeDeprecated: true)'), " {\n          ...InputValue\n        }\n        type {\n          ...TypeRef\n        }\n        isDeprecated\n        deprecationReason\n      }\n      inputFields").concat(inputDeprecation('(includeDeprecated: true)'), " {\n        ...InputValue\n      }\n      interfaces {\n        ...TypeRef\n      }\n      enumValues(includeDeprecated: true) {\n        name\n        ").concat(descriptions, "\n        isDeprecated\n        deprecationReason\n      }\n      possibleTypes {\n        ...TypeRef\n      }\n    }\n\n    fragment InputValue on __InputValue {\n      name\n      ").concat(descriptions, "\n      type { ...TypeRef }\n      defaultValue\n      ").concat(inputDeprecation('isDeprecated'), "\n      ").concat(inputDeprecation('deprecationReason'), "\n    }\n\n    fragment TypeRef on __Type {\n      kind\n      name\n      ofType {\n        kind\n        name\n        ofType {\n          kind\n          name\n          ofType {\n            kind\n            name\n            ofType {\n              kind\n              name\n              ofType {\n                kind\n                name\n                ofType {\n                  kind\n                  name\n                  ofType {\n                    kind\n                    name\n                  }\n                }\n              }\n            }\n          }\n        }\n      }\n    }\n  ");
}

},{}],93:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.getOperationAST = getOperationAST;

var _kinds = require("../language/kinds.js");

/**
 * Returns an operation AST given a document AST and optionally an operation
 * name. If a name is not provided, an operation is only returned if only one is
 * provided in the document.
 */
function getOperationAST(documentAST, operationName) {
  var operation = null;

  for (var _i2 = 0, _documentAST$definiti2 = documentAST.definitions; _i2 < _documentAST$definiti2.length; _i2++) {
    var definition = _documentAST$definiti2[_i2];

    if (definition.kind === _kinds.Kind.OPERATION_DEFINITION) {
      var _definition$name;

      if (operationName == null) {
        // If no operation name was provided, only return an Operation if there
        // is one defined in the document. Upon encountering the second, return
        // null.
        if (operation) {
          return null;
        }

        operation = definition;
      } else if (((_definition$name = definition.name) === null || _definition$name === void 0 ? void 0 : _definition$name.value) === operationName) {
        return definition;
      }
    }
  }

  return operation;
}

},{"../language/kinds.js":55}],94:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.getOperationRootType = getOperationRootType;

var _GraphQLError = require("../error/GraphQLError.js");

/**
 * Extracts the root type of the operation from the schema.
 */
function getOperationRootType(schema, operation) {
  if (operation.operation === 'query') {
    var queryType = schema.getQueryType();

    if (!queryType) {
      throw new _GraphQLError.GraphQLError('Schema does not define the required query root type.', operation);
    }

    return queryType;
  }

  if (operation.operation === 'mutation') {
    var mutationType = schema.getMutationType();

    if (!mutationType) {
      throw new _GraphQLError.GraphQLError('Schema is not configured for mutations.', operation);
    }

    return mutationType;
  }

  if (operation.operation === 'subscription') {
    var subscriptionType = schema.getSubscriptionType();

    if (!subscriptionType) {
      throw new _GraphQLError.GraphQLError('Schema is not configured for subscriptions.', operation);
    }

    return subscriptionType;
  }

  throw new _GraphQLError.GraphQLError('Can only have query, mutation and subscription operations.', operation);
}

},{"../error/GraphQLError.js":18}],95:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "getIntrospectionQuery", {
  enumerable: true,
  get: function get() {
    return _getIntrospectionQuery.getIntrospectionQuery;
  }
});
Object.defineProperty(exports, "getOperationAST", {
  enumerable: true,
  get: function get() {
    return _getOperationAST.getOperationAST;
  }
});
Object.defineProperty(exports, "getOperationRootType", {
  enumerable: true,
  get: function get() {
    return _getOperationRootType.getOperationRootType;
  }
});
Object.defineProperty(exports, "introspectionFromSchema", {
  enumerable: true,
  get: function get() {
    return _introspectionFromSchema.introspectionFromSchema;
  }
});
Object.defineProperty(exports, "buildClientSchema", {
  enumerable: true,
  get: function get() {
    return _buildClientSchema.buildClientSchema;
  }
});
Object.defineProperty(exports, "buildASTSchema", {
  enumerable: true,
  get: function get() {
    return _buildASTSchema.buildASTSchema;
  }
});
Object.defineProperty(exports, "buildSchema", {
  enumerable: true,
  get: function get() {
    return _buildASTSchema.buildSchema;
  }
});
Object.defineProperty(exports, "extendSchema", {
  enumerable: true,
  get: function get() {
    return _extendSchema.extendSchema;
  }
});
Object.defineProperty(exports, "getDescription", {
  enumerable: true,
  get: function get() {
    return _extendSchema.getDescription;
  }
});
Object.defineProperty(exports, "lexicographicSortSchema", {
  enumerable: true,
  get: function get() {
    return _lexicographicSortSchema.lexicographicSortSchema;
  }
});
Object.defineProperty(exports, "printSchema", {
  enumerable: true,
  get: function get() {
    return _printSchema.printSchema;
  }
});
Object.defineProperty(exports, "printType", {
  enumerable: true,
  get: function get() {
    return _printSchema.printType;
  }
});
Object.defineProperty(exports, "printIntrospectionSchema", {
  enumerable: true,
  get: function get() {
    return _printSchema.printIntrospectionSchema;
  }
});
Object.defineProperty(exports, "typeFromAST", {
  enumerable: true,
  get: function get() {
    return _typeFromAST.typeFromAST;
  }
});
Object.defineProperty(exports, "valueFromAST", {
  enumerable: true,
  get: function get() {
    return _valueFromAST.valueFromAST;
  }
});
Object.defineProperty(exports, "valueFromASTUntyped", {
  enumerable: true,
  get: function get() {
    return _valueFromASTUntyped.valueFromASTUntyped;
  }
});
Object.defineProperty(exports, "astFromValue", {
  enumerable: true,
  get: function get() {
    return _astFromValue.astFromValue;
  }
});
Object.defineProperty(exports, "TypeInfo", {
  enumerable: true,
  get: function get() {
    return _TypeInfo.TypeInfo;
  }
});
Object.defineProperty(exports, "visitWithTypeInfo", {
  enumerable: true,
  get: function get() {
    return _TypeInfo.visitWithTypeInfo;
  }
});
Object.defineProperty(exports, "coerceInputValue", {
  enumerable: true,
  get: function get() {
    return _coerceInputValue.coerceInputValue;
  }
});
Object.defineProperty(exports, "concatAST", {
  enumerable: true,
  get: function get() {
    return _concatAST.concatAST;
  }
});
Object.defineProperty(exports, "separateOperations", {
  enumerable: true,
  get: function get() {
    return _separateOperations.separateOperations;
  }
});
Object.defineProperty(exports, "stripIgnoredCharacters", {
  enumerable: true,
  get: function get() {
    return _stripIgnoredCharacters.stripIgnoredCharacters;
  }
});
Object.defineProperty(exports, "isEqualType", {
  enumerable: true,
  get: function get() {
    return _typeComparators.isEqualType;
  }
});
Object.defineProperty(exports, "isTypeSubTypeOf", {
  enumerable: true,
  get: function get() {
    return _typeComparators.isTypeSubTypeOf;
  }
});
Object.defineProperty(exports, "doTypesOverlap", {
  enumerable: true,
  get: function get() {
    return _typeComparators.doTypesOverlap;
  }
});
Object.defineProperty(exports, "assertValidName", {
  enumerable: true,
  get: function get() {
    return _assertValidName.assertValidName;
  }
});
Object.defineProperty(exports, "isValidNameError", {
  enumerable: true,
  get: function get() {
    return _assertValidName.isValidNameError;
  }
});
Object.defineProperty(exports, "BreakingChangeType", {
  enumerable: true,
  get: function get() {
    return _findBreakingChanges.BreakingChangeType;
  }
});
Object.defineProperty(exports, "DangerousChangeType", {
  enumerable: true,
  get: function get() {
    return _findBreakingChanges.DangerousChangeType;
  }
});
Object.defineProperty(exports, "findBreakingChanges", {
  enumerable: true,
  get: function get() {
    return _findBreakingChanges.findBreakingChanges;
  }
});
Object.defineProperty(exports, "findDangerousChanges", {
  enumerable: true,
  get: function get() {
    return _findBreakingChanges.findDangerousChanges;
  }
});
Object.defineProperty(exports, "findDeprecatedUsages", {
  enumerable: true,
  get: function get() {
    return _findDeprecatedUsages.findDeprecatedUsages;
  }
});

var _getIntrospectionQuery = require("./getIntrospectionQuery.js");

var _getOperationAST = require("./getOperationAST.js");

var _getOperationRootType = require("./getOperationRootType.js");

var _introspectionFromSchema = require("./introspectionFromSchema.js");

var _buildClientSchema = require("./buildClientSchema.js");

var _buildASTSchema = require("./buildASTSchema.js");

var _extendSchema = require("./extendSchema.js");

var _lexicographicSortSchema = require("./lexicographicSortSchema.js");

var _printSchema = require("./printSchema.js");

var _typeFromAST = require("./typeFromAST.js");

var _valueFromAST = require("./valueFromAST.js");

var _valueFromASTUntyped = require("./valueFromASTUntyped.js");

var _astFromValue = require("./astFromValue.js");

var _TypeInfo = require("./TypeInfo.js");

var _coerceInputValue = require("./coerceInputValue.js");

var _concatAST = require("./concatAST.js");

var _separateOperations = require("./separateOperations.js");

var _stripIgnoredCharacters = require("./stripIgnoredCharacters.js");

var _typeComparators = require("./typeComparators.js");

var _assertValidName = require("./assertValidName.js");

var _findBreakingChanges = require("./findBreakingChanges.js");

var _findDeprecatedUsages = require("./findDeprecatedUsages.js");

},{"./TypeInfo.js":82,"./assertValidName.js":83,"./astFromValue.js":84,"./buildASTSchema.js":85,"./buildClientSchema.js":86,"./coerceInputValue.js":87,"./concatAST.js":88,"./extendSchema.js":89,"./findBreakingChanges.js":90,"./findDeprecatedUsages.js":91,"./getIntrospectionQuery.js":92,"./getOperationAST.js":93,"./getOperationRootType.js":94,"./introspectionFromSchema.js":96,"./lexicographicSortSchema.js":97,"./printSchema.js":98,"./separateOperations.js":99,"./stripIgnoredCharacters.js":100,"./typeComparators.js":101,"./typeFromAST.js":102,"./valueFromAST.js":103,"./valueFromASTUntyped.js":104}],96:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.introspectionFromSchema = introspectionFromSchema;

var _invariant = _interopRequireDefault(require("../jsutils/invariant.js"));

var _parser = require("../language/parser.js");

var _execute = require("../execution/execute.js");

var _getIntrospectionQuery = require("./getIntrospectionQuery.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function ownKeys(object, enumerableOnly) { var keys = Object.keys(object); if (Object.getOwnPropertySymbols) { var symbols = Object.getOwnPropertySymbols(object); if (enumerableOnly) symbols = symbols.filter(function (sym) { return Object.getOwnPropertyDescriptor(object, sym).enumerable; }); keys.push.apply(keys, symbols); } return keys; }

function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; if (i % 2) { ownKeys(Object(source), true).forEach(function (key) { _defineProperty(target, key, source[key]); }); } else if (Object.getOwnPropertyDescriptors) { Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)); } else { ownKeys(Object(source)).forEach(function (key) { Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key)); }); } } return target; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

/**
 * Build an IntrospectionQuery from a GraphQLSchema
 *
 * IntrospectionQuery is useful for utilities that care about type and field
 * relationships, but do not need to traverse through those relationships.
 *
 * This is the inverse of buildClientSchema. The primary use case is outside
 * of the server context, for instance when doing schema comparisons.
 */
function introspectionFromSchema(schema, options) {
  var optionsWithDefaults = _objectSpread({
    specifiedByUrl: true,
    directiveIsRepeatable: true,
    schemaDescription: true,
    inputValueDeprecation: true
  }, options);

  var document = (0, _parser.parse)((0, _getIntrospectionQuery.getIntrospectionQuery)(optionsWithDefaults));
  var result = (0, _execute.executeSync)({
    schema: schema,
    document: document
  });
  !result.errors && result.data || (0, _invariant.default)(0);
  return result.data;
}

},{"../execution/execute.js":23,"../jsutils/invariant.js":35,"../language/parser.js":58,"./getIntrospectionQuery.js":92}],97:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.lexicographicSortSchema = lexicographicSortSchema;

var _objectValues = _interopRequireDefault(require("../polyfills/objectValues.js"));

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _invariant = _interopRequireDefault(require("../jsutils/invariant.js"));

var _keyValMap = _interopRequireDefault(require("../jsutils/keyValMap.js"));

var _naturalCompare = _interopRequireDefault(require("../jsutils/naturalCompare.js"));

var _schema = require("../type/schema.js");

var _directives = require("../type/directives.js");

var _introspection = require("../type/introspection.js");

var _definition = require("../type/definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function ownKeys(object, enumerableOnly) { var keys = Object.keys(object); if (Object.getOwnPropertySymbols) { var symbols = Object.getOwnPropertySymbols(object); if (enumerableOnly) symbols = symbols.filter(function (sym) { return Object.getOwnPropertyDescriptor(object, sym).enumerable; }); keys.push.apply(keys, symbols); } return keys; }

function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; if (i % 2) { ownKeys(Object(source), true).forEach(function (key) { _defineProperty(target, key, source[key]); }); } else if (Object.getOwnPropertyDescriptors) { Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)); } else { ownKeys(Object(source)).forEach(function (key) { Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key)); }); } } return target; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

/**
 * Sort GraphQLSchema.
 *
 * This function returns a sorted copy of the given GraphQLSchema.
 */
function lexicographicSortSchema(schema) {
  var schemaConfig = schema.toConfig();
  var typeMap = (0, _keyValMap.default)(sortByName(schemaConfig.types), function (type) {
    return type.name;
  }, sortNamedType);
  return new _schema.GraphQLSchema(_objectSpread(_objectSpread({}, schemaConfig), {}, {
    types: (0, _objectValues.default)(typeMap),
    directives: sortByName(schemaConfig.directives).map(sortDirective),
    query: replaceMaybeType(schemaConfig.query),
    mutation: replaceMaybeType(schemaConfig.mutation),
    subscription: replaceMaybeType(schemaConfig.subscription)
  }));

  function replaceType(type) {
    if ((0, _definition.isListType)(type)) {
      // $FlowFixMe[incompatible-return]
      return new _definition.GraphQLList(replaceType(type.ofType));
    } else if ((0, _definition.isNonNullType)(type)) {
      // $FlowFixMe[incompatible-return]
      return new _definition.GraphQLNonNull(replaceType(type.ofType));
    }

    return replaceNamedType(type);
  }

  function replaceNamedType(type) {
    return typeMap[type.name];
  }

  function replaceMaybeType(maybeType) {
    return maybeType && replaceNamedType(maybeType);
  }

  function sortDirective(directive) {
    var config = directive.toConfig();
    return new _directives.GraphQLDirective(_objectSpread(_objectSpread({}, config), {}, {
      locations: sortBy(config.locations, function (x) {
        return x;
      }),
      args: sortArgs(config.args)
    }));
  }

  function sortArgs(args) {
    return sortObjMap(args, function (arg) {
      return _objectSpread(_objectSpread({}, arg), {}, {
        type: replaceType(arg.type)
      });
    });
  }

  function sortFields(fieldsMap) {
    return sortObjMap(fieldsMap, function (field) {
      return _objectSpread(_objectSpread({}, field), {}, {
        type: replaceType(field.type),
        args: sortArgs(field.args)
      });
    });
  }

  function sortInputFields(fieldsMap) {
    return sortObjMap(fieldsMap, function (field) {
      return _objectSpread(_objectSpread({}, field), {}, {
        type: replaceType(field.type)
      });
    });
  }

  function sortTypes(arr) {
    return sortByName(arr).map(replaceNamedType);
  }

  function sortNamedType(type) {
    if ((0, _definition.isScalarType)(type) || (0, _introspection.isIntrospectionType)(type)) {
      return type;
    }

    if ((0, _definition.isObjectType)(type)) {
      var config = type.toConfig();
      return new _definition.GraphQLObjectType(_objectSpread(_objectSpread({}, config), {}, {
        interfaces: function interfaces() {
          return sortTypes(config.interfaces);
        },
        fields: function fields() {
          return sortFields(config.fields);
        }
      }));
    }

    if ((0, _definition.isInterfaceType)(type)) {
      var _config = type.toConfig();

      return new _definition.GraphQLInterfaceType(_objectSpread(_objectSpread({}, _config), {}, {
        interfaces: function interfaces() {
          return sortTypes(_config.interfaces);
        },
        fields: function fields() {
          return sortFields(_config.fields);
        }
      }));
    }

    if ((0, _definition.isUnionType)(type)) {
      var _config2 = type.toConfig();

      return new _definition.GraphQLUnionType(_objectSpread(_objectSpread({}, _config2), {}, {
        types: function types() {
          return sortTypes(_config2.types);
        }
      }));
    }

    if ((0, _definition.isEnumType)(type)) {
      var _config3 = type.toConfig();

      return new _definition.GraphQLEnumType(_objectSpread(_objectSpread({}, _config3), {}, {
        values: sortObjMap(_config3.values)
      }));
    } // istanbul ignore else (See: 'https://github.com/graphql/graphql-js/issues/2618')


    if ((0, _definition.isInputObjectType)(type)) {
      var _config4 = type.toConfig();

      return new _definition.GraphQLInputObjectType(_objectSpread(_objectSpread({}, _config4), {}, {
        fields: function fields() {
          return sortInputFields(_config4.fields);
        }
      }));
    } // istanbul ignore next (Not reachable. All possible types have been considered)


    false || (0, _invariant.default)(0, 'Unexpected type: ' + (0, _inspect.default)(type));
  }
}

function sortObjMap(map, sortValueFn) {
  var sortedMap = Object.create(null);
  var sortedKeys = sortBy(Object.keys(map), function (x) {
    return x;
  });

  for (var _i2 = 0; _i2 < sortedKeys.length; _i2++) {
    var key = sortedKeys[_i2];
    var value = map[key];
    sortedMap[key] = sortValueFn ? sortValueFn(value) : value;
  }

  return sortedMap;
}

function sortByName(array) {
  return sortBy(array, function (obj) {
    return obj.name;
  });
}

function sortBy(array, mapToKey) {
  return array.slice().sort(function (obj1, obj2) {
    var key1 = mapToKey(obj1);
    var key2 = mapToKey(obj2);
    return (0, _naturalCompare.default)(key1, key2);
  });
}

},{"../jsutils/inspect.js":33,"../jsutils/invariant.js":35,"../jsutils/keyValMap.js":40,"../jsutils/naturalCompare.js":43,"../polyfills/objectValues.js":70,"../type/definition.js":75,"../type/directives.js":76,"../type/introspection.js":78,"../type/schema.js":80}],98:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.printSchema = printSchema;
exports.printIntrospectionSchema = printIntrospectionSchema;
exports.printType = printType;

var _objectValues = _interopRequireDefault(require("../polyfills/objectValues.js"));

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _invariant = _interopRequireDefault(require("../jsutils/invariant.js"));

var _printer = require("../language/printer.js");

var _blockString = require("../language/blockString.js");

var _introspection = require("../type/introspection.js");

var _scalars = require("../type/scalars.js");

var _directives = require("../type/directives.js");

var _definition = require("../type/definition.js");

var _astFromValue = require("./astFromValue.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Accepts options as a second argument:
 *
 *    - commentDescriptions:
 *        Provide true to use preceding comments as the description.
 *
 */
function printSchema(schema, options) {
  return printFilteredSchema(schema, function (n) {
    return !(0, _directives.isSpecifiedDirective)(n);
  }, isDefinedType, options);
}

function printIntrospectionSchema(schema, options) {
  return printFilteredSchema(schema, _directives.isSpecifiedDirective, _introspection.isIntrospectionType, options);
}

function isDefinedType(type) {
  return !(0, _scalars.isSpecifiedScalarType)(type) && !(0, _introspection.isIntrospectionType)(type);
}

function printFilteredSchema(schema, directiveFilter, typeFilter, options) {
  var directives = schema.getDirectives().filter(directiveFilter);
  var types = (0, _objectValues.default)(schema.getTypeMap()).filter(typeFilter);
  return [printSchemaDefinition(schema)].concat(directives.map(function (directive) {
    return printDirective(directive, options);
  }), types.map(function (type) {
    return printType(type, options);
  })).filter(Boolean).join('\n\n') + '\n';
}

function printSchemaDefinition(schema) {
  if (schema.description == null && isSchemaOfCommonNames(schema)) {
    return;
  }

  var operationTypes = [];
  var queryType = schema.getQueryType();

  if (queryType) {
    operationTypes.push("  query: ".concat(queryType.name));
  }

  var mutationType = schema.getMutationType();

  if (mutationType) {
    operationTypes.push("  mutation: ".concat(mutationType.name));
  }

  var subscriptionType = schema.getSubscriptionType();

  if (subscriptionType) {
    operationTypes.push("  subscription: ".concat(subscriptionType.name));
  }

  return printDescription({}, schema) + "schema {\n".concat(operationTypes.join('\n'), "\n}");
}
/**
 * GraphQL schema define root types for each type of operation. These types are
 * the same as any other type and can be named in any manner, however there is
 * a common naming convention:
 *
 *   schema {
 *     query: Query
 *     mutation: Mutation
 *   }
 *
 * When using this naming convention, the schema description can be omitted.
 */


function isSchemaOfCommonNames(schema) {
  var queryType = schema.getQueryType();

  if (queryType && queryType.name !== 'Query') {
    return false;
  }

  var mutationType = schema.getMutationType();

  if (mutationType && mutationType.name !== 'Mutation') {
    return false;
  }

  var subscriptionType = schema.getSubscriptionType();

  if (subscriptionType && subscriptionType.name !== 'Subscription') {
    return false;
  }

  return true;
}

function printType(type, options) {
  if ((0, _definition.isScalarType)(type)) {
    return printScalar(type, options);
  }

  if ((0, _definition.isObjectType)(type)) {
    return printObject(type, options);
  }

  if ((0, _definition.isInterfaceType)(type)) {
    return printInterface(type, options);
  }

  if ((0, _definition.isUnionType)(type)) {
    return printUnion(type, options);
  }

  if ((0, _definition.isEnumType)(type)) {
    return printEnum(type, options);
  } // istanbul ignore else (See: 'https://github.com/graphql/graphql-js/issues/2618')


  if ((0, _definition.isInputObjectType)(type)) {
    return printInputObject(type, options);
  } // istanbul ignore next (Not reachable. All possible types have been considered)


  false || (0, _invariant.default)(0, 'Unexpected type: ' + (0, _inspect.default)(type));
}

function printScalar(type, options) {
  return printDescription(options, type) + "scalar ".concat(type.name) + printSpecifiedByUrl(type);
}

function printImplementedInterfaces(type) {
  var interfaces = type.getInterfaces();
  return interfaces.length ? ' implements ' + interfaces.map(function (i) {
    return i.name;
  }).join(' & ') : '';
}

function printObject(type, options) {
  return printDescription(options, type) + "type ".concat(type.name) + printImplementedInterfaces(type) + printFields(options, type);
}

function printInterface(type, options) {
  return printDescription(options, type) + "interface ".concat(type.name) + printImplementedInterfaces(type) + printFields(options, type);
}

function printUnion(type, options) {
  var types = type.getTypes();
  var possibleTypes = types.length ? ' = ' + types.join(' | ') : '';
  return printDescription(options, type) + 'union ' + type.name + possibleTypes;
}

function printEnum(type, options) {
  var values = type.getValues().map(function (value, i) {
    return printDescription(options, value, '  ', !i) + '  ' + value.name + printDeprecated(value.deprecationReason);
  });
  return printDescription(options, type) + "enum ".concat(type.name) + printBlock(values);
}

function printInputObject(type, options) {
  var fields = (0, _objectValues.default)(type.getFields()).map(function (f, i) {
    return printDescription(options, f, '  ', !i) + '  ' + printInputValue(f);
  });
  return printDescription(options, type) + "input ".concat(type.name) + printBlock(fields);
}

function printFields(options, type) {
  var fields = (0, _objectValues.default)(type.getFields()).map(function (f, i) {
    return printDescription(options, f, '  ', !i) + '  ' + f.name + printArgs(options, f.args, '  ') + ': ' + String(f.type) + printDeprecated(f.deprecationReason);
  });
  return printBlock(fields);
}

function printBlock(items) {
  return items.length !== 0 ? ' {\n' + items.join('\n') + '\n}' : '';
}

function printArgs(options, args) {
  var indentation = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : '';

  if (args.length === 0) {
    return '';
  } // If every arg does not have a description, print them on one line.


  if (args.every(function (arg) {
    return !arg.description;
  })) {
    return '(' + args.map(printInputValue).join(', ') + ')';
  }

  return '(\n' + args.map(function (arg, i) {
    return printDescription(options, arg, '  ' + indentation, !i) + '  ' + indentation + printInputValue(arg);
  }).join('\n') + '\n' + indentation + ')';
}

function printInputValue(arg) {
  var defaultAST = (0, _astFromValue.astFromValue)(arg.defaultValue, arg.type);
  var argDecl = arg.name + ': ' + String(arg.type);

  if (defaultAST) {
    argDecl += " = ".concat((0, _printer.print)(defaultAST));
  }

  return argDecl + printDeprecated(arg.deprecationReason);
}

function printDirective(directive, options) {
  return printDescription(options, directive) + 'directive @' + directive.name + printArgs(options, directive.args) + (directive.isRepeatable ? ' repeatable' : '') + ' on ' + directive.locations.join(' | ');
}

function printDeprecated(reason) {
  if (reason == null) {
    return '';
  }

  var reasonAST = (0, _astFromValue.astFromValue)(reason, _scalars.GraphQLString);

  if (reasonAST && reason !== _directives.DEFAULT_DEPRECATION_REASON) {
    return ' @deprecated(reason: ' + (0, _printer.print)(reasonAST) + ')';
  }

  return ' @deprecated';
}

function printSpecifiedByUrl(scalar) {
  if (scalar.specifiedByUrl == null) {
    return '';
  }

  var url = scalar.specifiedByUrl;
  var urlAST = (0, _astFromValue.astFromValue)(url, _scalars.GraphQLString);
  urlAST || (0, _invariant.default)(0, 'Unexpected null value returned from `astFromValue` for specifiedByUrl');
  return ' @specifiedBy(url: ' + (0, _printer.print)(urlAST) + ')';
}

function printDescription(options, def) {
  var indentation = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : '';
  var firstInBlock = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : true;
  var description = def.description;

  if (description == null) {
    return '';
  }

  if ((options === null || options === void 0 ? void 0 : options.commentDescriptions) === true) {
    return printDescriptionWithComments(description, indentation, firstInBlock);
  }

  var preferMultipleLines = description.length > 70;
  var blockString = (0, _blockString.printBlockString)(description, '', preferMultipleLines);
  var prefix = indentation && !firstInBlock ? '\n' + indentation : indentation;
  return prefix + blockString.replace(/\n/g, '\n' + indentation) + '\n';
}

function printDescriptionWithComments(description, indentation, firstInBlock) {
  var prefix = indentation && !firstInBlock ? '\n' : '';
  var comment = description.split('\n').map(function (line) {
    return indentation + (line !== '' ? '# ' + line : '#');
  }).join('\n');
  return prefix + comment + '\n';
}

},{"../jsutils/inspect.js":33,"../jsutils/invariant.js":35,"../language/blockString.js":52,"../language/printer.js":61,"../polyfills/objectValues.js":70,"../type/definition.js":75,"../type/directives.js":76,"../type/introspection.js":78,"../type/scalars.js":79,"./astFromValue.js":84}],99:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.separateOperations = separateOperations;

var _kinds = require("../language/kinds.js");

var _visitor = require("../language/visitor.js");

/**
 * separateOperations accepts a single AST document which may contain many
 * operations and fragments and returns a collection of AST documents each of
 * which contains a single operation as well the fragment definitions it
 * refers to.
 */
function separateOperations(documentAST) {
  var operations = [];
  var depGraph = Object.create(null); // Populate metadata and build a dependency graph.

  for (var _i2 = 0, _documentAST$definiti2 = documentAST.definitions; _i2 < _documentAST$definiti2.length; _i2++) {
    var definitionNode = _documentAST$definiti2[_i2];

    switch (definitionNode.kind) {
      case _kinds.Kind.OPERATION_DEFINITION:
        operations.push(definitionNode);
        break;

      case _kinds.Kind.FRAGMENT_DEFINITION:
        depGraph[definitionNode.name.value] = collectDependencies(definitionNode.selectionSet);
        break;
    }
  } // For each operation, produce a new synthesized AST which includes only what
  // is necessary for completing that operation.


  var separatedDocumentASTs = Object.create(null);

  var _loop = function _loop(_i4) {
    var operation = operations[_i4];
    var dependencies = new Set();

    for (var _i6 = 0, _collectDependencies2 = collectDependencies(operation.selectionSet); _i6 < _collectDependencies2.length; _i6++) {
      var fragmentName = _collectDependencies2[_i6];
      collectTransitiveDependencies(dependencies, depGraph, fragmentName);
    } // Provides the empty string for anonymous operations.


    var operationName = operation.name ? operation.name.value : ''; // The list of definition nodes to be included for this operation, sorted
    // to retain the same order as the original document.

    separatedDocumentASTs[operationName] = {
      kind: _kinds.Kind.DOCUMENT,
      definitions: documentAST.definitions.filter(function (node) {
        return node === operation || node.kind === _kinds.Kind.FRAGMENT_DEFINITION && dependencies.has(node.name.value);
      })
    };
  };

  for (var _i4 = 0; _i4 < operations.length; _i4++) {
    _loop(_i4);
  }

  return separatedDocumentASTs;
}

// From a dependency graph, collects a list of transitive dependencies by
// recursing through a dependency graph.
function collectTransitiveDependencies(collected, depGraph, fromName) {
  if (!collected.has(fromName)) {
    collected.add(fromName);
    var immediateDeps = depGraph[fromName];

    if (immediateDeps !== undefined) {
      for (var _i8 = 0; _i8 < immediateDeps.length; _i8++) {
        var toName = immediateDeps[_i8];
        collectTransitiveDependencies(collected, depGraph, toName);
      }
    }
  }
}

function collectDependencies(selectionSet) {
  var dependencies = [];
  (0, _visitor.visit)(selectionSet, {
    FragmentSpread: function FragmentSpread(node) {
      dependencies.push(node.name.value);
    }
  });
  return dependencies;
}

},{"../language/kinds.js":55,"../language/visitor.js":64}],100:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.stripIgnoredCharacters = stripIgnoredCharacters;

var _source = require("../language/source.js");

var _tokenKind = require("../language/tokenKind.js");

var _lexer = require("../language/lexer.js");

var _blockString = require("../language/blockString.js");

/**
 * Strips characters that are not significant to the validity or execution
 * of a GraphQL document:
 *   - UnicodeBOM
 *   - WhiteSpace
 *   - LineTerminator
 *   - Comment
 *   - Comma
 *   - BlockString indentation
 *
 * Note: It is required to have a delimiter character between neighboring
 * non-punctuator tokens and this function always uses single space as delimiter.
 *
 * It is guaranteed that both input and output documents if parsed would result
 * in the exact same AST except for nodes location.
 *
 * Warning: It is guaranteed that this function will always produce stable results.
 * However, it's not guaranteed that it will stay the same between different
 * releases due to bugfixes or changes in the GraphQL specification.
 *
 * Query example:
 *
 * query SomeQuery($foo: String!, $bar: String) {
 *   someField(foo: $foo, bar: $bar) {
 *     a
 *     b {
 *       c
 *       d
 *     }
 *   }
 * }
 *
 * Becomes:
 *
 * query SomeQuery($foo:String!$bar:String){someField(foo:$foo bar:$bar){a b{c d}}}
 *
 * SDL example:
 *
 * """
 * Type description
 * """
 * type Foo {
 *   """
 *   Field description
 *   """
 *   bar: String
 * }
 *
 * Becomes:
 *
 * """Type description""" type Foo{"""Field description""" bar:String}
 */
function stripIgnoredCharacters(source) {
  var sourceObj = (0, _source.isSource)(source) ? source : new _source.Source(source);
  var body = sourceObj.body;
  var lexer = new _lexer.Lexer(sourceObj);
  var strippedBody = '';
  var wasLastAddedTokenNonPunctuator = false;

  while (lexer.advance().kind !== _tokenKind.TokenKind.EOF) {
    var currentToken = lexer.token;
    var tokenKind = currentToken.kind;
    /**
     * Every two non-punctuator tokens should have space between them.
     * Also prevent case of non-punctuator token following by spread resulting
     * in invalid token (e.g. `1...` is invalid Float token).
     */

    var isNonPunctuator = !(0, _lexer.isPunctuatorTokenKind)(currentToken.kind);

    if (wasLastAddedTokenNonPunctuator) {
      if (isNonPunctuator || currentToken.kind === _tokenKind.TokenKind.SPREAD) {
        strippedBody += ' ';
      }
    }

    var tokenBody = body.slice(currentToken.start, currentToken.end);

    if (tokenKind === _tokenKind.TokenKind.BLOCK_STRING) {
      strippedBody += dedentBlockString(tokenBody);
    } else {
      strippedBody += tokenBody;
    }

    wasLastAddedTokenNonPunctuator = isNonPunctuator;
  }

  return strippedBody;
}

function dedentBlockString(blockStr) {
  // skip leading and trailing triple quotations
  var rawStr = blockStr.slice(3, -3);
  var body = (0, _blockString.dedentBlockStringValue)(rawStr);

  if ((0, _blockString.getBlockStringIndentation)(body) > 0) {
    body = '\n' + body;
  }

  var lastChar = body[body.length - 1];
  var hasTrailingQuote = lastChar === '"' && body.slice(-4) !== '\\"""';

  if (hasTrailingQuote || lastChar === '\\') {
    body += '\n';
  }

  return '"""' + body + '"""';
}

},{"../language/blockString.js":52,"../language/lexer.js":56,"../language/source.js":62,"../language/tokenKind.js":63}],101:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isEqualType = isEqualType;
exports.isTypeSubTypeOf = isTypeSubTypeOf;
exports.doTypesOverlap = doTypesOverlap;

var _definition = require("../type/definition.js");

/**
 * Provided two types, return true if the types are equal (invariant).
 */
function isEqualType(typeA, typeB) {
  // Equivalent types are equal.
  if (typeA === typeB) {
    return true;
  } // If either type is non-null, the other must also be non-null.


  if ((0, _definition.isNonNullType)(typeA) && (0, _definition.isNonNullType)(typeB)) {
    return isEqualType(typeA.ofType, typeB.ofType);
  } // If either type is a list, the other must also be a list.


  if ((0, _definition.isListType)(typeA) && (0, _definition.isListType)(typeB)) {
    return isEqualType(typeA.ofType, typeB.ofType);
  } // Otherwise the types are not equal.


  return false;
}
/**
 * Provided a type and a super type, return true if the first type is either
 * equal or a subset of the second super type (covariant).
 */


function isTypeSubTypeOf(schema, maybeSubType, superType) {
  // Equivalent type is a valid subtype
  if (maybeSubType === superType) {
    return true;
  } // If superType is non-null, maybeSubType must also be non-null.


  if ((0, _definition.isNonNullType)(superType)) {
    if ((0, _definition.isNonNullType)(maybeSubType)) {
      return isTypeSubTypeOf(schema, maybeSubType.ofType, superType.ofType);
    }

    return false;
  }

  if ((0, _definition.isNonNullType)(maybeSubType)) {
    // If superType is nullable, maybeSubType may be non-null or nullable.
    return isTypeSubTypeOf(schema, maybeSubType.ofType, superType);
  } // If superType type is a list, maybeSubType type must also be a list.


  if ((0, _definition.isListType)(superType)) {
    if ((0, _definition.isListType)(maybeSubType)) {
      return isTypeSubTypeOf(schema, maybeSubType.ofType, superType.ofType);
    }

    return false;
  }

  if ((0, _definition.isListType)(maybeSubType)) {
    // If superType is not a list, maybeSubType must also be not a list.
    return false;
  } // If superType type is an abstract type, check if it is super type of maybeSubType.
  // Otherwise, the child type is not a valid subtype of the parent type.


  return (0, _definition.isAbstractType)(superType) && ((0, _definition.isInterfaceType)(maybeSubType) || (0, _definition.isObjectType)(maybeSubType)) && schema.isSubType(superType, maybeSubType);
}
/**
 * Provided two composite types, determine if they "overlap". Two composite
 * types overlap when the Sets of possible concrete types for each intersect.
 *
 * This is often used to determine if a fragment of a given type could possibly
 * be visited in a context of another type.
 *
 * This function is commutative.
 */


function doTypesOverlap(schema, typeA, typeB) {
  // Equivalent types overlap
  if (typeA === typeB) {
    return true;
  }

  if ((0, _definition.isAbstractType)(typeA)) {
    if ((0, _definition.isAbstractType)(typeB)) {
      // If both types are abstract, then determine if there is any intersection
      // between possible concrete types of each.
      return schema.getPossibleTypes(typeA).some(function (type) {
        return schema.isSubType(typeB, type);
      });
    } // Determine if the latter type is a possible concrete type of the former.


    return schema.isSubType(typeA, typeB);
  }

  if ((0, _definition.isAbstractType)(typeB)) {
    // Determine if the former type is a possible concrete type of the latter.
    return schema.isSubType(typeB, typeA);
  } // Otherwise the types do not overlap.


  return false;
}

},{"../type/definition.js":75}],102:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.typeFromAST = typeFromAST;

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _invariant = _interopRequireDefault(require("../jsutils/invariant.js"));

var _kinds = require("../language/kinds.js");

var _definition = require("../type/definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function typeFromAST(schema, typeNode) {
  /* eslint-enable no-redeclare */
  var innerType;

  if (typeNode.kind === _kinds.Kind.LIST_TYPE) {
    innerType = typeFromAST(schema, typeNode.type);
    return innerType && new _definition.GraphQLList(innerType);
  }

  if (typeNode.kind === _kinds.Kind.NON_NULL_TYPE) {
    innerType = typeFromAST(schema, typeNode.type);
    return innerType && new _definition.GraphQLNonNull(innerType);
  } // istanbul ignore else (See: 'https://github.com/graphql/graphql-js/issues/2618')


  if (typeNode.kind === _kinds.Kind.NAMED_TYPE) {
    return schema.getType(typeNode.name.value);
  } // istanbul ignore next (Not reachable. All possible type nodes have been considered)


  false || (0, _invariant.default)(0, 'Unexpected type node: ' + (0, _inspect.default)(typeNode));
}

},{"../jsutils/inspect.js":33,"../jsutils/invariant.js":35,"../language/kinds.js":55,"../type/definition.js":75}],103:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.valueFromAST = valueFromAST;

var _objectValues3 = _interopRequireDefault(require("../polyfills/objectValues.js"));

var _keyMap = _interopRequireDefault(require("../jsutils/keyMap.js"));

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _invariant = _interopRequireDefault(require("../jsutils/invariant.js"));

var _kinds = require("../language/kinds.js");

var _definition = require("../type/definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Produces a JavaScript value given a GraphQL Value AST.
 *
 * A GraphQL type must be provided, which will be used to interpret different
 * GraphQL Value literals.
 *
 * Returns `undefined` when the value could not be validly coerced according to
 * the provided type.
 *
 * | GraphQL Value        | JSON Value    |
 * | -------------------- | ------------- |
 * | Input Object         | Object        |
 * | List                 | Array         |
 * | Boolean              | Boolean       |
 * | String               | String        |
 * | Int / Float          | Number        |
 * | Enum Value           | Mixed         |
 * | NullValue            | null          |
 *
 */
function valueFromAST(valueNode, type, variables) {
  if (!valueNode) {
    // When there is no node, then there is also no value.
    // Importantly, this is different from returning the value null.
    return;
  }

  if (valueNode.kind === _kinds.Kind.VARIABLE) {
    var variableName = valueNode.name.value;

    if (variables == null || variables[variableName] === undefined) {
      // No valid return value.
      return;
    }

    var variableValue = variables[variableName];

    if (variableValue === null && (0, _definition.isNonNullType)(type)) {
      return; // Invalid: intentionally return no value.
    } // Note: This does no further checking that this variable is correct.
    // This assumes that this query has been validated and the variable
    // usage here is of the correct type.


    return variableValue;
  }

  if ((0, _definition.isNonNullType)(type)) {
    if (valueNode.kind === _kinds.Kind.NULL) {
      return; // Invalid: intentionally return no value.
    }

    return valueFromAST(valueNode, type.ofType, variables);
  }

  if (valueNode.kind === _kinds.Kind.NULL) {
    // This is explicitly returning the value null.
    return null;
  }

  if ((0, _definition.isListType)(type)) {
    var itemType = type.ofType;

    if (valueNode.kind === _kinds.Kind.LIST) {
      var coercedValues = [];

      for (var _i2 = 0, _valueNode$values2 = valueNode.values; _i2 < _valueNode$values2.length; _i2++) {
        var itemNode = _valueNode$values2[_i2];

        if (isMissingVariable(itemNode, variables)) {
          // If an array contains a missing variable, it is either coerced to
          // null or if the item type is non-null, it considered invalid.
          if ((0, _definition.isNonNullType)(itemType)) {
            return; // Invalid: intentionally return no value.
          }

          coercedValues.push(null);
        } else {
          var itemValue = valueFromAST(itemNode, itemType, variables);

          if (itemValue === undefined) {
            return; // Invalid: intentionally return no value.
          }

          coercedValues.push(itemValue);
        }
      }

      return coercedValues;
    }

    var coercedValue = valueFromAST(valueNode, itemType, variables);

    if (coercedValue === undefined) {
      return; // Invalid: intentionally return no value.
    }

    return [coercedValue];
  }

  if ((0, _definition.isInputObjectType)(type)) {
    if (valueNode.kind !== _kinds.Kind.OBJECT) {
      return; // Invalid: intentionally return no value.
    }

    var coercedObj = Object.create(null);
    var fieldNodes = (0, _keyMap.default)(valueNode.fields, function (field) {
      return field.name.value;
    });

    for (var _i4 = 0, _objectValues2 = (0, _objectValues3.default)(type.getFields()); _i4 < _objectValues2.length; _i4++) {
      var field = _objectValues2[_i4];
      var fieldNode = fieldNodes[field.name];

      if (!fieldNode || isMissingVariable(fieldNode.value, variables)) {
        if (field.defaultValue !== undefined) {
          coercedObj[field.name] = field.defaultValue;
        } else if ((0, _definition.isNonNullType)(field.type)) {
          return; // Invalid: intentionally return no value.
        }

        continue;
      }

      var fieldValue = valueFromAST(fieldNode.value, field.type, variables);

      if (fieldValue === undefined) {
        return; // Invalid: intentionally return no value.
      }

      coercedObj[field.name] = fieldValue;
    }

    return coercedObj;
  } // istanbul ignore else (See: 'https://github.com/graphql/graphql-js/issues/2618')


  if ((0, _definition.isLeafType)(type)) {
    // Scalars and Enums fulfill parsing a literal value via parseLiteral().
    // Invalid values represent a failure to parse correctly, in which case
    // no value is returned.
    var result;

    try {
      result = type.parseLiteral(valueNode, variables);
    } catch (_error) {
      return; // Invalid: intentionally return no value.
    }

    if (result === undefined) {
      return; // Invalid: intentionally return no value.
    }

    return result;
  } // istanbul ignore next (Not reachable. All possible input types have been considered)


  false || (0, _invariant.default)(0, 'Unexpected input type: ' + (0, _inspect.default)(type));
} // Returns true if the provided valueNode is a variable which is not defined
// in the set of variables.


function isMissingVariable(valueNode, variables) {
  return valueNode.kind === _kinds.Kind.VARIABLE && (variables == null || variables[valueNode.name.value] === undefined);
}

},{"../jsutils/inspect.js":33,"../jsutils/invariant.js":35,"../jsutils/keyMap.js":39,"../language/kinds.js":55,"../polyfills/objectValues.js":70,"../type/definition.js":75}],104:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.valueFromASTUntyped = valueFromASTUntyped;

var _inspect = _interopRequireDefault(require("../jsutils/inspect.js"));

var _invariant = _interopRequireDefault(require("../jsutils/invariant.js"));

var _keyValMap = _interopRequireDefault(require("../jsutils/keyValMap.js"));

var _kinds = require("../language/kinds.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Produces a JavaScript value given a GraphQL Value AST.
 *
 * Unlike `valueFromAST()`, no type is provided. The resulting JavaScript value
 * will reflect the provided GraphQL value AST.
 *
 * | GraphQL Value        | JavaScript Value |
 * | -------------------- | ---------------- |
 * | Input Object         | Object           |
 * | List                 | Array            |
 * | Boolean              | Boolean          |
 * | String / Enum        | String           |
 * | Int / Float          | Number           |
 * | Null                 | null             |
 *
 */
function valueFromASTUntyped(valueNode, variables) {
  switch (valueNode.kind) {
    case _kinds.Kind.NULL:
      return null;

    case _kinds.Kind.INT:
      return parseInt(valueNode.value, 10);

    case _kinds.Kind.FLOAT:
      return parseFloat(valueNode.value);

    case _kinds.Kind.STRING:
    case _kinds.Kind.ENUM:
    case _kinds.Kind.BOOLEAN:
      return valueNode.value;

    case _kinds.Kind.LIST:
      return valueNode.values.map(function (node) {
        return valueFromASTUntyped(node, variables);
      });

    case _kinds.Kind.OBJECT:
      return (0, _keyValMap.default)(valueNode.fields, function (field) {
        return field.name.value;
      }, function (field) {
        return valueFromASTUntyped(field.value, variables);
      });

    case _kinds.Kind.VARIABLE:
      return variables === null || variables === void 0 ? void 0 : variables[valueNode.name.value];
  } // istanbul ignore next (Not reachable. All possible value nodes have been considered)


  false || (0, _invariant.default)(0, 'Unexpected value node: ' + (0, _inspect.default)(valueNode));
}

},{"../jsutils/inspect.js":33,"../jsutils/invariant.js":35,"../jsutils/keyValMap.js":40,"../language/kinds.js":55}],105:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ValidationContext = exports.SDLValidationContext = exports.ASTValidationContext = void 0;

var _kinds = require("../language/kinds.js");

var _visitor = require("../language/visitor.js");

var _TypeInfo = require("../utilities/TypeInfo.js");

function _inheritsLoose(subClass, superClass) { subClass.prototype = Object.create(superClass.prototype); subClass.prototype.constructor = subClass; subClass.__proto__ = superClass; }

/**
 * An instance of this class is passed as the "this" context to all validators,
 * allowing access to commonly useful contextual information from within a
 * validation rule.
 */
var ASTValidationContext = /*#__PURE__*/function () {
  function ASTValidationContext(ast, onError) {
    this._ast = ast;
    this._fragments = undefined;
    this._fragmentSpreads = new Map();
    this._recursivelyReferencedFragments = new Map();
    this._onError = onError;
  }

  var _proto = ASTValidationContext.prototype;

  _proto.reportError = function reportError(error) {
    this._onError(error);
  };

  _proto.getDocument = function getDocument() {
    return this._ast;
  };

  _proto.getFragment = function getFragment(name) {
    var fragments = this._fragments;

    if (!fragments) {
      this._fragments = fragments = this.getDocument().definitions.reduce(function (frags, statement) {
        if (statement.kind === _kinds.Kind.FRAGMENT_DEFINITION) {
          frags[statement.name.value] = statement;
        }

        return frags;
      }, Object.create(null));
    }

    return fragments[name];
  };

  _proto.getFragmentSpreads = function getFragmentSpreads(node) {
    var spreads = this._fragmentSpreads.get(node);

    if (!spreads) {
      spreads = [];
      var setsToVisit = [node];

      while (setsToVisit.length !== 0) {
        var set = setsToVisit.pop();

        for (var _i2 = 0, _set$selections2 = set.selections; _i2 < _set$selections2.length; _i2++) {
          var selection = _set$selections2[_i2];

          if (selection.kind === _kinds.Kind.FRAGMENT_SPREAD) {
            spreads.push(selection);
          } else if (selection.selectionSet) {
            setsToVisit.push(selection.selectionSet);
          }
        }
      }

      this._fragmentSpreads.set(node, spreads);
    }

    return spreads;
  };

  _proto.getRecursivelyReferencedFragments = function getRecursivelyReferencedFragments(operation) {
    var fragments = this._recursivelyReferencedFragments.get(operation);

    if (!fragments) {
      fragments = [];
      var collectedNames = Object.create(null);
      var nodesToVisit = [operation.selectionSet];

      while (nodesToVisit.length !== 0) {
        var node = nodesToVisit.pop();

        for (var _i4 = 0, _this$getFragmentSpre2 = this.getFragmentSpreads(node); _i4 < _this$getFragmentSpre2.length; _i4++) {
          var spread = _this$getFragmentSpre2[_i4];
          var fragName = spread.name.value;

          if (collectedNames[fragName] !== true) {
            collectedNames[fragName] = true;
            var fragment = this.getFragment(fragName);

            if (fragment) {
              fragments.push(fragment);
              nodesToVisit.push(fragment.selectionSet);
            }
          }
        }
      }

      this._recursivelyReferencedFragments.set(operation, fragments);
    }

    return fragments;
  };

  return ASTValidationContext;
}();

exports.ASTValidationContext = ASTValidationContext;

var SDLValidationContext = /*#__PURE__*/function (_ASTValidationContext) {
  _inheritsLoose(SDLValidationContext, _ASTValidationContext);

  function SDLValidationContext(ast, schema, onError) {
    var _this;

    _this = _ASTValidationContext.call(this, ast, onError) || this;
    _this._schema = schema;
    return _this;
  }

  var _proto2 = SDLValidationContext.prototype;

  _proto2.getSchema = function getSchema() {
    return this._schema;
  };

  return SDLValidationContext;
}(ASTValidationContext);

exports.SDLValidationContext = SDLValidationContext;

var ValidationContext = /*#__PURE__*/function (_ASTValidationContext2) {
  _inheritsLoose(ValidationContext, _ASTValidationContext2);

  function ValidationContext(schema, ast, typeInfo, onError) {
    var _this2;

    _this2 = _ASTValidationContext2.call(this, ast, onError) || this;
    _this2._schema = schema;
    _this2._typeInfo = typeInfo;
    _this2._variableUsages = new Map();
    _this2._recursiveVariableUsages = new Map();
    return _this2;
  }

  var _proto3 = ValidationContext.prototype;

  _proto3.getSchema = function getSchema() {
    return this._schema;
  };

  _proto3.getVariableUsages = function getVariableUsages(node) {
    var usages = this._variableUsages.get(node);

    if (!usages) {
      var newUsages = [];
      var typeInfo = new _TypeInfo.TypeInfo(this._schema);
      (0, _visitor.visit)(node, (0, _TypeInfo.visitWithTypeInfo)(typeInfo, {
        VariableDefinition: function VariableDefinition() {
          return false;
        },
        Variable: function Variable(variable) {
          newUsages.push({
            node: variable,
            type: typeInfo.getInputType(),
            defaultValue: typeInfo.getDefaultValue()
          });
        }
      }));
      usages = newUsages;

      this._variableUsages.set(node, usages);
    }

    return usages;
  };

  _proto3.getRecursiveVariableUsages = function getRecursiveVariableUsages(operation) {
    var usages = this._recursiveVariableUsages.get(operation);

    if (!usages) {
      usages = this.getVariableUsages(operation);

      for (var _i6 = 0, _this$getRecursivelyR2 = this.getRecursivelyReferencedFragments(operation); _i6 < _this$getRecursivelyR2.length; _i6++) {
        var frag = _this$getRecursivelyR2[_i6];
        usages = usages.concat(this.getVariableUsages(frag));
      }

      this._recursiveVariableUsages.set(operation, usages);
    }

    return usages;
  };

  _proto3.getType = function getType() {
    return this._typeInfo.getType();
  };

  _proto3.getParentType = function getParentType() {
    return this._typeInfo.getParentType();
  };

  _proto3.getInputType = function getInputType() {
    return this._typeInfo.getInputType();
  };

  _proto3.getParentInputType = function getParentInputType() {
    return this._typeInfo.getParentInputType();
  };

  _proto3.getFieldDef = function getFieldDef() {
    return this._typeInfo.getFieldDef();
  };

  _proto3.getDirective = function getDirective() {
    return this._typeInfo.getDirective();
  };

  _proto3.getArgument = function getArgument() {
    return this._typeInfo.getArgument();
  };

  _proto3.getEnumValue = function getEnumValue() {
    return this._typeInfo.getEnumValue();
  };

  return ValidationContext;
}(ASTValidationContext);

exports.ValidationContext = ValidationContext;

},{"../language/kinds.js":55,"../language/visitor.js":64,"../utilities/TypeInfo.js":82}],106:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "validate", {
  enumerable: true,
  get: function get() {
    return _validate.validate;
  }
});
Object.defineProperty(exports, "ValidationContext", {
  enumerable: true,
  get: function get() {
    return _ValidationContext.ValidationContext;
  }
});
Object.defineProperty(exports, "specifiedRules", {
  enumerable: true,
  get: function get() {
    return _specifiedRules.specifiedRules;
  }
});
Object.defineProperty(exports, "ExecutableDefinitionsRule", {
  enumerable: true,
  get: function get() {
    return _ExecutableDefinitionsRule.ExecutableDefinitionsRule;
  }
});
Object.defineProperty(exports, "FieldsOnCorrectTypeRule", {
  enumerable: true,
  get: function get() {
    return _FieldsOnCorrectTypeRule.FieldsOnCorrectTypeRule;
  }
});
Object.defineProperty(exports, "FragmentsOnCompositeTypesRule", {
  enumerable: true,
  get: function get() {
    return _FragmentsOnCompositeTypesRule.FragmentsOnCompositeTypesRule;
  }
});
Object.defineProperty(exports, "KnownArgumentNamesRule", {
  enumerable: true,
  get: function get() {
    return _KnownArgumentNamesRule.KnownArgumentNamesRule;
  }
});
Object.defineProperty(exports, "KnownDirectivesRule", {
  enumerable: true,
  get: function get() {
    return _KnownDirectivesRule.KnownDirectivesRule;
  }
});
Object.defineProperty(exports, "KnownFragmentNamesRule", {
  enumerable: true,
  get: function get() {
    return _KnownFragmentNamesRule.KnownFragmentNamesRule;
  }
});
Object.defineProperty(exports, "KnownTypeNamesRule", {
  enumerable: true,
  get: function get() {
    return _KnownTypeNamesRule.KnownTypeNamesRule;
  }
});
Object.defineProperty(exports, "LoneAnonymousOperationRule", {
  enumerable: true,
  get: function get() {
    return _LoneAnonymousOperationRule.LoneAnonymousOperationRule;
  }
});
Object.defineProperty(exports, "NoFragmentCyclesRule", {
  enumerable: true,
  get: function get() {
    return _NoFragmentCyclesRule.NoFragmentCyclesRule;
  }
});
Object.defineProperty(exports, "NoUndefinedVariablesRule", {
  enumerable: true,
  get: function get() {
    return _NoUndefinedVariablesRule.NoUndefinedVariablesRule;
  }
});
Object.defineProperty(exports, "NoUnusedFragmentsRule", {
  enumerable: true,
  get: function get() {
    return _NoUnusedFragmentsRule.NoUnusedFragmentsRule;
  }
});
Object.defineProperty(exports, "NoUnusedVariablesRule", {
  enumerable: true,
  get: function get() {
    return _NoUnusedVariablesRule.NoUnusedVariablesRule;
  }
});
Object.defineProperty(exports, "OverlappingFieldsCanBeMergedRule", {
  enumerable: true,
  get: function get() {
    return _OverlappingFieldsCanBeMergedRule.OverlappingFieldsCanBeMergedRule;
  }
});
Object.defineProperty(exports, "PossibleFragmentSpreadsRule", {
  enumerable: true,
  get: function get() {
    return _PossibleFragmentSpreadsRule.PossibleFragmentSpreadsRule;
  }
});
Object.defineProperty(exports, "ProvidedRequiredArgumentsRule", {
  enumerable: true,
  get: function get() {
    return _ProvidedRequiredArgumentsRule.ProvidedRequiredArgumentsRule;
  }
});
Object.defineProperty(exports, "ScalarLeafsRule", {
  enumerable: true,
  get: function get() {
    return _ScalarLeafsRule.ScalarLeafsRule;
  }
});
Object.defineProperty(exports, "SingleFieldSubscriptionsRule", {
  enumerable: true,
  get: function get() {
    return _SingleFieldSubscriptionsRule.SingleFieldSubscriptionsRule;
  }
});
Object.defineProperty(exports, "UniqueArgumentNamesRule", {
  enumerable: true,
  get: function get() {
    return _UniqueArgumentNamesRule.UniqueArgumentNamesRule;
  }
});
Object.defineProperty(exports, "UniqueDirectivesPerLocationRule", {
  enumerable: true,
  get: function get() {
    return _UniqueDirectivesPerLocationRule.UniqueDirectivesPerLocationRule;
  }
});
Object.defineProperty(exports, "UniqueFragmentNamesRule", {
  enumerable: true,
  get: function get() {
    return _UniqueFragmentNamesRule.UniqueFragmentNamesRule;
  }
});
Object.defineProperty(exports, "UniqueInputFieldNamesRule", {
  enumerable: true,
  get: function get() {
    return _UniqueInputFieldNamesRule.UniqueInputFieldNamesRule;
  }
});
Object.defineProperty(exports, "UniqueOperationNamesRule", {
  enumerable: true,
  get: function get() {
    return _UniqueOperationNamesRule.UniqueOperationNamesRule;
  }
});
Object.defineProperty(exports, "UniqueVariableNamesRule", {
  enumerable: true,
  get: function get() {
    return _UniqueVariableNamesRule.UniqueVariableNamesRule;
  }
});
Object.defineProperty(exports, "ValuesOfCorrectTypeRule", {
  enumerable: true,
  get: function get() {
    return _ValuesOfCorrectTypeRule.ValuesOfCorrectTypeRule;
  }
});
Object.defineProperty(exports, "VariablesAreInputTypesRule", {
  enumerable: true,
  get: function get() {
    return _VariablesAreInputTypesRule.VariablesAreInputTypesRule;
  }
});
Object.defineProperty(exports, "VariablesInAllowedPositionRule", {
  enumerable: true,
  get: function get() {
    return _VariablesInAllowedPositionRule.VariablesInAllowedPositionRule;
  }
});
Object.defineProperty(exports, "LoneSchemaDefinitionRule", {
  enumerable: true,
  get: function get() {
    return _LoneSchemaDefinitionRule.LoneSchemaDefinitionRule;
  }
});
Object.defineProperty(exports, "UniqueOperationTypesRule", {
  enumerable: true,
  get: function get() {
    return _UniqueOperationTypesRule.UniqueOperationTypesRule;
  }
});
Object.defineProperty(exports, "UniqueTypeNamesRule", {
  enumerable: true,
  get: function get() {
    return _UniqueTypeNamesRule.UniqueTypeNamesRule;
  }
});
Object.defineProperty(exports, "UniqueEnumValueNamesRule", {
  enumerable: true,
  get: function get() {
    return _UniqueEnumValueNamesRule.UniqueEnumValueNamesRule;
  }
});
Object.defineProperty(exports, "UniqueFieldDefinitionNamesRule", {
  enumerable: true,
  get: function get() {
    return _UniqueFieldDefinitionNamesRule.UniqueFieldDefinitionNamesRule;
  }
});
Object.defineProperty(exports, "UniqueDirectiveNamesRule", {
  enumerable: true,
  get: function get() {
    return _UniqueDirectiveNamesRule.UniqueDirectiveNamesRule;
  }
});
Object.defineProperty(exports, "PossibleTypeExtensionsRule", {
  enumerable: true,
  get: function get() {
    return _PossibleTypeExtensionsRule.PossibleTypeExtensionsRule;
  }
});
Object.defineProperty(exports, "NoDeprecatedCustomRule", {
  enumerable: true,
  get: function get() {
    return _NoDeprecatedCustomRule.NoDeprecatedCustomRule;
  }
});
Object.defineProperty(exports, "NoSchemaIntrospectionCustomRule", {
  enumerable: true,
  get: function get() {
    return _NoSchemaIntrospectionCustomRule.NoSchemaIntrospectionCustomRule;
  }
});

var _validate = require("./validate.js");

var _ValidationContext = require("./ValidationContext.js");

var _specifiedRules = require("./specifiedRules.js");

var _ExecutableDefinitionsRule = require("./rules/ExecutableDefinitionsRule.js");

var _FieldsOnCorrectTypeRule = require("./rules/FieldsOnCorrectTypeRule.js");

var _FragmentsOnCompositeTypesRule = require("./rules/FragmentsOnCompositeTypesRule.js");

var _KnownArgumentNamesRule = require("./rules/KnownArgumentNamesRule.js");

var _KnownDirectivesRule = require("./rules/KnownDirectivesRule.js");

var _KnownFragmentNamesRule = require("./rules/KnownFragmentNamesRule.js");

var _KnownTypeNamesRule = require("./rules/KnownTypeNamesRule.js");

var _LoneAnonymousOperationRule = require("./rules/LoneAnonymousOperationRule.js");

var _NoFragmentCyclesRule = require("./rules/NoFragmentCyclesRule.js");

var _NoUndefinedVariablesRule = require("./rules/NoUndefinedVariablesRule.js");

var _NoUnusedFragmentsRule = require("./rules/NoUnusedFragmentsRule.js");

var _NoUnusedVariablesRule = require("./rules/NoUnusedVariablesRule.js");

var _OverlappingFieldsCanBeMergedRule = require("./rules/OverlappingFieldsCanBeMergedRule.js");

var _PossibleFragmentSpreadsRule = require("./rules/PossibleFragmentSpreadsRule.js");

var _ProvidedRequiredArgumentsRule = require("./rules/ProvidedRequiredArgumentsRule.js");

var _ScalarLeafsRule = require("./rules/ScalarLeafsRule.js");

var _SingleFieldSubscriptionsRule = require("./rules/SingleFieldSubscriptionsRule.js");

var _UniqueArgumentNamesRule = require("./rules/UniqueArgumentNamesRule.js");

var _UniqueDirectivesPerLocationRule = require("./rules/UniqueDirectivesPerLocationRule.js");

var _UniqueFragmentNamesRule = require("./rules/UniqueFragmentNamesRule.js");

var _UniqueInputFieldNamesRule = require("./rules/UniqueInputFieldNamesRule.js");

var _UniqueOperationNamesRule = require("./rules/UniqueOperationNamesRule.js");

var _UniqueVariableNamesRule = require("./rules/UniqueVariableNamesRule.js");

var _ValuesOfCorrectTypeRule = require("./rules/ValuesOfCorrectTypeRule.js");

var _VariablesAreInputTypesRule = require("./rules/VariablesAreInputTypesRule.js");

var _VariablesInAllowedPositionRule = require("./rules/VariablesInAllowedPositionRule.js");

var _LoneSchemaDefinitionRule = require("./rules/LoneSchemaDefinitionRule.js");

var _UniqueOperationTypesRule = require("./rules/UniqueOperationTypesRule.js");

var _UniqueTypeNamesRule = require("./rules/UniqueTypeNamesRule.js");

var _UniqueEnumValueNamesRule = require("./rules/UniqueEnumValueNamesRule.js");

var _UniqueFieldDefinitionNamesRule = require("./rules/UniqueFieldDefinitionNamesRule.js");

var _UniqueDirectiveNamesRule = require("./rules/UniqueDirectiveNamesRule.js");

var _PossibleTypeExtensionsRule = require("./rules/PossibleTypeExtensionsRule.js");

var _NoDeprecatedCustomRule = require("./rules/custom/NoDeprecatedCustomRule.js");

var _NoSchemaIntrospectionCustomRule = require("./rules/custom/NoSchemaIntrospectionCustomRule.js");

},{"./ValidationContext.js":105,"./rules/ExecutableDefinitionsRule.js":107,"./rules/FieldsOnCorrectTypeRule.js":108,"./rules/FragmentsOnCompositeTypesRule.js":109,"./rules/KnownArgumentNamesRule.js":110,"./rules/KnownDirectivesRule.js":111,"./rules/KnownFragmentNamesRule.js":112,"./rules/KnownTypeNamesRule.js":113,"./rules/LoneAnonymousOperationRule.js":114,"./rules/LoneSchemaDefinitionRule.js":115,"./rules/NoFragmentCyclesRule.js":116,"./rules/NoUndefinedVariablesRule.js":117,"./rules/NoUnusedFragmentsRule.js":118,"./rules/NoUnusedVariablesRule.js":119,"./rules/OverlappingFieldsCanBeMergedRule.js":120,"./rules/PossibleFragmentSpreadsRule.js":121,"./rules/PossibleTypeExtensionsRule.js":122,"./rules/ProvidedRequiredArgumentsRule.js":123,"./rules/ScalarLeafsRule.js":124,"./rules/SingleFieldSubscriptionsRule.js":125,"./rules/UniqueArgumentNamesRule.js":126,"./rules/UniqueDirectiveNamesRule.js":127,"./rules/UniqueDirectivesPerLocationRule.js":128,"./rules/UniqueEnumValueNamesRule.js":129,"./rules/UniqueFieldDefinitionNamesRule.js":130,"./rules/UniqueFragmentNamesRule.js":131,"./rules/UniqueInputFieldNamesRule.js":132,"./rules/UniqueOperationNamesRule.js":133,"./rules/UniqueOperationTypesRule.js":134,"./rules/UniqueTypeNamesRule.js":135,"./rules/UniqueVariableNamesRule.js":136,"./rules/ValuesOfCorrectTypeRule.js":137,"./rules/VariablesAreInputTypesRule.js":138,"./rules/VariablesInAllowedPositionRule.js":139,"./rules/custom/NoDeprecatedCustomRule.js":140,"./rules/custom/NoSchemaIntrospectionCustomRule.js":141,"./specifiedRules.js":142,"./validate.js":143}],107:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ExecutableDefinitionsRule = ExecutableDefinitionsRule;

var _GraphQLError = require("../../error/GraphQLError.js");

var _kinds = require("../../language/kinds.js");

var _predicates = require("../../language/predicates.js");

/**
 * Executable definitions
 *
 * A GraphQL document is only valid for execution if all definitions are either
 * operation or fragment definitions.
 */
function ExecutableDefinitionsRule(context) {
  return {
    Document: function Document(node) {
      for (var _i2 = 0, _node$definitions2 = node.definitions; _i2 < _node$definitions2.length; _i2++) {
        var definition = _node$definitions2[_i2];

        if (!(0, _predicates.isExecutableDefinitionNode)(definition)) {
          var defName = definition.kind === _kinds.Kind.SCHEMA_DEFINITION || definition.kind === _kinds.Kind.SCHEMA_EXTENSION ? 'schema' : '"' + definition.name.value + '"';
          context.reportError(new _GraphQLError.GraphQLError("The ".concat(defName, " definition is not executable."), definition));
        }
      }

      return false;
    }
  };
}

},{"../../error/GraphQLError.js":18,"../../language/kinds.js":55,"../../language/predicates.js":59}],108:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.FieldsOnCorrectTypeRule = FieldsOnCorrectTypeRule;

var _arrayFrom = _interopRequireDefault(require("../../polyfills/arrayFrom.js"));

var _didYouMean = _interopRequireDefault(require("../../jsutils/didYouMean.js"));

var _suggestionList = _interopRequireDefault(require("../../jsutils/suggestionList.js"));

var _naturalCompare = _interopRequireDefault(require("../../jsutils/naturalCompare.js"));

var _GraphQLError = require("../../error/GraphQLError.js");

var _definition = require("../../type/definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Fields on correct type
 *
 * A GraphQL document is only valid if all fields selected are defined by the
 * parent type, or are an allowed meta field such as __typename.
 */
function FieldsOnCorrectTypeRule(context) {
  return {
    Field: function Field(node) {
      var type = context.getParentType();

      if (type) {
        var fieldDef = context.getFieldDef();

        if (!fieldDef) {
          // This field doesn't exist, lets look for suggestions.
          var schema = context.getSchema();
          var fieldName = node.name.value; // First determine if there are any suggested types to condition on.

          var suggestion = (0, _didYouMean.default)('to use an inline fragment on', getSuggestedTypeNames(schema, type, fieldName)); // If there are no suggested types, then perhaps this was a typo?

          if (suggestion === '') {
            suggestion = (0, _didYouMean.default)(getSuggestedFieldNames(type, fieldName));
          } // Report an error, including helpful suggestions.


          context.reportError(new _GraphQLError.GraphQLError("Cannot query field \"".concat(fieldName, "\" on type \"").concat(type.name, "\".") + suggestion, node));
        }
      }
    }
  };
}
/**
 * Go through all of the implementations of type, as well as the interfaces that
 * they implement. If any of those types include the provided field, suggest them,
 * sorted by how often the type is referenced.
 */


function getSuggestedTypeNames(schema, type, fieldName) {
  if (!(0, _definition.isAbstractType)(type)) {
    // Must be an Object type, which does not have possible fields.
    return [];
  }

  var suggestedTypes = new Set();
  var usageCount = Object.create(null);

  for (var _i2 = 0, _schema$getPossibleTy2 = schema.getPossibleTypes(type); _i2 < _schema$getPossibleTy2.length; _i2++) {
    var possibleType = _schema$getPossibleTy2[_i2];

    if (!possibleType.getFields()[fieldName]) {
      continue;
    } // This object type defines this field.


    suggestedTypes.add(possibleType);
    usageCount[possibleType.name] = 1;

    for (var _i4 = 0, _possibleType$getInte2 = possibleType.getInterfaces(); _i4 < _possibleType$getInte2.length; _i4++) {
      var _usageCount$possibleI;

      var possibleInterface = _possibleType$getInte2[_i4];

      if (!possibleInterface.getFields()[fieldName]) {
        continue;
      } // This interface type defines this field.


      suggestedTypes.add(possibleInterface);
      usageCount[possibleInterface.name] = ((_usageCount$possibleI = usageCount[possibleInterface.name]) !== null && _usageCount$possibleI !== void 0 ? _usageCount$possibleI : 0) + 1;
    }
  }

  return (0, _arrayFrom.default)(suggestedTypes).sort(function (typeA, typeB) {
    // Suggest both interface and object types based on how common they are.
    var usageCountDiff = usageCount[typeB.name] - usageCount[typeA.name];

    if (usageCountDiff !== 0) {
      return usageCountDiff;
    } // Suggest super types first followed by subtypes


    if ((0, _definition.isInterfaceType)(typeA) && schema.isSubType(typeA, typeB)) {
      return -1;
    }

    if ((0, _definition.isInterfaceType)(typeB) && schema.isSubType(typeB, typeA)) {
      return 1;
    }

    return (0, _naturalCompare.default)(typeA.name, typeB.name);
  }).map(function (x) {
    return x.name;
  });
}
/**
 * For the field name provided, determine if there are any similar field names
 * that may be the result of a typo.
 */


function getSuggestedFieldNames(type, fieldName) {
  if ((0, _definition.isObjectType)(type) || (0, _definition.isInterfaceType)(type)) {
    var possibleFieldNames = Object.keys(type.getFields());
    return (0, _suggestionList.default)(fieldName, possibleFieldNames);
  } // Otherwise, must be a Union type, which does not define fields.


  return [];
}

},{"../../error/GraphQLError.js":18,"../../jsutils/didYouMean.js":31,"../../jsutils/naturalCompare.js":43,"../../jsutils/suggestionList.js":49,"../../polyfills/arrayFrom.js":65,"../../type/definition.js":75}],109:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.FragmentsOnCompositeTypesRule = FragmentsOnCompositeTypesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

var _printer = require("../../language/printer.js");

var _definition = require("../../type/definition.js");

var _typeFromAST = require("../../utilities/typeFromAST.js");

/**
 * Fragments on composite type
 *
 * Fragments use a type condition to determine if they apply, since fragments
 * can only be spread into a composite type (object, interface, or union), the
 * type condition must also be a composite type.
 */
function FragmentsOnCompositeTypesRule(context) {
  return {
    InlineFragment: function InlineFragment(node) {
      var typeCondition = node.typeCondition;

      if (typeCondition) {
        var type = (0, _typeFromAST.typeFromAST)(context.getSchema(), typeCondition);

        if (type && !(0, _definition.isCompositeType)(type)) {
          var typeStr = (0, _printer.print)(typeCondition);
          context.reportError(new _GraphQLError.GraphQLError("Fragment cannot condition on non composite type \"".concat(typeStr, "\"."), typeCondition));
        }
      }
    },
    FragmentDefinition: function FragmentDefinition(node) {
      var type = (0, _typeFromAST.typeFromAST)(context.getSchema(), node.typeCondition);

      if (type && !(0, _definition.isCompositeType)(type)) {
        var typeStr = (0, _printer.print)(node.typeCondition);
        context.reportError(new _GraphQLError.GraphQLError("Fragment \"".concat(node.name.value, "\" cannot condition on non composite type \"").concat(typeStr, "\"."), node.typeCondition));
      }
    }
  };
}

},{"../../error/GraphQLError.js":18,"../../language/printer.js":61,"../../type/definition.js":75,"../../utilities/typeFromAST.js":102}],110:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.KnownArgumentNamesRule = KnownArgumentNamesRule;
exports.KnownArgumentNamesOnDirectivesRule = KnownArgumentNamesOnDirectivesRule;

var _didYouMean = _interopRequireDefault(require("../../jsutils/didYouMean.js"));

var _suggestionList = _interopRequireDefault(require("../../jsutils/suggestionList.js"));

var _GraphQLError = require("../../error/GraphQLError.js");

var _kinds = require("../../language/kinds.js");

var _directives = require("../../type/directives.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function ownKeys(object, enumerableOnly) { var keys = Object.keys(object); if (Object.getOwnPropertySymbols) { var symbols = Object.getOwnPropertySymbols(object); if (enumerableOnly) symbols = symbols.filter(function (sym) { return Object.getOwnPropertyDescriptor(object, sym).enumerable; }); keys.push.apply(keys, symbols); } return keys; }

function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; if (i % 2) { ownKeys(Object(source), true).forEach(function (key) { _defineProperty(target, key, source[key]); }); } else if (Object.getOwnPropertyDescriptors) { Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)); } else { ownKeys(Object(source)).forEach(function (key) { Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key)); }); } } return target; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

/**
 * Known argument names
 *
 * A GraphQL field is only valid if all supplied arguments are defined by
 * that field.
 */
function KnownArgumentNamesRule(context) {
  return _objectSpread(_objectSpread({}, KnownArgumentNamesOnDirectivesRule(context)), {}, {
    Argument: function Argument(argNode) {
      var argDef = context.getArgument();
      var fieldDef = context.getFieldDef();
      var parentType = context.getParentType();

      if (!argDef && fieldDef && parentType) {
        var argName = argNode.name.value;
        var knownArgsNames = fieldDef.args.map(function (arg) {
          return arg.name;
        });
        var suggestions = (0, _suggestionList.default)(argName, knownArgsNames);
        context.reportError(new _GraphQLError.GraphQLError("Unknown argument \"".concat(argName, "\" on field \"").concat(parentType.name, ".").concat(fieldDef.name, "\".") + (0, _didYouMean.default)(suggestions), argNode));
      }
    }
  });
}
/**
 * @internal
 */


function KnownArgumentNamesOnDirectivesRule(context) {
  var directiveArgs = Object.create(null);
  var schema = context.getSchema();
  var definedDirectives = schema ? schema.getDirectives() : _directives.specifiedDirectives;

  for (var _i2 = 0; _i2 < definedDirectives.length; _i2++) {
    var directive = definedDirectives[_i2];
    directiveArgs[directive.name] = directive.args.map(function (arg) {
      return arg.name;
    });
  }

  var astDefinitions = context.getDocument().definitions;

  for (var _i4 = 0; _i4 < astDefinitions.length; _i4++) {
    var def = astDefinitions[_i4];

    if (def.kind === _kinds.Kind.DIRECTIVE_DEFINITION) {
      var _def$arguments;

      // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')
      var argsNodes = (_def$arguments = def.arguments) !== null && _def$arguments !== void 0 ? _def$arguments : [];
      directiveArgs[def.name.value] = argsNodes.map(function (arg) {
        return arg.name.value;
      });
    }
  }

  return {
    Directive: function Directive(directiveNode) {
      var directiveName = directiveNode.name.value;
      var knownArgs = directiveArgs[directiveName];

      if (directiveNode.arguments && knownArgs) {
        for (var _i6 = 0, _directiveNode$argume2 = directiveNode.arguments; _i6 < _directiveNode$argume2.length; _i6++) {
          var argNode = _directiveNode$argume2[_i6];
          var argName = argNode.name.value;

          if (knownArgs.indexOf(argName) === -1) {
            var suggestions = (0, _suggestionList.default)(argName, knownArgs);
            context.reportError(new _GraphQLError.GraphQLError("Unknown argument \"".concat(argName, "\" on directive \"@").concat(directiveName, "\".") + (0, _didYouMean.default)(suggestions), argNode));
          }
        }
      }

      return false;
    }
  };
}

},{"../../error/GraphQLError.js":18,"../../jsutils/didYouMean.js":31,"../../jsutils/suggestionList.js":49,"../../language/kinds.js":55,"../../type/directives.js":76}],111:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.KnownDirectivesRule = KnownDirectivesRule;

var _inspect = _interopRequireDefault(require("../../jsutils/inspect.js"));

var _invariant = _interopRequireDefault(require("../../jsutils/invariant.js"));

var _GraphQLError = require("../../error/GraphQLError.js");

var _kinds = require("../../language/kinds.js");

var _directiveLocation = require("../../language/directiveLocation.js");

var _directives = require("../../type/directives.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Known directives
 *
 * A GraphQL document is only valid if all `@directives` are known by the
 * schema and legally positioned.
 */
function KnownDirectivesRule(context) {
  var locationsMap = Object.create(null);
  var schema = context.getSchema();
  var definedDirectives = schema ? schema.getDirectives() : _directives.specifiedDirectives;

  for (var _i2 = 0; _i2 < definedDirectives.length; _i2++) {
    var directive = definedDirectives[_i2];
    locationsMap[directive.name] = directive.locations;
  }

  var astDefinitions = context.getDocument().definitions;

  for (var _i4 = 0; _i4 < astDefinitions.length; _i4++) {
    var def = astDefinitions[_i4];

    if (def.kind === _kinds.Kind.DIRECTIVE_DEFINITION) {
      locationsMap[def.name.value] = def.locations.map(function (name) {
        return name.value;
      });
    }
  }

  return {
    Directive: function Directive(node, _key, _parent, _path, ancestors) {
      var name = node.name.value;
      var locations = locationsMap[name];

      if (!locations) {
        context.reportError(new _GraphQLError.GraphQLError("Unknown directive \"@".concat(name, "\"."), node));
        return;
      }

      var candidateLocation = getDirectiveLocationForASTPath(ancestors);

      if (candidateLocation && locations.indexOf(candidateLocation) === -1) {
        context.reportError(new _GraphQLError.GraphQLError("Directive \"@".concat(name, "\" may not be used on ").concat(candidateLocation, "."), node));
      }
    }
  };
}

function getDirectiveLocationForASTPath(ancestors) {
  var appliedTo = ancestors[ancestors.length - 1];
  !Array.isArray(appliedTo) || (0, _invariant.default)(0);

  switch (appliedTo.kind) {
    case _kinds.Kind.OPERATION_DEFINITION:
      return getDirectiveLocationForOperation(appliedTo.operation);

    case _kinds.Kind.FIELD:
      return _directiveLocation.DirectiveLocation.FIELD;

    case _kinds.Kind.FRAGMENT_SPREAD:
      return _directiveLocation.DirectiveLocation.FRAGMENT_SPREAD;

    case _kinds.Kind.INLINE_FRAGMENT:
      return _directiveLocation.DirectiveLocation.INLINE_FRAGMENT;

    case _kinds.Kind.FRAGMENT_DEFINITION:
      return _directiveLocation.DirectiveLocation.FRAGMENT_DEFINITION;

    case _kinds.Kind.VARIABLE_DEFINITION:
      return _directiveLocation.DirectiveLocation.VARIABLE_DEFINITION;

    case _kinds.Kind.SCHEMA_DEFINITION:
    case _kinds.Kind.SCHEMA_EXTENSION:
      return _directiveLocation.DirectiveLocation.SCHEMA;

    case _kinds.Kind.SCALAR_TYPE_DEFINITION:
    case _kinds.Kind.SCALAR_TYPE_EXTENSION:
      return _directiveLocation.DirectiveLocation.SCALAR;

    case _kinds.Kind.OBJECT_TYPE_DEFINITION:
    case _kinds.Kind.OBJECT_TYPE_EXTENSION:
      return _directiveLocation.DirectiveLocation.OBJECT;

    case _kinds.Kind.FIELD_DEFINITION:
      return _directiveLocation.DirectiveLocation.FIELD_DEFINITION;

    case _kinds.Kind.INTERFACE_TYPE_DEFINITION:
    case _kinds.Kind.INTERFACE_TYPE_EXTENSION:
      return _directiveLocation.DirectiveLocation.INTERFACE;

    case _kinds.Kind.UNION_TYPE_DEFINITION:
    case _kinds.Kind.UNION_TYPE_EXTENSION:
      return _directiveLocation.DirectiveLocation.UNION;

    case _kinds.Kind.ENUM_TYPE_DEFINITION:
    case _kinds.Kind.ENUM_TYPE_EXTENSION:
      return _directiveLocation.DirectiveLocation.ENUM;

    case _kinds.Kind.ENUM_VALUE_DEFINITION:
      return _directiveLocation.DirectiveLocation.ENUM_VALUE;

    case _kinds.Kind.INPUT_OBJECT_TYPE_DEFINITION:
    case _kinds.Kind.INPUT_OBJECT_TYPE_EXTENSION:
      return _directiveLocation.DirectiveLocation.INPUT_OBJECT;

    case _kinds.Kind.INPUT_VALUE_DEFINITION:
      {
        var parentNode = ancestors[ancestors.length - 3];
        return parentNode.kind === _kinds.Kind.INPUT_OBJECT_TYPE_DEFINITION ? _directiveLocation.DirectiveLocation.INPUT_FIELD_DEFINITION : _directiveLocation.DirectiveLocation.ARGUMENT_DEFINITION;
      }
  }
}

function getDirectiveLocationForOperation(operation) {
  switch (operation) {
    case 'query':
      return _directiveLocation.DirectiveLocation.QUERY;

    case 'mutation':
      return _directiveLocation.DirectiveLocation.MUTATION;

    case 'subscription':
      return _directiveLocation.DirectiveLocation.SUBSCRIPTION;
  } // istanbul ignore next (Not reachable. All possible types have been considered)


  false || (0, _invariant.default)(0, 'Unexpected operation: ' + (0, _inspect.default)(operation));
}

},{"../../error/GraphQLError.js":18,"../../jsutils/inspect.js":33,"../../jsutils/invariant.js":35,"../../language/directiveLocation.js":53,"../../language/kinds.js":55,"../../type/directives.js":76}],112:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.KnownFragmentNamesRule = KnownFragmentNamesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * Known fragment names
 *
 * A GraphQL document is only valid if all `...Fragment` fragment spreads refer
 * to fragments defined in the same document.
 */
function KnownFragmentNamesRule(context) {
  return {
    FragmentSpread: function FragmentSpread(node) {
      var fragmentName = node.name.value;
      var fragment = context.getFragment(fragmentName);

      if (!fragment) {
        context.reportError(new _GraphQLError.GraphQLError("Unknown fragment \"".concat(fragmentName, "\"."), node.name));
      }
    }
  };
}

},{"../../error/GraphQLError.js":18}],113:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.KnownTypeNamesRule = KnownTypeNamesRule;

var _didYouMean = _interopRequireDefault(require("../../jsutils/didYouMean.js"));

var _suggestionList = _interopRequireDefault(require("../../jsutils/suggestionList.js"));

var _GraphQLError = require("../../error/GraphQLError.js");

var _predicates = require("../../language/predicates.js");

var _scalars = require("../../type/scalars.js");

var _introspection = require("../../type/introspection.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Known type names
 *
 * A GraphQL document is only valid if referenced types (specifically
 * variable definitions and fragment conditions) are defined by the type schema.
 */
function KnownTypeNamesRule(context) {
  var schema = context.getSchema();
  var existingTypesMap = schema ? schema.getTypeMap() : Object.create(null);
  var definedTypes = Object.create(null);

  for (var _i2 = 0, _context$getDocument$2 = context.getDocument().definitions; _i2 < _context$getDocument$2.length; _i2++) {
    var def = _context$getDocument$2[_i2];

    if ((0, _predicates.isTypeDefinitionNode)(def)) {
      definedTypes[def.name.value] = true;
    }
  }

  var typeNames = Object.keys(existingTypesMap).concat(Object.keys(definedTypes));
  return {
    NamedType: function NamedType(node, _1, parent, _2, ancestors) {
      var typeName = node.name.value;

      if (!existingTypesMap[typeName] && !definedTypes[typeName]) {
        var _ancestors$;

        var definitionNode = (_ancestors$ = ancestors[2]) !== null && _ancestors$ !== void 0 ? _ancestors$ : parent;
        var isSDL = definitionNode != null && isSDLNode(definitionNode);

        if (isSDL && isStandardTypeName(typeName)) {
          return;
        }

        var suggestedTypes = (0, _suggestionList.default)(typeName, isSDL ? standardTypeNames.concat(typeNames) : typeNames);
        context.reportError(new _GraphQLError.GraphQLError("Unknown type \"".concat(typeName, "\".") + (0, _didYouMean.default)(suggestedTypes), node));
      }
    }
  };
}

var standardTypeNames = [].concat(_scalars.specifiedScalarTypes, _introspection.introspectionTypes).map(function (type) {
  return type.name;
});

function isStandardTypeName(typeName) {
  return standardTypeNames.indexOf(typeName) !== -1;
}

function isSDLNode(value) {
  return !Array.isArray(value) && ((0, _predicates.isTypeSystemDefinitionNode)(value) || (0, _predicates.isTypeSystemExtensionNode)(value));
}

},{"../../error/GraphQLError.js":18,"../../jsutils/didYouMean.js":31,"../../jsutils/suggestionList.js":49,"../../language/predicates.js":59,"../../type/introspection.js":78,"../../type/scalars.js":79}],114:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.LoneAnonymousOperationRule = LoneAnonymousOperationRule;

var _GraphQLError = require("../../error/GraphQLError.js");

var _kinds = require("../../language/kinds.js");

/**
 * Lone anonymous operation
 *
 * A GraphQL document is only valid if when it contains an anonymous operation
 * (the query short-hand) that it contains only that one operation definition.
 */
function LoneAnonymousOperationRule(context) {
  var operationCount = 0;
  return {
    Document: function Document(node) {
      operationCount = node.definitions.filter(function (definition) {
        return definition.kind === _kinds.Kind.OPERATION_DEFINITION;
      }).length;
    },
    OperationDefinition: function OperationDefinition(node) {
      if (!node.name && operationCount > 1) {
        context.reportError(new _GraphQLError.GraphQLError('This anonymous operation must be the only defined operation.', node));
      }
    }
  };
}

},{"../../error/GraphQLError.js":18,"../../language/kinds.js":55}],115:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.LoneSchemaDefinitionRule = LoneSchemaDefinitionRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * Lone Schema definition
 *
 * A GraphQL document is only valid if it contains only one schema definition.
 */
function LoneSchemaDefinitionRule(context) {
  var _ref, _ref2, _oldSchema$astNode;

  var oldSchema = context.getSchema();
  var alreadyDefined = (_ref = (_ref2 = (_oldSchema$astNode = oldSchema === null || oldSchema === void 0 ? void 0 : oldSchema.astNode) !== null && _oldSchema$astNode !== void 0 ? _oldSchema$astNode : oldSchema === null || oldSchema === void 0 ? void 0 : oldSchema.getQueryType()) !== null && _ref2 !== void 0 ? _ref2 : oldSchema === null || oldSchema === void 0 ? void 0 : oldSchema.getMutationType()) !== null && _ref !== void 0 ? _ref : oldSchema === null || oldSchema === void 0 ? void 0 : oldSchema.getSubscriptionType();
  var schemaDefinitionsCount = 0;
  return {
    SchemaDefinition: function SchemaDefinition(node) {
      if (alreadyDefined) {
        context.reportError(new _GraphQLError.GraphQLError('Cannot define a new schema within a schema extension.', node));
        return;
      }

      if (schemaDefinitionsCount > 0) {
        context.reportError(new _GraphQLError.GraphQLError('Must provide only one schema definition.', node));
      }

      ++schemaDefinitionsCount;
    }
  };
}

},{"../../error/GraphQLError.js":18}],116:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.NoFragmentCyclesRule = NoFragmentCyclesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

function NoFragmentCyclesRule(context) {
  // Tracks already visited fragments to maintain O(N) and to ensure that cycles
  // are not redundantly reported.
  var visitedFrags = Object.create(null); // Array of AST nodes used to produce meaningful errors

  var spreadPath = []; // Position in the spread path

  var spreadPathIndexByName = Object.create(null);
  return {
    OperationDefinition: function OperationDefinition() {
      return false;
    },
    FragmentDefinition: function FragmentDefinition(node) {
      detectCycleRecursive(node);
      return false;
    }
  }; // This does a straight-forward DFS to find cycles.
  // It does not terminate when a cycle was found but continues to explore
  // the graph to find all possible cycles.

  function detectCycleRecursive(fragment) {
    if (visitedFrags[fragment.name.value]) {
      return;
    }

    var fragmentName = fragment.name.value;
    visitedFrags[fragmentName] = true;
    var spreadNodes = context.getFragmentSpreads(fragment.selectionSet);

    if (spreadNodes.length === 0) {
      return;
    }

    spreadPathIndexByName[fragmentName] = spreadPath.length;

    for (var _i2 = 0; _i2 < spreadNodes.length; _i2++) {
      var spreadNode = spreadNodes[_i2];
      var spreadName = spreadNode.name.value;
      var cycleIndex = spreadPathIndexByName[spreadName];
      spreadPath.push(spreadNode);

      if (cycleIndex === undefined) {
        var spreadFragment = context.getFragment(spreadName);

        if (spreadFragment) {
          detectCycleRecursive(spreadFragment);
        }
      } else {
        var cyclePath = spreadPath.slice(cycleIndex);
        var viaPath = cyclePath.slice(0, -1).map(function (s) {
          return '"' + s.name.value + '"';
        }).join(', ');
        context.reportError(new _GraphQLError.GraphQLError("Cannot spread fragment \"".concat(spreadName, "\" within itself") + (viaPath !== '' ? " via ".concat(viaPath, ".") : '.'), cyclePath));
      }

      spreadPath.pop();
    }

    spreadPathIndexByName[fragmentName] = undefined;
  }
}

},{"../../error/GraphQLError.js":18}],117:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.NoUndefinedVariablesRule = NoUndefinedVariablesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * No undefined variables
 *
 * A GraphQL operation is only valid if all variables encountered, both directly
 * and via fragment spreads, are defined by that operation.
 */
function NoUndefinedVariablesRule(context) {
  var variableNameDefined = Object.create(null);
  return {
    OperationDefinition: {
      enter: function enter() {
        variableNameDefined = Object.create(null);
      },
      leave: function leave(operation) {
        var usages = context.getRecursiveVariableUsages(operation);

        for (var _i2 = 0; _i2 < usages.length; _i2++) {
          var _ref2 = usages[_i2];
          var node = _ref2.node;
          var varName = node.name.value;

          if (variableNameDefined[varName] !== true) {
            context.reportError(new _GraphQLError.GraphQLError(operation.name ? "Variable \"$".concat(varName, "\" is not defined by operation \"").concat(operation.name.value, "\".") : "Variable \"$".concat(varName, "\" is not defined."), [node, operation]));
          }
        }
      }
    },
    VariableDefinition: function VariableDefinition(node) {
      variableNameDefined[node.variable.name.value] = true;
    }
  };
}

},{"../../error/GraphQLError.js":18}],118:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.NoUnusedFragmentsRule = NoUnusedFragmentsRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * No unused fragments
 *
 * A GraphQL document is only valid if all fragment definitions are spread
 * within operations, or spread within other fragments spread within operations.
 */
function NoUnusedFragmentsRule(context) {
  var operationDefs = [];
  var fragmentDefs = [];
  return {
    OperationDefinition: function OperationDefinition(node) {
      operationDefs.push(node);
      return false;
    },
    FragmentDefinition: function FragmentDefinition(node) {
      fragmentDefs.push(node);
      return false;
    },
    Document: {
      leave: function leave() {
        var fragmentNameUsed = Object.create(null);

        for (var _i2 = 0; _i2 < operationDefs.length; _i2++) {
          var operation = operationDefs[_i2];

          for (var _i4 = 0, _context$getRecursive2 = context.getRecursivelyReferencedFragments(operation); _i4 < _context$getRecursive2.length; _i4++) {
            var fragment = _context$getRecursive2[_i4];
            fragmentNameUsed[fragment.name.value] = true;
          }
        }

        for (var _i6 = 0; _i6 < fragmentDefs.length; _i6++) {
          var fragmentDef = fragmentDefs[_i6];
          var fragName = fragmentDef.name.value;

          if (fragmentNameUsed[fragName] !== true) {
            context.reportError(new _GraphQLError.GraphQLError("Fragment \"".concat(fragName, "\" is never used."), fragmentDef));
          }
        }
      }
    }
  };
}

},{"../../error/GraphQLError.js":18}],119:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.NoUnusedVariablesRule = NoUnusedVariablesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * No unused variables
 *
 * A GraphQL operation is only valid if all variables defined by an operation
 * are used, either directly or within a spread fragment.
 */
function NoUnusedVariablesRule(context) {
  var variableDefs = [];
  return {
    OperationDefinition: {
      enter: function enter() {
        variableDefs = [];
      },
      leave: function leave(operation) {
        var variableNameUsed = Object.create(null);
        var usages = context.getRecursiveVariableUsages(operation);

        for (var _i2 = 0; _i2 < usages.length; _i2++) {
          var _ref2 = usages[_i2];
          var node = _ref2.node;
          variableNameUsed[node.name.value] = true;
        }

        for (var _i4 = 0, _variableDefs2 = variableDefs; _i4 < _variableDefs2.length; _i4++) {
          var variableDef = _variableDefs2[_i4];
          var variableName = variableDef.variable.name.value;

          if (variableNameUsed[variableName] !== true) {
            context.reportError(new _GraphQLError.GraphQLError(operation.name ? "Variable \"$".concat(variableName, "\" is never used in operation \"").concat(operation.name.value, "\".") : "Variable \"$".concat(variableName, "\" is never used."), variableDef));
          }
        }
      }
    },
    VariableDefinition: function VariableDefinition(def) {
      variableDefs.push(def);
    }
  };
}

},{"../../error/GraphQLError.js":18}],120:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.OverlappingFieldsCanBeMergedRule = OverlappingFieldsCanBeMergedRule;

var _find = _interopRequireDefault(require("../../polyfills/find.js"));

var _objectEntries3 = _interopRequireDefault(require("../../polyfills/objectEntries.js"));

var _inspect = _interopRequireDefault(require("../../jsutils/inspect.js"));

var _GraphQLError = require("../../error/GraphQLError.js");

var _kinds = require("../../language/kinds.js");

var _printer = require("../../language/printer.js");

var _definition = require("../../type/definition.js");

var _typeFromAST = require("../../utilities/typeFromAST.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function reasonMessage(reason) {
  if (Array.isArray(reason)) {
    return reason.map(function (_ref) {
      var responseName = _ref[0],
          subReason = _ref[1];
      return "subfields \"".concat(responseName, "\" conflict because ") + reasonMessage(subReason);
    }).join(' and ');
  }

  return reason;
}
/**
 * Overlapping fields can be merged
 *
 * A selection set is only valid if all fields (including spreading any
 * fragments) either correspond to distinct response names or can be merged
 * without ambiguity.
 */


function OverlappingFieldsCanBeMergedRule(context) {
  // A memoization for when two fragments are compared "between" each other for
  // conflicts. Two fragments may be compared many times, so memoizing this can
  // dramatically improve the performance of this validator.
  var comparedFragmentPairs = new PairSet(); // A cache for the "field map" and list of fragment names found in any given
  // selection set. Selection sets may be asked for this information multiple
  // times, so this improves the performance of this validator.

  var cachedFieldsAndFragmentNames = new Map();
  return {
    SelectionSet: function SelectionSet(selectionSet) {
      var conflicts = findConflictsWithinSelectionSet(context, cachedFieldsAndFragmentNames, comparedFragmentPairs, context.getParentType(), selectionSet);

      for (var _i2 = 0; _i2 < conflicts.length; _i2++) {
        var _ref3 = conflicts[_i2];
        var _ref2$ = _ref3[0];
        var responseName = _ref2$[0];
        var reason = _ref2$[1];
        var fields1 = _ref3[1];
        var fields2 = _ref3[2];
        var reasonMsg = reasonMessage(reason);
        context.reportError(new _GraphQLError.GraphQLError("Fields \"".concat(responseName, "\" conflict because ").concat(reasonMsg, ". Use different aliases on the fields to fetch both if this was intentional."), fields1.concat(fields2)));
      }
    }
  };
}

/**
 * Algorithm:
 *
 * Conflicts occur when two fields exist in a query which will produce the same
 * response name, but represent differing values, thus creating a conflict.
 * The algorithm below finds all conflicts via making a series of comparisons
 * between fields. In order to compare as few fields as possible, this makes
 * a series of comparisons "within" sets of fields and "between" sets of fields.
 *
 * Given any selection set, a collection produces both a set of fields by
 * also including all inline fragments, as well as a list of fragments
 * referenced by fragment spreads.
 *
 * A) Each selection set represented in the document first compares "within" its
 * collected set of fields, finding any conflicts between every pair of
 * overlapping fields.
 * Note: This is the *only time* that a the fields "within" a set are compared
 * to each other. After this only fields "between" sets are compared.
 *
 * B) Also, if any fragment is referenced in a selection set, then a
 * comparison is made "between" the original set of fields and the
 * referenced fragment.
 *
 * C) Also, if multiple fragments are referenced, then comparisons
 * are made "between" each referenced fragment.
 *
 * D) When comparing "between" a set of fields and a referenced fragment, first
 * a comparison is made between each field in the original set of fields and
 * each field in the the referenced set of fields.
 *
 * E) Also, if any fragment is referenced in the referenced selection set,
 * then a comparison is made "between" the original set of fields and the
 * referenced fragment (recursively referring to step D).
 *
 * F) When comparing "between" two fragments, first a comparison is made between
 * each field in the first referenced set of fields and each field in the the
 * second referenced set of fields.
 *
 * G) Also, any fragments referenced by the first must be compared to the
 * second, and any fragments referenced by the second must be compared to the
 * first (recursively referring to step F).
 *
 * H) When comparing two fields, if both have selection sets, then a comparison
 * is made "between" both selection sets, first comparing the set of fields in
 * the first selection set with the set of fields in the second.
 *
 * I) Also, if any fragment is referenced in either selection set, then a
 * comparison is made "between" the other set of fields and the
 * referenced fragment.
 *
 * J) Also, if two fragments are referenced in both selection sets, then a
 * comparison is made "between" the two fragments.
 *
 */
// Find all conflicts found "within" a selection set, including those found
// via spreading in fragments. Called when visiting each SelectionSet in the
// GraphQL Document.
function findConflictsWithinSelectionSet(context, cachedFieldsAndFragmentNames, comparedFragmentPairs, parentType, selectionSet) {
  var conflicts = [];

  var _getFieldsAndFragment = getFieldsAndFragmentNames(context, cachedFieldsAndFragmentNames, parentType, selectionSet),
      fieldMap = _getFieldsAndFragment[0],
      fragmentNames = _getFieldsAndFragment[1]; // (A) Find find all conflicts "within" the fields of this selection set.
  // Note: this is the *only place* `collectConflictsWithin` is called.


  collectConflictsWithin(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, fieldMap);

  if (fragmentNames.length !== 0) {
    // (B) Then collect conflicts between these fields and those represented by
    // each spread fragment name found.
    for (var i = 0; i < fragmentNames.length; i++) {
      collectConflictsBetweenFieldsAndFragment(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, false, fieldMap, fragmentNames[i]); // (C) Then compare this fragment with all other fragments found in this
      // selection set to collect conflicts between fragments spread together.
      // This compares each item in the list of fragment names to every other
      // item in that same list (except for itself).

      for (var j = i + 1; j < fragmentNames.length; j++) {
        collectConflictsBetweenFragments(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, false, fragmentNames[i], fragmentNames[j]);
      }
    }
  }

  return conflicts;
} // Collect all conflicts found between a set of fields and a fragment reference
// including via spreading in any nested fragments.


function collectConflictsBetweenFieldsAndFragment(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, areMutuallyExclusive, fieldMap, fragmentName) {
  var fragment = context.getFragment(fragmentName);

  if (!fragment) {
    return;
  }

  var _getReferencedFieldsA = getReferencedFieldsAndFragmentNames(context, cachedFieldsAndFragmentNames, fragment),
      fieldMap2 = _getReferencedFieldsA[0],
      fragmentNames2 = _getReferencedFieldsA[1]; // Do not compare a fragment's fieldMap to itself.


  if (fieldMap === fieldMap2) {
    return;
  } // (D) First collect any conflicts between the provided collection of fields
  // and the collection of fields represented by the given fragment.


  collectConflictsBetween(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, areMutuallyExclusive, fieldMap, fieldMap2); // (E) Then collect any conflicts between the provided collection of fields
  // and any fragment names found in the given fragment.

  for (var i = 0; i < fragmentNames2.length; i++) {
    collectConflictsBetweenFieldsAndFragment(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, areMutuallyExclusive, fieldMap, fragmentNames2[i]);
  }
} // Collect all conflicts found between two fragments, including via spreading in
// any nested fragments.


function collectConflictsBetweenFragments(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, areMutuallyExclusive, fragmentName1, fragmentName2) {
  // No need to compare a fragment to itself.
  if (fragmentName1 === fragmentName2) {
    return;
  } // Memoize so two fragments are not compared for conflicts more than once.


  if (comparedFragmentPairs.has(fragmentName1, fragmentName2, areMutuallyExclusive)) {
    return;
  }

  comparedFragmentPairs.add(fragmentName1, fragmentName2, areMutuallyExclusive);
  var fragment1 = context.getFragment(fragmentName1);
  var fragment2 = context.getFragment(fragmentName2);

  if (!fragment1 || !fragment2) {
    return;
  }

  var _getReferencedFieldsA2 = getReferencedFieldsAndFragmentNames(context, cachedFieldsAndFragmentNames, fragment1),
      fieldMap1 = _getReferencedFieldsA2[0],
      fragmentNames1 = _getReferencedFieldsA2[1];

  var _getReferencedFieldsA3 = getReferencedFieldsAndFragmentNames(context, cachedFieldsAndFragmentNames, fragment2),
      fieldMap2 = _getReferencedFieldsA3[0],
      fragmentNames2 = _getReferencedFieldsA3[1]; // (F) First, collect all conflicts between these two collections of fields
  // (not including any nested fragments).


  collectConflictsBetween(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, areMutuallyExclusive, fieldMap1, fieldMap2); // (G) Then collect conflicts between the first fragment and any nested
  // fragments spread in the second fragment.

  for (var j = 0; j < fragmentNames2.length; j++) {
    collectConflictsBetweenFragments(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, areMutuallyExclusive, fragmentName1, fragmentNames2[j]);
  } // (G) Then collect conflicts between the second fragment and any nested
  // fragments spread in the first fragment.


  for (var i = 0; i < fragmentNames1.length; i++) {
    collectConflictsBetweenFragments(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, areMutuallyExclusive, fragmentNames1[i], fragmentName2);
  }
} // Find all conflicts found between two selection sets, including those found
// via spreading in fragments. Called when determining if conflicts exist
// between the sub-fields of two overlapping fields.


function findConflictsBetweenSubSelectionSets(context, cachedFieldsAndFragmentNames, comparedFragmentPairs, areMutuallyExclusive, parentType1, selectionSet1, parentType2, selectionSet2) {
  var conflicts = [];

  var _getFieldsAndFragment2 = getFieldsAndFragmentNames(context, cachedFieldsAndFragmentNames, parentType1, selectionSet1),
      fieldMap1 = _getFieldsAndFragment2[0],
      fragmentNames1 = _getFieldsAndFragment2[1];

  var _getFieldsAndFragment3 = getFieldsAndFragmentNames(context, cachedFieldsAndFragmentNames, parentType2, selectionSet2),
      fieldMap2 = _getFieldsAndFragment3[0],
      fragmentNames2 = _getFieldsAndFragment3[1]; // (H) First, collect all conflicts between these two collections of field.


  collectConflictsBetween(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, areMutuallyExclusive, fieldMap1, fieldMap2); // (I) Then collect conflicts between the first collection of fields and
  // those referenced by each fragment name associated with the second.

  if (fragmentNames2.length !== 0) {
    for (var j = 0; j < fragmentNames2.length; j++) {
      collectConflictsBetweenFieldsAndFragment(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, areMutuallyExclusive, fieldMap1, fragmentNames2[j]);
    }
  } // (I) Then collect conflicts between the second collection of fields and
  // those referenced by each fragment name associated with the first.


  if (fragmentNames1.length !== 0) {
    for (var i = 0; i < fragmentNames1.length; i++) {
      collectConflictsBetweenFieldsAndFragment(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, areMutuallyExclusive, fieldMap2, fragmentNames1[i]);
    }
  } // (J) Also collect conflicts between any fragment names by the first and
  // fragment names by the second. This compares each item in the first set of
  // names to each item in the second set of names.


  for (var _i3 = 0; _i3 < fragmentNames1.length; _i3++) {
    for (var _j = 0; _j < fragmentNames2.length; _j++) {
      collectConflictsBetweenFragments(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, areMutuallyExclusive, fragmentNames1[_i3], fragmentNames2[_j]);
    }
  }

  return conflicts;
} // Collect all Conflicts "within" one collection of fields.


function collectConflictsWithin(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, fieldMap) {
  // A field map is a keyed collection, where each key represents a response
  // name and the value at that key is a list of all fields which provide that
  // response name. For every response name, if there are multiple fields, they
  // must be compared to find a potential conflict.
  for (var _i5 = 0, _objectEntries2 = (0, _objectEntries3.default)(fieldMap); _i5 < _objectEntries2.length; _i5++) {
    var _ref5 = _objectEntries2[_i5];
    var responseName = _ref5[0];
    var fields = _ref5[1];

    // This compares every field in the list to every other field in this list
    // (except to itself). If the list only has one item, nothing needs to
    // be compared.
    if (fields.length > 1) {
      for (var i = 0; i < fields.length; i++) {
        for (var j = i + 1; j < fields.length; j++) {
          var conflict = findConflict(context, cachedFieldsAndFragmentNames, comparedFragmentPairs, false, // within one collection is never mutually exclusive
          responseName, fields[i], fields[j]);

          if (conflict) {
            conflicts.push(conflict);
          }
        }
      }
    }
  }
} // Collect all Conflicts between two collections of fields. This is similar to,
// but different from the `collectConflictsWithin` function above. This check
// assumes that `collectConflictsWithin` has already been called on each
// provided collection of fields. This is true because this validator traverses
// each individual selection set.


function collectConflictsBetween(context, conflicts, cachedFieldsAndFragmentNames, comparedFragmentPairs, parentFieldsAreMutuallyExclusive, fieldMap1, fieldMap2) {
  // A field map is a keyed collection, where each key represents a response
  // name and the value at that key is a list of all fields which provide that
  // response name. For any response name which appears in both provided field
  // maps, each field from the first field map must be compared to every field
  // in the second field map to find potential conflicts.
  for (var _i7 = 0, _Object$keys2 = Object.keys(fieldMap1); _i7 < _Object$keys2.length; _i7++) {
    var responseName = _Object$keys2[_i7];
    var fields2 = fieldMap2[responseName];

    if (fields2) {
      var fields1 = fieldMap1[responseName];

      for (var i = 0; i < fields1.length; i++) {
        for (var j = 0; j < fields2.length; j++) {
          var conflict = findConflict(context, cachedFieldsAndFragmentNames, comparedFragmentPairs, parentFieldsAreMutuallyExclusive, responseName, fields1[i], fields2[j]);

          if (conflict) {
            conflicts.push(conflict);
          }
        }
      }
    }
  }
} // Determines if there is a conflict between two particular fields, including
// comparing their sub-fields.


function findConflict(context, cachedFieldsAndFragmentNames, comparedFragmentPairs, parentFieldsAreMutuallyExclusive, responseName, field1, field2) {
  var parentType1 = field1[0],
      node1 = field1[1],
      def1 = field1[2];
  var parentType2 = field2[0],
      node2 = field2[1],
      def2 = field2[2]; // If it is known that two fields could not possibly apply at the same
  // time, due to the parent types, then it is safe to permit them to diverge
  // in aliased field or arguments used as they will not present any ambiguity
  // by differing.
  // It is known that two parent types could never overlap if they are
  // different Object types. Interface or Union types might overlap - if not
  // in the current state of the schema, then perhaps in some future version,
  // thus may not safely diverge.

  var areMutuallyExclusive = parentFieldsAreMutuallyExclusive || parentType1 !== parentType2 && (0, _definition.isObjectType)(parentType1) && (0, _definition.isObjectType)(parentType2);

  if (!areMutuallyExclusive) {
    var _node1$arguments, _node2$arguments;

    // Two aliases must refer to the same field.
    var name1 = node1.name.value;
    var name2 = node2.name.value;

    if (name1 !== name2) {
      return [[responseName, "\"".concat(name1, "\" and \"").concat(name2, "\" are different fields")], [node1], [node2]];
    } // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')


    var args1 = (_node1$arguments = node1.arguments) !== null && _node1$arguments !== void 0 ? _node1$arguments : []; // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')

    var args2 = (_node2$arguments = node2.arguments) !== null && _node2$arguments !== void 0 ? _node2$arguments : []; // Two field calls must have the same arguments.

    if (!sameArguments(args1, args2)) {
      return [[responseName, 'they have differing arguments'], [node1], [node2]];
    }
  } // The return type for each field.


  var type1 = def1 === null || def1 === void 0 ? void 0 : def1.type;
  var type2 = def2 === null || def2 === void 0 ? void 0 : def2.type;

  if (type1 && type2 && doTypesConflict(type1, type2)) {
    return [[responseName, "they return conflicting types \"".concat((0, _inspect.default)(type1), "\" and \"").concat((0, _inspect.default)(type2), "\"")], [node1], [node2]];
  } // Collect and compare sub-fields. Use the same "visited fragment names" list
  // for both collections so fields in a fragment reference are never
  // compared to themselves.


  var selectionSet1 = node1.selectionSet;
  var selectionSet2 = node2.selectionSet;

  if (selectionSet1 && selectionSet2) {
    var conflicts = findConflictsBetweenSubSelectionSets(context, cachedFieldsAndFragmentNames, comparedFragmentPairs, areMutuallyExclusive, (0, _definition.getNamedType)(type1), selectionSet1, (0, _definition.getNamedType)(type2), selectionSet2);
    return subfieldConflicts(conflicts, responseName, node1, node2);
  }
}

function sameArguments(arguments1, arguments2) {
  if (arguments1.length !== arguments2.length) {
    return false;
  }

  return arguments1.every(function (argument1) {
    var argument2 = (0, _find.default)(arguments2, function (argument) {
      return argument.name.value === argument1.name.value;
    });

    if (!argument2) {
      return false;
    }

    return sameValue(argument1.value, argument2.value);
  });
}

function sameValue(value1, value2) {
  return (0, _printer.print)(value1) === (0, _printer.print)(value2);
} // Two types conflict if both types could not apply to a value simultaneously.
// Composite types are ignored as their individual field types will be compared
// later recursively. However List and Non-Null types must match.


function doTypesConflict(type1, type2) {
  if ((0, _definition.isListType)(type1)) {
    return (0, _definition.isListType)(type2) ? doTypesConflict(type1.ofType, type2.ofType) : true;
  }

  if ((0, _definition.isListType)(type2)) {
    return true;
  }

  if ((0, _definition.isNonNullType)(type1)) {
    return (0, _definition.isNonNullType)(type2) ? doTypesConflict(type1.ofType, type2.ofType) : true;
  }

  if ((0, _definition.isNonNullType)(type2)) {
    return true;
  }

  if ((0, _definition.isLeafType)(type1) || (0, _definition.isLeafType)(type2)) {
    return type1 !== type2;
  }

  return false;
} // Given a selection set, return the collection of fields (a mapping of response
// name to field nodes and definitions) as well as a list of fragment names
// referenced via fragment spreads.


function getFieldsAndFragmentNames(context, cachedFieldsAndFragmentNames, parentType, selectionSet) {
  var cached = cachedFieldsAndFragmentNames.get(selectionSet);

  if (!cached) {
    var nodeAndDefs = Object.create(null);
    var fragmentNames = Object.create(null);

    _collectFieldsAndFragmentNames(context, parentType, selectionSet, nodeAndDefs, fragmentNames);

    cached = [nodeAndDefs, Object.keys(fragmentNames)];
    cachedFieldsAndFragmentNames.set(selectionSet, cached);
  }

  return cached;
} // Given a reference to a fragment, return the represented collection of fields
// as well as a list of nested fragment names referenced via fragment spreads.


function getReferencedFieldsAndFragmentNames(context, cachedFieldsAndFragmentNames, fragment) {
  // Short-circuit building a type from the node if possible.
  var cached = cachedFieldsAndFragmentNames.get(fragment.selectionSet);

  if (cached) {
    return cached;
  }

  var fragmentType = (0, _typeFromAST.typeFromAST)(context.getSchema(), fragment.typeCondition);
  return getFieldsAndFragmentNames(context, cachedFieldsAndFragmentNames, fragmentType, fragment.selectionSet);
}

function _collectFieldsAndFragmentNames(context, parentType, selectionSet, nodeAndDefs, fragmentNames) {
  for (var _i9 = 0, _selectionSet$selecti2 = selectionSet.selections; _i9 < _selectionSet$selecti2.length; _i9++) {
    var selection = _selectionSet$selecti2[_i9];

    switch (selection.kind) {
      case _kinds.Kind.FIELD:
        {
          var fieldName = selection.name.value;
          var fieldDef = void 0;

          if ((0, _definition.isObjectType)(parentType) || (0, _definition.isInterfaceType)(parentType)) {
            fieldDef = parentType.getFields()[fieldName];
          }

          var responseName = selection.alias ? selection.alias.value : fieldName;

          if (!nodeAndDefs[responseName]) {
            nodeAndDefs[responseName] = [];
          }

          nodeAndDefs[responseName].push([parentType, selection, fieldDef]);
          break;
        }

      case _kinds.Kind.FRAGMENT_SPREAD:
        fragmentNames[selection.name.value] = true;
        break;

      case _kinds.Kind.INLINE_FRAGMENT:
        {
          var typeCondition = selection.typeCondition;
          var inlineFragmentType = typeCondition ? (0, _typeFromAST.typeFromAST)(context.getSchema(), typeCondition) : parentType;

          _collectFieldsAndFragmentNames(context, inlineFragmentType, selection.selectionSet, nodeAndDefs, fragmentNames);

          break;
        }
    }
  }
} // Given a series of Conflicts which occurred between two sub-fields, generate
// a single Conflict.


function subfieldConflicts(conflicts, responseName, node1, node2) {
  if (conflicts.length > 0) {
    return [[responseName, conflicts.map(function (_ref6) {
      var reason = _ref6[0];
      return reason;
    })], conflicts.reduce(function (allFields, _ref7) {
      var fields1 = _ref7[1];
      return allFields.concat(fields1);
    }, [node1]), conflicts.reduce(function (allFields, _ref8) {
      var fields2 = _ref8[2];
      return allFields.concat(fields2);
    }, [node2])];
  }
}
/**
 * A way to keep track of pairs of things when the ordering of the pair does
 * not matter. We do this by maintaining a sort of double adjacency sets.
 */


var PairSet = /*#__PURE__*/function () {
  function PairSet() {
    this._data = Object.create(null);
  }

  var _proto = PairSet.prototype;

  _proto.has = function has(a, b, areMutuallyExclusive) {
    var first = this._data[a];
    var result = first && first[b];

    if (result === undefined) {
      return false;
    } // areMutuallyExclusive being false is a superset of being true,
    // hence if we want to know if this PairSet "has" these two with no
    // exclusivity, we have to ensure it was added as such.


    if (areMutuallyExclusive === false) {
      return result === false;
    }

    return true;
  };

  _proto.add = function add(a, b, areMutuallyExclusive) {
    this._pairSetAdd(a, b, areMutuallyExclusive);

    this._pairSetAdd(b, a, areMutuallyExclusive);
  };

  _proto._pairSetAdd = function _pairSetAdd(a, b, areMutuallyExclusive) {
    var map = this._data[a];

    if (!map) {
      map = Object.create(null);
      this._data[a] = map;
    }

    map[b] = areMutuallyExclusive;
  };

  return PairSet;
}();

},{"../../error/GraphQLError.js":18,"../../jsutils/inspect.js":33,"../../language/kinds.js":55,"../../language/printer.js":61,"../../polyfills/find.js":66,"../../polyfills/objectEntries.js":69,"../../type/definition.js":75,"../../utilities/typeFromAST.js":102}],121:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.PossibleFragmentSpreadsRule = PossibleFragmentSpreadsRule;

var _inspect = _interopRequireDefault(require("../../jsutils/inspect.js"));

var _GraphQLError = require("../../error/GraphQLError.js");

var _definition = require("../../type/definition.js");

var _typeFromAST = require("../../utilities/typeFromAST.js");

var _typeComparators = require("../../utilities/typeComparators.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Possible fragment spread
 *
 * A fragment spread is only valid if the type condition could ever possibly
 * be true: if there is a non-empty intersection of the possible parent types,
 * and possible types which pass the type condition.
 */
function PossibleFragmentSpreadsRule(context) {
  return {
    InlineFragment: function InlineFragment(node) {
      var fragType = context.getType();
      var parentType = context.getParentType();

      if ((0, _definition.isCompositeType)(fragType) && (0, _definition.isCompositeType)(parentType) && !(0, _typeComparators.doTypesOverlap)(context.getSchema(), fragType, parentType)) {
        var parentTypeStr = (0, _inspect.default)(parentType);
        var fragTypeStr = (0, _inspect.default)(fragType);
        context.reportError(new _GraphQLError.GraphQLError("Fragment cannot be spread here as objects of type \"".concat(parentTypeStr, "\" can never be of type \"").concat(fragTypeStr, "\"."), node));
      }
    },
    FragmentSpread: function FragmentSpread(node) {
      var fragName = node.name.value;
      var fragType = getFragmentType(context, fragName);
      var parentType = context.getParentType();

      if (fragType && parentType && !(0, _typeComparators.doTypesOverlap)(context.getSchema(), fragType, parentType)) {
        var parentTypeStr = (0, _inspect.default)(parentType);
        var fragTypeStr = (0, _inspect.default)(fragType);
        context.reportError(new _GraphQLError.GraphQLError("Fragment \"".concat(fragName, "\" cannot be spread here as objects of type \"").concat(parentTypeStr, "\" can never be of type \"").concat(fragTypeStr, "\"."), node));
      }
    }
  };
}

function getFragmentType(context, name) {
  var frag = context.getFragment(name);

  if (frag) {
    var type = (0, _typeFromAST.typeFromAST)(context.getSchema(), frag.typeCondition);

    if ((0, _definition.isCompositeType)(type)) {
      return type;
    }
  }
}

},{"../../error/GraphQLError.js":18,"../../jsutils/inspect.js":33,"../../type/definition.js":75,"../../utilities/typeComparators.js":101,"../../utilities/typeFromAST.js":102}],122:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.PossibleTypeExtensionsRule = PossibleTypeExtensionsRule;

var _inspect = _interopRequireDefault(require("../../jsutils/inspect.js"));

var _invariant = _interopRequireDefault(require("../../jsutils/invariant.js"));

var _didYouMean = _interopRequireDefault(require("../../jsutils/didYouMean.js"));

var _suggestionList = _interopRequireDefault(require("../../jsutils/suggestionList.js"));

var _GraphQLError = require("../../error/GraphQLError.js");

var _kinds = require("../../language/kinds.js");

var _predicates = require("../../language/predicates.js");

var _definition = require("../../type/definition.js");

var _defKindToExtKind;

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

/**
 * Possible type extension
 *
 * A type extension is only valid if the type is defined and has the same kind.
 */
function PossibleTypeExtensionsRule(context) {
  var schema = context.getSchema();
  var definedTypes = Object.create(null);

  for (var _i2 = 0, _context$getDocument$2 = context.getDocument().definitions; _i2 < _context$getDocument$2.length; _i2++) {
    var def = _context$getDocument$2[_i2];

    if ((0, _predicates.isTypeDefinitionNode)(def)) {
      definedTypes[def.name.value] = def;
    }
  }

  return {
    ScalarTypeExtension: checkExtension,
    ObjectTypeExtension: checkExtension,
    InterfaceTypeExtension: checkExtension,
    UnionTypeExtension: checkExtension,
    EnumTypeExtension: checkExtension,
    InputObjectTypeExtension: checkExtension
  };

  function checkExtension(node) {
    var typeName = node.name.value;
    var defNode = definedTypes[typeName];
    var existingType = schema === null || schema === void 0 ? void 0 : schema.getType(typeName);
    var expectedKind;

    if (defNode) {
      expectedKind = defKindToExtKind[defNode.kind];
    } else if (existingType) {
      expectedKind = typeToExtKind(existingType);
    }

    if (expectedKind) {
      if (expectedKind !== node.kind) {
        var kindStr = extensionKindToTypeName(node.kind);
        context.reportError(new _GraphQLError.GraphQLError("Cannot extend non-".concat(kindStr, " type \"").concat(typeName, "\"."), defNode ? [defNode, node] : node));
      }
    } else {
      var allTypeNames = Object.keys(definedTypes);

      if (schema) {
        allTypeNames = allTypeNames.concat(Object.keys(schema.getTypeMap()));
      }

      var suggestedTypes = (0, _suggestionList.default)(typeName, allTypeNames);
      context.reportError(new _GraphQLError.GraphQLError("Cannot extend type \"".concat(typeName, "\" because it is not defined.") + (0, _didYouMean.default)(suggestedTypes), node.name));
    }
  }
}

var defKindToExtKind = (_defKindToExtKind = {}, _defineProperty(_defKindToExtKind, _kinds.Kind.SCALAR_TYPE_DEFINITION, _kinds.Kind.SCALAR_TYPE_EXTENSION), _defineProperty(_defKindToExtKind, _kinds.Kind.OBJECT_TYPE_DEFINITION, _kinds.Kind.OBJECT_TYPE_EXTENSION), _defineProperty(_defKindToExtKind, _kinds.Kind.INTERFACE_TYPE_DEFINITION, _kinds.Kind.INTERFACE_TYPE_EXTENSION), _defineProperty(_defKindToExtKind, _kinds.Kind.UNION_TYPE_DEFINITION, _kinds.Kind.UNION_TYPE_EXTENSION), _defineProperty(_defKindToExtKind, _kinds.Kind.ENUM_TYPE_DEFINITION, _kinds.Kind.ENUM_TYPE_EXTENSION), _defineProperty(_defKindToExtKind, _kinds.Kind.INPUT_OBJECT_TYPE_DEFINITION, _kinds.Kind.INPUT_OBJECT_TYPE_EXTENSION), _defKindToExtKind);

function typeToExtKind(type) {
  if ((0, _definition.isScalarType)(type)) {
    return _kinds.Kind.SCALAR_TYPE_EXTENSION;
  }

  if ((0, _definition.isObjectType)(type)) {
    return _kinds.Kind.OBJECT_TYPE_EXTENSION;
  }

  if ((0, _definition.isInterfaceType)(type)) {
    return _kinds.Kind.INTERFACE_TYPE_EXTENSION;
  }

  if ((0, _definition.isUnionType)(type)) {
    return _kinds.Kind.UNION_TYPE_EXTENSION;
  }

  if ((0, _definition.isEnumType)(type)) {
    return _kinds.Kind.ENUM_TYPE_EXTENSION;
  } // istanbul ignore else (See: 'https://github.com/graphql/graphql-js/issues/2618')


  if ((0, _definition.isInputObjectType)(type)) {
    return _kinds.Kind.INPUT_OBJECT_TYPE_EXTENSION;
  } // istanbul ignore next (Not reachable. All possible types have been considered)


  false || (0, _invariant.default)(0, 'Unexpected type: ' + (0, _inspect.default)(type));
}

function extensionKindToTypeName(kind) {
  switch (kind) {
    case _kinds.Kind.SCALAR_TYPE_EXTENSION:
      return 'scalar';

    case _kinds.Kind.OBJECT_TYPE_EXTENSION:
      return 'object';

    case _kinds.Kind.INTERFACE_TYPE_EXTENSION:
      return 'interface';

    case _kinds.Kind.UNION_TYPE_EXTENSION:
      return 'union';

    case _kinds.Kind.ENUM_TYPE_EXTENSION:
      return 'enum';

    case _kinds.Kind.INPUT_OBJECT_TYPE_EXTENSION:
      return 'input object';
  } // istanbul ignore next (Not reachable. All possible types have been considered)


  false || (0, _invariant.default)(0, 'Unexpected kind: ' + (0, _inspect.default)(kind));
}

},{"../../error/GraphQLError.js":18,"../../jsutils/didYouMean.js":31,"../../jsutils/inspect.js":33,"../../jsutils/invariant.js":35,"../../jsutils/suggestionList.js":49,"../../language/kinds.js":55,"../../language/predicates.js":59,"../../type/definition.js":75}],123:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ProvidedRequiredArgumentsRule = ProvidedRequiredArgumentsRule;
exports.ProvidedRequiredArgumentsOnDirectivesRule = ProvidedRequiredArgumentsOnDirectivesRule;

var _inspect = _interopRequireDefault(require("../../jsutils/inspect.js"));

var _keyMap = _interopRequireDefault(require("../../jsutils/keyMap.js"));

var _GraphQLError = require("../../error/GraphQLError.js");

var _kinds = require("../../language/kinds.js");

var _printer = require("../../language/printer.js");

var _directives = require("../../type/directives.js");

var _definition = require("../../type/definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function ownKeys(object, enumerableOnly) { var keys = Object.keys(object); if (Object.getOwnPropertySymbols) { var symbols = Object.getOwnPropertySymbols(object); if (enumerableOnly) symbols = symbols.filter(function (sym) { return Object.getOwnPropertyDescriptor(object, sym).enumerable; }); keys.push.apply(keys, symbols); } return keys; }

function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; if (i % 2) { ownKeys(Object(source), true).forEach(function (key) { _defineProperty(target, key, source[key]); }); } else if (Object.getOwnPropertyDescriptors) { Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)); } else { ownKeys(Object(source)).forEach(function (key) { Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key)); }); } } return target; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

/**
 * Provided required arguments
 *
 * A field or directive is only valid if all required (non-null without a
 * default value) field arguments have been provided.
 */
function ProvidedRequiredArgumentsRule(context) {
  return _objectSpread(_objectSpread({}, ProvidedRequiredArgumentsOnDirectivesRule(context)), {}, {
    Field: {
      // Validate on leave to allow for deeper errors to appear first.
      leave: function leave(fieldNode) {
        var _fieldNode$arguments;

        var fieldDef = context.getFieldDef();

        if (!fieldDef) {
          return false;
        } // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')


        var argNodes = (_fieldNode$arguments = fieldNode.arguments) !== null && _fieldNode$arguments !== void 0 ? _fieldNode$arguments : [];
        var argNodeMap = (0, _keyMap.default)(argNodes, function (arg) {
          return arg.name.value;
        });

        for (var _i2 = 0, _fieldDef$args2 = fieldDef.args; _i2 < _fieldDef$args2.length; _i2++) {
          var argDef = _fieldDef$args2[_i2];
          var argNode = argNodeMap[argDef.name];

          if (!argNode && (0, _definition.isRequiredArgument)(argDef)) {
            var argTypeStr = (0, _inspect.default)(argDef.type);
            context.reportError(new _GraphQLError.GraphQLError("Field \"".concat(fieldDef.name, "\" argument \"").concat(argDef.name, "\" of type \"").concat(argTypeStr, "\" is required, but it was not provided."), fieldNode));
          }
        }
      }
    }
  });
}
/**
 * @internal
 */


function ProvidedRequiredArgumentsOnDirectivesRule(context) {
  var requiredArgsMap = Object.create(null);
  var schema = context.getSchema();
  var definedDirectives = schema ? schema.getDirectives() : _directives.specifiedDirectives;

  for (var _i4 = 0; _i4 < definedDirectives.length; _i4++) {
    var directive = definedDirectives[_i4];
    requiredArgsMap[directive.name] = (0, _keyMap.default)(directive.args.filter(_definition.isRequiredArgument), function (arg) {
      return arg.name;
    });
  }

  var astDefinitions = context.getDocument().definitions;

  for (var _i6 = 0; _i6 < astDefinitions.length; _i6++) {
    var def = astDefinitions[_i6];

    if (def.kind === _kinds.Kind.DIRECTIVE_DEFINITION) {
      var _def$arguments;

      // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')
      var argNodes = (_def$arguments = def.arguments) !== null && _def$arguments !== void 0 ? _def$arguments : [];
      requiredArgsMap[def.name.value] = (0, _keyMap.default)(argNodes.filter(isRequiredArgumentNode), function (arg) {
        return arg.name.value;
      });
    }
  }

  return {
    Directive: {
      // Validate on leave to allow for deeper errors to appear first.
      leave: function leave(directiveNode) {
        var directiveName = directiveNode.name.value;
        var requiredArgs = requiredArgsMap[directiveName];

        if (requiredArgs) {
          var _directiveNode$argume;

          // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')
          var _argNodes = (_directiveNode$argume = directiveNode.arguments) !== null && _directiveNode$argume !== void 0 ? _directiveNode$argume : [];

          var argNodeMap = (0, _keyMap.default)(_argNodes, function (arg) {
            return arg.name.value;
          });

          for (var _i8 = 0, _Object$keys2 = Object.keys(requiredArgs); _i8 < _Object$keys2.length; _i8++) {
            var argName = _Object$keys2[_i8];

            if (!argNodeMap[argName]) {
              var argType = requiredArgs[argName].type;
              var argTypeStr = (0, _definition.isType)(argType) ? (0, _inspect.default)(argType) : (0, _printer.print)(argType);
              context.reportError(new _GraphQLError.GraphQLError("Directive \"@".concat(directiveName, "\" argument \"").concat(argName, "\" of type \"").concat(argTypeStr, "\" is required, but it was not provided."), directiveNode));
            }
          }
        }
      }
    }
  };
}

function isRequiredArgumentNode(arg) {
  return arg.type.kind === _kinds.Kind.NON_NULL_TYPE && arg.defaultValue == null;
}

},{"../../error/GraphQLError.js":18,"../../jsutils/inspect.js":33,"../../jsutils/keyMap.js":39,"../../language/kinds.js":55,"../../language/printer.js":61,"../../type/definition.js":75,"../../type/directives.js":76}],124:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ScalarLeafsRule = ScalarLeafsRule;

var _inspect = _interopRequireDefault(require("../../jsutils/inspect.js"));

var _GraphQLError = require("../../error/GraphQLError.js");

var _definition = require("../../type/definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Scalar leafs
 *
 * A GraphQL document is valid only if all leaf fields (fields without
 * sub selections) are of scalar or enum types.
 */
function ScalarLeafsRule(context) {
  return {
    Field: function Field(node) {
      var type = context.getType();
      var selectionSet = node.selectionSet;

      if (type) {
        if ((0, _definition.isLeafType)((0, _definition.getNamedType)(type))) {
          if (selectionSet) {
            var fieldName = node.name.value;
            var typeStr = (0, _inspect.default)(type);
            context.reportError(new _GraphQLError.GraphQLError("Field \"".concat(fieldName, "\" must not have a selection since type \"").concat(typeStr, "\" has no subfields."), selectionSet));
          }
        } else if (!selectionSet) {
          var _fieldName = node.name.value;

          var _typeStr = (0, _inspect.default)(type);

          context.reportError(new _GraphQLError.GraphQLError("Field \"".concat(_fieldName, "\" of type \"").concat(_typeStr, "\" must have a selection of subfields. Did you mean \"").concat(_fieldName, " { ... }\"?"), node));
        }
      }
    }
  };
}

},{"../../error/GraphQLError.js":18,"../../jsutils/inspect.js":33,"../../type/definition.js":75}],125:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.SingleFieldSubscriptionsRule = SingleFieldSubscriptionsRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * Subscriptions must only include one field.
 *
 * A GraphQL subscription is valid only if it contains a single root field.
 */
function SingleFieldSubscriptionsRule(context) {
  return {
    OperationDefinition: function OperationDefinition(node) {
      if (node.operation === 'subscription') {
        if (node.selectionSet.selections.length !== 1) {
          context.reportError(new _GraphQLError.GraphQLError(node.name ? "Subscription \"".concat(node.name.value, "\" must select only one top level field.") : 'Anonymous Subscription must select only one top level field.', node.selectionSet.selections.slice(1)));
        }
      }
    }
  };
}

},{"../../error/GraphQLError.js":18}],126:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.UniqueArgumentNamesRule = UniqueArgumentNamesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * Unique argument names
 *
 * A GraphQL field or directive is only valid if all supplied arguments are
 * uniquely named.
 */
function UniqueArgumentNamesRule(context) {
  var knownArgNames = Object.create(null);
  return {
    Field: function Field() {
      knownArgNames = Object.create(null);
    },
    Directive: function Directive() {
      knownArgNames = Object.create(null);
    },
    Argument: function Argument(node) {
      var argName = node.name.value;

      if (knownArgNames[argName]) {
        context.reportError(new _GraphQLError.GraphQLError("There can be only one argument named \"".concat(argName, "\"."), [knownArgNames[argName], node.name]));
      } else {
        knownArgNames[argName] = node.name;
      }

      return false;
    }
  };
}

},{"../../error/GraphQLError.js":18}],127:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.UniqueDirectiveNamesRule = UniqueDirectiveNamesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * Unique directive names
 *
 * A GraphQL document is only valid if all defined directives have unique names.
 */
function UniqueDirectiveNamesRule(context) {
  var knownDirectiveNames = Object.create(null);
  var schema = context.getSchema();
  return {
    DirectiveDefinition: function DirectiveDefinition(node) {
      var directiveName = node.name.value;

      if (schema !== null && schema !== void 0 && schema.getDirective(directiveName)) {
        context.reportError(new _GraphQLError.GraphQLError("Directive \"@".concat(directiveName, "\" already exists in the schema. It cannot be redefined."), node.name));
        return;
      }

      if (knownDirectiveNames[directiveName]) {
        context.reportError(new _GraphQLError.GraphQLError("There can be only one directive named \"@".concat(directiveName, "\"."), [knownDirectiveNames[directiveName], node.name]));
      } else {
        knownDirectiveNames[directiveName] = node.name;
      }

      return false;
    }
  };
}

},{"../../error/GraphQLError.js":18}],128:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.UniqueDirectivesPerLocationRule = UniqueDirectivesPerLocationRule;

var _GraphQLError = require("../../error/GraphQLError.js");

var _kinds = require("../../language/kinds.js");

var _predicates = require("../../language/predicates.js");

var _directives = require("../../type/directives.js");

/**
 * Unique directive names per location
 *
 * A GraphQL document is only valid if all non-repeatable directives at
 * a given location are uniquely named.
 */
function UniqueDirectivesPerLocationRule(context) {
  var uniqueDirectiveMap = Object.create(null);
  var schema = context.getSchema();
  var definedDirectives = schema ? schema.getDirectives() : _directives.specifiedDirectives;

  for (var _i2 = 0; _i2 < definedDirectives.length; _i2++) {
    var directive = definedDirectives[_i2];
    uniqueDirectiveMap[directive.name] = !directive.isRepeatable;
  }

  var astDefinitions = context.getDocument().definitions;

  for (var _i4 = 0; _i4 < astDefinitions.length; _i4++) {
    var def = astDefinitions[_i4];

    if (def.kind === _kinds.Kind.DIRECTIVE_DEFINITION) {
      uniqueDirectiveMap[def.name.value] = !def.repeatable;
    }
  }

  var schemaDirectives = Object.create(null);
  var typeDirectivesMap = Object.create(null);
  return {
    // Many different AST nodes may contain directives. Rather than listing
    // them all, just listen for entering any node, and check to see if it
    // defines any directives.
    enter: function enter(node) {
      if (node.directives == null) {
        return;
      }

      var seenDirectives;

      if (node.kind === _kinds.Kind.SCHEMA_DEFINITION || node.kind === _kinds.Kind.SCHEMA_EXTENSION) {
        seenDirectives = schemaDirectives;
      } else if ((0, _predicates.isTypeDefinitionNode)(node) || (0, _predicates.isTypeExtensionNode)(node)) {
        var typeName = node.name.value;
        seenDirectives = typeDirectivesMap[typeName];

        if (seenDirectives === undefined) {
          typeDirectivesMap[typeName] = seenDirectives = Object.create(null);
        }
      } else {
        seenDirectives = Object.create(null);
      }

      for (var _i6 = 0, _node$directives2 = node.directives; _i6 < _node$directives2.length; _i6++) {
        var _directive = _node$directives2[_i6];
        var directiveName = _directive.name.value;

        if (uniqueDirectiveMap[directiveName]) {
          if (seenDirectives[directiveName]) {
            context.reportError(new _GraphQLError.GraphQLError("The directive \"@".concat(directiveName, "\" can only be used once at this location."), [seenDirectives[directiveName], _directive]));
          } else {
            seenDirectives[directiveName] = _directive;
          }
        }
      }
    }
  };
}

},{"../../error/GraphQLError.js":18,"../../language/kinds.js":55,"../../language/predicates.js":59,"../../type/directives.js":76}],129:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.UniqueEnumValueNamesRule = UniqueEnumValueNamesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

var _definition = require("../../type/definition.js");

/**
 * Unique enum value names
 *
 * A GraphQL enum type is only valid if all its values are uniquely named.
 */
function UniqueEnumValueNamesRule(context) {
  var schema = context.getSchema();
  var existingTypeMap = schema ? schema.getTypeMap() : Object.create(null);
  var knownValueNames = Object.create(null);
  return {
    EnumTypeDefinition: checkValueUniqueness,
    EnumTypeExtension: checkValueUniqueness
  };

  function checkValueUniqueness(node) {
    var _node$values;

    var typeName = node.name.value;

    if (!knownValueNames[typeName]) {
      knownValueNames[typeName] = Object.create(null);
    } // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')


    var valueNodes = (_node$values = node.values) !== null && _node$values !== void 0 ? _node$values : [];
    var valueNames = knownValueNames[typeName];

    for (var _i2 = 0; _i2 < valueNodes.length; _i2++) {
      var valueDef = valueNodes[_i2];
      var valueName = valueDef.name.value;
      var existingType = existingTypeMap[typeName];

      if ((0, _definition.isEnumType)(existingType) && existingType.getValue(valueName)) {
        context.reportError(new _GraphQLError.GraphQLError("Enum value \"".concat(typeName, ".").concat(valueName, "\" already exists in the schema. It cannot also be defined in this type extension."), valueDef.name));
      } else if (valueNames[valueName]) {
        context.reportError(new _GraphQLError.GraphQLError("Enum value \"".concat(typeName, ".").concat(valueName, "\" can only be defined once."), [valueNames[valueName], valueDef.name]));
      } else {
        valueNames[valueName] = valueDef.name;
      }
    }

    return false;
  }
}

},{"../../error/GraphQLError.js":18,"../../type/definition.js":75}],130:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.UniqueFieldDefinitionNamesRule = UniqueFieldDefinitionNamesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

var _definition = require("../../type/definition.js");

/**
 * Unique field definition names
 *
 * A GraphQL complex type is only valid if all its fields are uniquely named.
 */
function UniqueFieldDefinitionNamesRule(context) {
  var schema = context.getSchema();
  var existingTypeMap = schema ? schema.getTypeMap() : Object.create(null);
  var knownFieldNames = Object.create(null);
  return {
    InputObjectTypeDefinition: checkFieldUniqueness,
    InputObjectTypeExtension: checkFieldUniqueness,
    InterfaceTypeDefinition: checkFieldUniqueness,
    InterfaceTypeExtension: checkFieldUniqueness,
    ObjectTypeDefinition: checkFieldUniqueness,
    ObjectTypeExtension: checkFieldUniqueness
  };

  function checkFieldUniqueness(node) {
    var _node$fields;

    var typeName = node.name.value;

    if (!knownFieldNames[typeName]) {
      knownFieldNames[typeName] = Object.create(null);
    } // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')


    var fieldNodes = (_node$fields = node.fields) !== null && _node$fields !== void 0 ? _node$fields : [];
    var fieldNames = knownFieldNames[typeName];

    for (var _i2 = 0; _i2 < fieldNodes.length; _i2++) {
      var fieldDef = fieldNodes[_i2];
      var fieldName = fieldDef.name.value;

      if (hasField(existingTypeMap[typeName], fieldName)) {
        context.reportError(new _GraphQLError.GraphQLError("Field \"".concat(typeName, ".").concat(fieldName, "\" already exists in the schema. It cannot also be defined in this type extension."), fieldDef.name));
      } else if (fieldNames[fieldName]) {
        context.reportError(new _GraphQLError.GraphQLError("Field \"".concat(typeName, ".").concat(fieldName, "\" can only be defined once."), [fieldNames[fieldName], fieldDef.name]));
      } else {
        fieldNames[fieldName] = fieldDef.name;
      }
    }

    return false;
  }
}

function hasField(type, fieldName) {
  if ((0, _definition.isObjectType)(type) || (0, _definition.isInterfaceType)(type) || (0, _definition.isInputObjectType)(type)) {
    return type.getFields()[fieldName] != null;
  }

  return false;
}

},{"../../error/GraphQLError.js":18,"../../type/definition.js":75}],131:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.UniqueFragmentNamesRule = UniqueFragmentNamesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * Unique fragment names
 *
 * A GraphQL document is only valid if all defined fragments have unique names.
 */
function UniqueFragmentNamesRule(context) {
  var knownFragmentNames = Object.create(null);
  return {
    OperationDefinition: function OperationDefinition() {
      return false;
    },
    FragmentDefinition: function FragmentDefinition(node) {
      var fragmentName = node.name.value;

      if (knownFragmentNames[fragmentName]) {
        context.reportError(new _GraphQLError.GraphQLError("There can be only one fragment named \"".concat(fragmentName, "\"."), [knownFragmentNames[fragmentName], node.name]));
      } else {
        knownFragmentNames[fragmentName] = node.name;
      }

      return false;
    }
  };
}

},{"../../error/GraphQLError.js":18}],132:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.UniqueInputFieldNamesRule = UniqueInputFieldNamesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * Unique input field names
 *
 * A GraphQL input object value is only valid if all supplied fields are
 * uniquely named.
 */
function UniqueInputFieldNamesRule(context) {
  var knownNameStack = [];
  var knownNames = Object.create(null);
  return {
    ObjectValue: {
      enter: function enter() {
        knownNameStack.push(knownNames);
        knownNames = Object.create(null);
      },
      leave: function leave() {
        knownNames = knownNameStack.pop();
      }
    },
    ObjectField: function ObjectField(node) {
      var fieldName = node.name.value;

      if (knownNames[fieldName]) {
        context.reportError(new _GraphQLError.GraphQLError("There can be only one input field named \"".concat(fieldName, "\"."), [knownNames[fieldName], node.name]));
      } else {
        knownNames[fieldName] = node.name;
      }
    }
  };
}

},{"../../error/GraphQLError.js":18}],133:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.UniqueOperationNamesRule = UniqueOperationNamesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * Unique operation names
 *
 * A GraphQL document is only valid if all defined operations have unique names.
 */
function UniqueOperationNamesRule(context) {
  var knownOperationNames = Object.create(null);
  return {
    OperationDefinition: function OperationDefinition(node) {
      var operationName = node.name;

      if (operationName) {
        if (knownOperationNames[operationName.value]) {
          context.reportError(new _GraphQLError.GraphQLError("There can be only one operation named \"".concat(operationName.value, "\"."), [knownOperationNames[operationName.value], operationName]));
        } else {
          knownOperationNames[operationName.value] = operationName;
        }
      }

      return false;
    },
    FragmentDefinition: function FragmentDefinition() {
      return false;
    }
  };
}

},{"../../error/GraphQLError.js":18}],134:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.UniqueOperationTypesRule = UniqueOperationTypesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * Unique operation types
 *
 * A GraphQL document is only valid if it has only one type per operation.
 */
function UniqueOperationTypesRule(context) {
  var schema = context.getSchema();
  var definedOperationTypes = Object.create(null);
  var existingOperationTypes = schema ? {
    query: schema.getQueryType(),
    mutation: schema.getMutationType(),
    subscription: schema.getSubscriptionType()
  } : {};
  return {
    SchemaDefinition: checkOperationTypes,
    SchemaExtension: checkOperationTypes
  };

  function checkOperationTypes(node) {
    var _node$operationTypes;

    // istanbul ignore next (See: 'https://github.com/graphql/graphql-js/issues/2203')
    var operationTypesNodes = (_node$operationTypes = node.operationTypes) !== null && _node$operationTypes !== void 0 ? _node$operationTypes : [];

    for (var _i2 = 0; _i2 < operationTypesNodes.length; _i2++) {
      var operationType = operationTypesNodes[_i2];
      var operation = operationType.operation;
      var alreadyDefinedOperationType = definedOperationTypes[operation];

      if (existingOperationTypes[operation]) {
        context.reportError(new _GraphQLError.GraphQLError("Type for ".concat(operation, " already defined in the schema. It cannot be redefined."), operationType));
      } else if (alreadyDefinedOperationType) {
        context.reportError(new _GraphQLError.GraphQLError("There can be only one ".concat(operation, " type in schema."), [alreadyDefinedOperationType, operationType]));
      } else {
        definedOperationTypes[operation] = operationType;
      }
    }

    return false;
  }
}

},{"../../error/GraphQLError.js":18}],135:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.UniqueTypeNamesRule = UniqueTypeNamesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * Unique type names
 *
 * A GraphQL document is only valid if all defined types have unique names.
 */
function UniqueTypeNamesRule(context) {
  var knownTypeNames = Object.create(null);
  var schema = context.getSchema();
  return {
    ScalarTypeDefinition: checkTypeName,
    ObjectTypeDefinition: checkTypeName,
    InterfaceTypeDefinition: checkTypeName,
    UnionTypeDefinition: checkTypeName,
    EnumTypeDefinition: checkTypeName,
    InputObjectTypeDefinition: checkTypeName
  };

  function checkTypeName(node) {
    var typeName = node.name.value;

    if (schema !== null && schema !== void 0 && schema.getType(typeName)) {
      context.reportError(new _GraphQLError.GraphQLError("Type \"".concat(typeName, "\" already exists in the schema. It cannot also be defined in this type definition."), node.name));
      return;
    }

    if (knownTypeNames[typeName]) {
      context.reportError(new _GraphQLError.GraphQLError("There can be only one type named \"".concat(typeName, "\"."), [knownTypeNames[typeName], node.name]));
    } else {
      knownTypeNames[typeName] = node.name;
    }

    return false;
  }
}

},{"../../error/GraphQLError.js":18}],136:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.UniqueVariableNamesRule = UniqueVariableNamesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

/**
 * Unique variable names
 *
 * A GraphQL operation is only valid if all its variables are uniquely named.
 */
function UniqueVariableNamesRule(context) {
  var knownVariableNames = Object.create(null);
  return {
    OperationDefinition: function OperationDefinition() {
      knownVariableNames = Object.create(null);
    },
    VariableDefinition: function VariableDefinition(node) {
      var variableName = node.variable.name.value;

      if (knownVariableNames[variableName]) {
        context.reportError(new _GraphQLError.GraphQLError("There can be only one variable named \"$".concat(variableName, "\"."), [knownVariableNames[variableName], node.variable.name]));
      } else {
        knownVariableNames[variableName] = node.variable.name;
      }
    }
  };
}

},{"../../error/GraphQLError.js":18}],137:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ValuesOfCorrectTypeRule = ValuesOfCorrectTypeRule;

var _objectValues3 = _interopRequireDefault(require("../../polyfills/objectValues.js"));

var _keyMap = _interopRequireDefault(require("../../jsutils/keyMap.js"));

var _inspect = _interopRequireDefault(require("../../jsutils/inspect.js"));

var _didYouMean = _interopRequireDefault(require("../../jsutils/didYouMean.js"));

var _suggestionList = _interopRequireDefault(require("../../jsutils/suggestionList.js"));

var _GraphQLError = require("../../error/GraphQLError.js");

var _printer = require("../../language/printer.js");

var _definition = require("../../type/definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Value literals of correct type
 *
 * A GraphQL document is only valid if all value literals are of the type
 * expected at their position.
 */
function ValuesOfCorrectTypeRule(context) {
  return {
    ListValue: function ListValue(node) {
      // Note: TypeInfo will traverse into a list's item type, so look to the
      // parent input type to check if it is a list.
      var type = (0, _definition.getNullableType)(context.getParentInputType());

      if (!(0, _definition.isListType)(type)) {
        isValidValueNode(context, node);
        return false; // Don't traverse further.
      }
    },
    ObjectValue: function ObjectValue(node) {
      var type = (0, _definition.getNamedType)(context.getInputType());

      if (!(0, _definition.isInputObjectType)(type)) {
        isValidValueNode(context, node);
        return false; // Don't traverse further.
      } // Ensure every required field exists.


      var fieldNodeMap = (0, _keyMap.default)(node.fields, function (field) {
        return field.name.value;
      });

      for (var _i2 = 0, _objectValues2 = (0, _objectValues3.default)(type.getFields()); _i2 < _objectValues2.length; _i2++) {
        var fieldDef = _objectValues2[_i2];
        var fieldNode = fieldNodeMap[fieldDef.name];

        if (!fieldNode && (0, _definition.isRequiredInputField)(fieldDef)) {
          var typeStr = (0, _inspect.default)(fieldDef.type);
          context.reportError(new _GraphQLError.GraphQLError("Field \"".concat(type.name, ".").concat(fieldDef.name, "\" of required type \"").concat(typeStr, "\" was not provided."), node));
        }
      }
    },
    ObjectField: function ObjectField(node) {
      var parentType = (0, _definition.getNamedType)(context.getParentInputType());
      var fieldType = context.getInputType();

      if (!fieldType && (0, _definition.isInputObjectType)(parentType)) {
        var suggestions = (0, _suggestionList.default)(node.name.value, Object.keys(parentType.getFields()));
        context.reportError(new _GraphQLError.GraphQLError("Field \"".concat(node.name.value, "\" is not defined by type \"").concat(parentType.name, "\".") + (0, _didYouMean.default)(suggestions), node));
      }
    },
    NullValue: function NullValue(node) {
      var type = context.getInputType();

      if ((0, _definition.isNonNullType)(type)) {
        context.reportError(new _GraphQLError.GraphQLError("Expected value of type \"".concat((0, _inspect.default)(type), "\", found ").concat((0, _printer.print)(node), "."), node));
      }
    },
    EnumValue: function EnumValue(node) {
      return isValidValueNode(context, node);
    },
    IntValue: function IntValue(node) {
      return isValidValueNode(context, node);
    },
    FloatValue: function FloatValue(node) {
      return isValidValueNode(context, node);
    },
    StringValue: function StringValue(node) {
      return isValidValueNode(context, node);
    },
    BooleanValue: function BooleanValue(node) {
      return isValidValueNode(context, node);
    }
  };
}
/**
 * Any value literal may be a valid representation of a Scalar, depending on
 * that scalar type.
 */


function isValidValueNode(context, node) {
  // Report any error at the full type expected by the location.
  var locationType = context.getInputType();

  if (!locationType) {
    return;
  }

  var type = (0, _definition.getNamedType)(locationType);

  if (!(0, _definition.isLeafType)(type)) {
    var typeStr = (0, _inspect.default)(locationType);
    context.reportError(new _GraphQLError.GraphQLError("Expected value of type \"".concat(typeStr, "\", found ").concat((0, _printer.print)(node), "."), node));
    return;
  } // Scalars and Enums determine if a literal value is valid via parseLiteral(),
  // which may throw or return an invalid value to indicate failure.


  try {
    var parseResult = type.parseLiteral(node, undefined
    /* variables */
    );

    if (parseResult === undefined) {
      var _typeStr = (0, _inspect.default)(locationType);

      context.reportError(new _GraphQLError.GraphQLError("Expected value of type \"".concat(_typeStr, "\", found ").concat((0, _printer.print)(node), "."), node));
    }
  } catch (error) {
    var _typeStr2 = (0, _inspect.default)(locationType);

    if (error instanceof _GraphQLError.GraphQLError) {
      context.reportError(error);
    } else {
      context.reportError(new _GraphQLError.GraphQLError("Expected value of type \"".concat(_typeStr2, "\", found ").concat((0, _printer.print)(node), "; ") + error.message, node, undefined, undefined, undefined, error));
    }
  }
}

},{"../../error/GraphQLError.js":18,"../../jsutils/didYouMean.js":31,"../../jsutils/inspect.js":33,"../../jsutils/keyMap.js":39,"../../jsutils/suggestionList.js":49,"../../language/printer.js":61,"../../polyfills/objectValues.js":70,"../../type/definition.js":75}],138:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.VariablesAreInputTypesRule = VariablesAreInputTypesRule;

var _GraphQLError = require("../../error/GraphQLError.js");

var _printer = require("../../language/printer.js");

var _definition = require("../../type/definition.js");

var _typeFromAST = require("../../utilities/typeFromAST.js");

/**
 * Variables are input types
 *
 * A GraphQL operation is only valid if all the variables it defines are of
 * input types (scalar, enum, or input object).
 */
function VariablesAreInputTypesRule(context) {
  return {
    VariableDefinition: function VariableDefinition(node) {
      var type = (0, _typeFromAST.typeFromAST)(context.getSchema(), node.type);

      if (type && !(0, _definition.isInputType)(type)) {
        var variableName = node.variable.name.value;
        var typeName = (0, _printer.print)(node.type);
        context.reportError(new _GraphQLError.GraphQLError("Variable \"$".concat(variableName, "\" cannot be non-input type \"").concat(typeName, "\"."), node.type));
      }
    }
  };
}

},{"../../error/GraphQLError.js":18,"../../language/printer.js":61,"../../type/definition.js":75,"../../utilities/typeFromAST.js":102}],139:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.VariablesInAllowedPositionRule = VariablesInAllowedPositionRule;

var _inspect = _interopRequireDefault(require("../../jsutils/inspect.js"));

var _GraphQLError = require("../../error/GraphQLError.js");

var _kinds = require("../../language/kinds.js");

var _definition = require("../../type/definition.js");

var _typeFromAST = require("../../utilities/typeFromAST.js");

var _typeComparators = require("../../utilities/typeComparators.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Variables passed to field arguments conform to type
 */
function VariablesInAllowedPositionRule(context) {
  var varDefMap = Object.create(null);
  return {
    OperationDefinition: {
      enter: function enter() {
        varDefMap = Object.create(null);
      },
      leave: function leave(operation) {
        var usages = context.getRecursiveVariableUsages(operation);

        for (var _i2 = 0; _i2 < usages.length; _i2++) {
          var _ref2 = usages[_i2];
          var node = _ref2.node;
          var type = _ref2.type;
          var defaultValue = _ref2.defaultValue;
          var varName = node.name.value;
          var varDef = varDefMap[varName];

          if (varDef && type) {
            // A var type is allowed if it is the same or more strict (e.g. is
            // a subtype of) than the expected type. It can be more strict if
            // the variable type is non-null when the expected type is nullable.
            // If both are list types, the variable item type can be more strict
            // than the expected item type (contravariant).
            var schema = context.getSchema();
            var varType = (0, _typeFromAST.typeFromAST)(schema, varDef.type);

            if (varType && !allowedVariableUsage(schema, varType, varDef.defaultValue, type, defaultValue)) {
              var varTypeStr = (0, _inspect.default)(varType);
              var typeStr = (0, _inspect.default)(type);
              context.reportError(new _GraphQLError.GraphQLError("Variable \"$".concat(varName, "\" of type \"").concat(varTypeStr, "\" used in position expecting type \"").concat(typeStr, "\"."), [varDef, node]));
            }
          }
        }
      }
    },
    VariableDefinition: function VariableDefinition(node) {
      varDefMap[node.variable.name.value] = node;
    }
  };
}
/**
 * Returns true if the variable is allowed in the location it was found,
 * which includes considering if default values exist for either the variable
 * or the location at which it is located.
 */


function allowedVariableUsage(schema, varType, varDefaultValue, locationType, locationDefaultValue) {
  if ((0, _definition.isNonNullType)(locationType) && !(0, _definition.isNonNullType)(varType)) {
    var hasNonNullVariableDefaultValue = varDefaultValue != null && varDefaultValue.kind !== _kinds.Kind.NULL;
    var hasLocationDefaultValue = locationDefaultValue !== undefined;

    if (!hasNonNullVariableDefaultValue && !hasLocationDefaultValue) {
      return false;
    }

    var nullableLocationType = locationType.ofType;
    return (0, _typeComparators.isTypeSubTypeOf)(schema, varType, nullableLocationType);
  }

  return (0, _typeComparators.isTypeSubTypeOf)(schema, varType, locationType);
}

},{"../../error/GraphQLError.js":18,"../../jsutils/inspect.js":33,"../../language/kinds.js":55,"../../type/definition.js":75,"../../utilities/typeComparators.js":101,"../../utilities/typeFromAST.js":102}],140:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.NoDeprecatedCustomRule = NoDeprecatedCustomRule;

var _invariant = _interopRequireDefault(require("../../../jsutils/invariant.js"));

var _GraphQLError = require("../../../error/GraphQLError.js");

var _definition = require("../../../type/definition.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * No deprecated
 *
 * A GraphQL document is only valid if all selected fields and all used enum values have not been
 * deprecated.
 *
 * Note: This rule is optional and is not part of the Validation section of the GraphQL
 * Specification. The main purpose of this rule is detection of deprecated usages and not
 * necessarily to forbid their use when querying a service.
 */
function NoDeprecatedCustomRule(context) {
  return {
    Field: function Field(node) {
      var fieldDef = context.getFieldDef();
      var deprecationReason = fieldDef === null || fieldDef === void 0 ? void 0 : fieldDef.deprecationReason;

      if (fieldDef && deprecationReason != null) {
        var parentType = context.getParentType();
        parentType != null || (0, _invariant.default)(0);
        context.reportError(new _GraphQLError.GraphQLError("The field ".concat(parentType.name, ".").concat(fieldDef.name, " is deprecated. ").concat(deprecationReason), node));
      }
    },
    Argument: function Argument(node) {
      var argDef = context.getArgument();
      var deprecationReason = argDef === null || argDef === void 0 ? void 0 : argDef.deprecationReason;

      if (argDef && deprecationReason != null) {
        var directiveDef = context.getDirective();

        if (directiveDef != null) {
          context.reportError(new _GraphQLError.GraphQLError("Directive \"@".concat(directiveDef.name, "\" argument \"").concat(argDef.name, "\" is deprecated. ").concat(deprecationReason), node));
        } else {
          var parentType = context.getParentType();
          var fieldDef = context.getFieldDef();
          parentType != null && fieldDef != null || (0, _invariant.default)(0);
          context.reportError(new _GraphQLError.GraphQLError("Field \"".concat(parentType.name, ".").concat(fieldDef.name, "\" argument \"").concat(argDef.name, "\" is deprecated. ").concat(deprecationReason), node));
        }
      }
    },
    ObjectField: function ObjectField(node) {
      var inputObjectDef = (0, _definition.getNamedType)(context.getParentInputType());

      if ((0, _definition.isInputObjectType)(inputObjectDef)) {
        var inputFieldDef = inputObjectDef.getFields()[node.name.value]; // flowlint-next-line unnecessary-optional-chain:off

        var deprecationReason = inputFieldDef === null || inputFieldDef === void 0 ? void 0 : inputFieldDef.deprecationReason;

        if (deprecationReason != null) {
          context.reportError(new _GraphQLError.GraphQLError("The input field ".concat(inputObjectDef.name, ".").concat(inputFieldDef.name, " is deprecated. ").concat(deprecationReason), node));
        }
      }
    },
    EnumValue: function EnumValue(node) {
      var enumValueDef = context.getEnumValue();
      var deprecationReason = enumValueDef === null || enumValueDef === void 0 ? void 0 : enumValueDef.deprecationReason;

      if (enumValueDef && deprecationReason != null) {
        var enumTypeDef = (0, _definition.getNamedType)(context.getInputType());
        enumTypeDef != null || (0, _invariant.default)(0);
        context.reportError(new _GraphQLError.GraphQLError("The enum value \"".concat(enumTypeDef.name, ".").concat(enumValueDef.name, "\" is deprecated. ").concat(deprecationReason), node));
      }
    }
  };
}

},{"../../../error/GraphQLError.js":18,"../../../jsutils/invariant.js":35,"../../../type/definition.js":75}],141:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.NoSchemaIntrospectionCustomRule = NoSchemaIntrospectionCustomRule;

var _GraphQLError = require("../../../error/GraphQLError.js");

var _definition = require("../../../type/definition.js");

var _introspection = require("../../../type/introspection.js");

/**
 * Prohibit introspection queries
 *
 * A GraphQL document is only valid if all fields selected are not fields that
 * return an introspection type.
 *
 * Note: This rule is optional and is not part of the Validation section of the
 * GraphQL Specification. This rule effectively disables introspection, which
 * does not reflect best practices and should only be done if absolutely necessary.
 */
function NoSchemaIntrospectionCustomRule(context) {
  return {
    Field: function Field(node) {
      var type = (0, _definition.getNamedType)(context.getType());

      if (type && (0, _introspection.isIntrospectionType)(type)) {
        context.reportError(new _GraphQLError.GraphQLError("GraphQL introspection has been disabled, but the requested query contained the field \"".concat(node.name.value, "\"."), node));
      }
    }
  };
}

},{"../../../error/GraphQLError.js":18,"../../../type/definition.js":75,"../../../type/introspection.js":78}],142:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.specifiedSDLRules = exports.specifiedRules = void 0;

var _ExecutableDefinitionsRule = require("./rules/ExecutableDefinitionsRule.js");

var _UniqueOperationNamesRule = require("./rules/UniqueOperationNamesRule.js");

var _LoneAnonymousOperationRule = require("./rules/LoneAnonymousOperationRule.js");

var _SingleFieldSubscriptionsRule = require("./rules/SingleFieldSubscriptionsRule.js");

var _KnownTypeNamesRule = require("./rules/KnownTypeNamesRule.js");

var _FragmentsOnCompositeTypesRule = require("./rules/FragmentsOnCompositeTypesRule.js");

var _VariablesAreInputTypesRule = require("./rules/VariablesAreInputTypesRule.js");

var _ScalarLeafsRule = require("./rules/ScalarLeafsRule.js");

var _FieldsOnCorrectTypeRule = require("./rules/FieldsOnCorrectTypeRule.js");

var _UniqueFragmentNamesRule = require("./rules/UniqueFragmentNamesRule.js");

var _KnownFragmentNamesRule = require("./rules/KnownFragmentNamesRule.js");

var _NoUnusedFragmentsRule = require("./rules/NoUnusedFragmentsRule.js");

var _PossibleFragmentSpreadsRule = require("./rules/PossibleFragmentSpreadsRule.js");

var _NoFragmentCyclesRule = require("./rules/NoFragmentCyclesRule.js");

var _UniqueVariableNamesRule = require("./rules/UniqueVariableNamesRule.js");

var _NoUndefinedVariablesRule = require("./rules/NoUndefinedVariablesRule.js");

var _NoUnusedVariablesRule = require("./rules/NoUnusedVariablesRule.js");

var _KnownDirectivesRule = require("./rules/KnownDirectivesRule.js");

var _UniqueDirectivesPerLocationRule = require("./rules/UniqueDirectivesPerLocationRule.js");

var _KnownArgumentNamesRule = require("./rules/KnownArgumentNamesRule.js");

var _UniqueArgumentNamesRule = require("./rules/UniqueArgumentNamesRule.js");

var _ValuesOfCorrectTypeRule = require("./rules/ValuesOfCorrectTypeRule.js");

var _ProvidedRequiredArgumentsRule = require("./rules/ProvidedRequiredArgumentsRule.js");

var _VariablesInAllowedPositionRule = require("./rules/VariablesInAllowedPositionRule.js");

var _OverlappingFieldsCanBeMergedRule = require("./rules/OverlappingFieldsCanBeMergedRule.js");

var _UniqueInputFieldNamesRule = require("./rules/UniqueInputFieldNamesRule.js");

var _LoneSchemaDefinitionRule = require("./rules/LoneSchemaDefinitionRule.js");

var _UniqueOperationTypesRule = require("./rules/UniqueOperationTypesRule.js");

var _UniqueTypeNamesRule = require("./rules/UniqueTypeNamesRule.js");

var _UniqueEnumValueNamesRule = require("./rules/UniqueEnumValueNamesRule.js");

var _UniqueFieldDefinitionNamesRule = require("./rules/UniqueFieldDefinitionNamesRule.js");

var _UniqueDirectiveNamesRule = require("./rules/UniqueDirectiveNamesRule.js");

var _PossibleTypeExtensionsRule = require("./rules/PossibleTypeExtensionsRule.js");

// Spec Section: "Executable Definitions"
// Spec Section: "Operation Name Uniqueness"
// Spec Section: "Lone Anonymous Operation"
// Spec Section: "Subscriptions with Single Root Field"
// Spec Section: "Fragment Spread Type Existence"
// Spec Section: "Fragments on Composite Types"
// Spec Section: "Variables are Input Types"
// Spec Section: "Leaf Field Selections"
// Spec Section: "Field Selections on Objects, Interfaces, and Unions Types"
// Spec Section: "Fragment Name Uniqueness"
// Spec Section: "Fragment spread target defined"
// Spec Section: "Fragments must be used"
// Spec Section: "Fragment spread is possible"
// Spec Section: "Fragments must not form cycles"
// Spec Section: "Variable Uniqueness"
// Spec Section: "All Variable Used Defined"
// Spec Section: "All Variables Used"
// Spec Section: "Directives Are Defined"
// Spec Section: "Directives Are Unique Per Location"
// Spec Section: "Argument Names"
// Spec Section: "Argument Uniqueness"
// Spec Section: "Value Type Correctness"
// Spec Section: "Argument Optionality"
// Spec Section: "All Variable Usages Are Allowed"
// Spec Section: "Field Selection Merging"
// Spec Section: "Input Object Field Uniqueness"
// SDL-specific validation rules

/**
 * This set includes all validation rules defined by the GraphQL spec.
 *
 * The order of the rules in this list has been adjusted to lead to the
 * most clear output when encountering multiple validation errors.
 */
var specifiedRules = Object.freeze([_ExecutableDefinitionsRule.ExecutableDefinitionsRule, _UniqueOperationNamesRule.UniqueOperationNamesRule, _LoneAnonymousOperationRule.LoneAnonymousOperationRule, _SingleFieldSubscriptionsRule.SingleFieldSubscriptionsRule, _KnownTypeNamesRule.KnownTypeNamesRule, _FragmentsOnCompositeTypesRule.FragmentsOnCompositeTypesRule, _VariablesAreInputTypesRule.VariablesAreInputTypesRule, _ScalarLeafsRule.ScalarLeafsRule, _FieldsOnCorrectTypeRule.FieldsOnCorrectTypeRule, _UniqueFragmentNamesRule.UniqueFragmentNamesRule, _KnownFragmentNamesRule.KnownFragmentNamesRule, _NoUnusedFragmentsRule.NoUnusedFragmentsRule, _PossibleFragmentSpreadsRule.PossibleFragmentSpreadsRule, _NoFragmentCyclesRule.NoFragmentCyclesRule, _UniqueVariableNamesRule.UniqueVariableNamesRule, _NoUndefinedVariablesRule.NoUndefinedVariablesRule, _NoUnusedVariablesRule.NoUnusedVariablesRule, _KnownDirectivesRule.KnownDirectivesRule, _UniqueDirectivesPerLocationRule.UniqueDirectivesPerLocationRule, _KnownArgumentNamesRule.KnownArgumentNamesRule, _UniqueArgumentNamesRule.UniqueArgumentNamesRule, _ValuesOfCorrectTypeRule.ValuesOfCorrectTypeRule, _ProvidedRequiredArgumentsRule.ProvidedRequiredArgumentsRule, _VariablesInAllowedPositionRule.VariablesInAllowedPositionRule, _OverlappingFieldsCanBeMergedRule.OverlappingFieldsCanBeMergedRule, _UniqueInputFieldNamesRule.UniqueInputFieldNamesRule]);
/**
 * @internal
 */

exports.specifiedRules = specifiedRules;
var specifiedSDLRules = Object.freeze([_LoneSchemaDefinitionRule.LoneSchemaDefinitionRule, _UniqueOperationTypesRule.UniqueOperationTypesRule, _UniqueTypeNamesRule.UniqueTypeNamesRule, _UniqueEnumValueNamesRule.UniqueEnumValueNamesRule, _UniqueFieldDefinitionNamesRule.UniqueFieldDefinitionNamesRule, _UniqueDirectiveNamesRule.UniqueDirectiveNamesRule, _KnownTypeNamesRule.KnownTypeNamesRule, _KnownDirectivesRule.KnownDirectivesRule, _UniqueDirectivesPerLocationRule.UniqueDirectivesPerLocationRule, _PossibleTypeExtensionsRule.PossibleTypeExtensionsRule, _KnownArgumentNamesRule.KnownArgumentNamesOnDirectivesRule, _UniqueArgumentNamesRule.UniqueArgumentNamesRule, _UniqueInputFieldNamesRule.UniqueInputFieldNamesRule, _ProvidedRequiredArgumentsRule.ProvidedRequiredArgumentsOnDirectivesRule]);
exports.specifiedSDLRules = specifiedSDLRules;

},{"./rules/ExecutableDefinitionsRule.js":107,"./rules/FieldsOnCorrectTypeRule.js":108,"./rules/FragmentsOnCompositeTypesRule.js":109,"./rules/KnownArgumentNamesRule.js":110,"./rules/KnownDirectivesRule.js":111,"./rules/KnownFragmentNamesRule.js":112,"./rules/KnownTypeNamesRule.js":113,"./rules/LoneAnonymousOperationRule.js":114,"./rules/LoneSchemaDefinitionRule.js":115,"./rules/NoFragmentCyclesRule.js":116,"./rules/NoUndefinedVariablesRule.js":117,"./rules/NoUnusedFragmentsRule.js":118,"./rules/NoUnusedVariablesRule.js":119,"./rules/OverlappingFieldsCanBeMergedRule.js":120,"./rules/PossibleFragmentSpreadsRule.js":121,"./rules/PossibleTypeExtensionsRule.js":122,"./rules/ProvidedRequiredArgumentsRule.js":123,"./rules/ScalarLeafsRule.js":124,"./rules/SingleFieldSubscriptionsRule.js":125,"./rules/UniqueArgumentNamesRule.js":126,"./rules/UniqueDirectiveNamesRule.js":127,"./rules/UniqueDirectivesPerLocationRule.js":128,"./rules/UniqueEnumValueNamesRule.js":129,"./rules/UniqueFieldDefinitionNamesRule.js":130,"./rules/UniqueFragmentNamesRule.js":131,"./rules/UniqueInputFieldNamesRule.js":132,"./rules/UniqueOperationNamesRule.js":133,"./rules/UniqueOperationTypesRule.js":134,"./rules/UniqueTypeNamesRule.js":135,"./rules/UniqueVariableNamesRule.js":136,"./rules/ValuesOfCorrectTypeRule.js":137,"./rules/VariablesAreInputTypesRule.js":138,"./rules/VariablesInAllowedPositionRule.js":139}],143:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.validate = validate;
exports.validateSDL = validateSDL;
exports.assertValidSDL = assertValidSDL;
exports.assertValidSDLExtension = assertValidSDLExtension;

var _devAssert = _interopRequireDefault(require("../jsutils/devAssert.js"));

var _GraphQLError = require("../error/GraphQLError.js");

var _visitor = require("../language/visitor.js");

var _validate = require("../type/validate.js");

var _TypeInfo = require("../utilities/TypeInfo.js");

var _specifiedRules = require("./specifiedRules.js");

var _ValidationContext = require("./ValidationContext.js");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Implements the "Validation" section of the spec.
 *
 * Validation runs synchronously, returning an array of encountered errors, or
 * an empty array if no errors were encountered and the document is valid.
 *
 * A list of specific validation rules may be provided. If not provided, the
 * default list of rules defined by the GraphQL specification will be used.
 *
 * Each validation rules is a function which returns a visitor
 * (see the language/visitor API). Visitor methods are expected to return
 * GraphQLErrors, or Arrays of GraphQLErrors when invalid.
 *
 * Optionally a custom TypeInfo instance may be provided. If not provided, one
 * will be created from the provided schema.
 */
function validate(schema, documentAST) {
  var rules = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : _specifiedRules.specifiedRules;
  var typeInfo = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : new _TypeInfo.TypeInfo(schema);
  var options = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : {
    maxErrors: undefined
  };
  documentAST || (0, _devAssert.default)(0, 'Must provide document.'); // If the schema used for validation is invalid, throw an error.

  (0, _validate.assertValidSchema)(schema);
  var abortObj = Object.freeze({});
  var errors = [];
  var context = new _ValidationContext.ValidationContext(schema, documentAST, typeInfo, function (error) {
    if (options.maxErrors != null && errors.length >= options.maxErrors) {
      errors.push(new _GraphQLError.GraphQLError('Too many validation errors, error limit reached. Validation aborted.'));
      throw abortObj;
    }

    errors.push(error);
  }); // This uses a specialized visitor which runs multiple visitors in parallel,
  // while maintaining the visitor skip and break API.

  var visitor = (0, _visitor.visitInParallel)(rules.map(function (rule) {
    return rule(context);
  })); // Visit the whole document with each instance of all provided rules.

  try {
    (0, _visitor.visit)(documentAST, (0, _TypeInfo.visitWithTypeInfo)(typeInfo, visitor));
  } catch (e) {
    if (e !== abortObj) {
      throw e;
    }
  }

  return errors;
}
/**
 * @internal
 */


function validateSDL(documentAST, schemaToExtend) {
  var rules = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : _specifiedRules.specifiedSDLRules;
  var errors = [];
  var context = new _ValidationContext.SDLValidationContext(documentAST, schemaToExtend, function (error) {
    errors.push(error);
  });
  var visitors = rules.map(function (rule) {
    return rule(context);
  });
  (0, _visitor.visit)(documentAST, (0, _visitor.visitInParallel)(visitors));
  return errors;
}
/**
 * Utility function which asserts a SDL document is valid by throwing an error
 * if it is invalid.
 *
 * @internal
 */


function assertValidSDL(documentAST) {
  var errors = validateSDL(documentAST);

  if (errors.length !== 0) {
    throw new Error(errors.map(function (error) {
      return error.message;
    }).join('\n\n'));
  }
}
/**
 * Utility function which asserts a SDL document is valid by throwing an error
 * if it is invalid.
 *
 * @internal
 */


function assertValidSDLExtension(documentAST, schema) {
  var errors = validateSDL(documentAST, schema);

  if (errors.length !== 0) {
    throw new Error(errors.map(function (error) {
      return error.message;
    }).join('\n\n'));
  }
}

},{"../error/GraphQLError.js":18,"../jsutils/devAssert.js":30,"../language/visitor.js":64,"../type/validate.js":81,"../utilities/TypeInfo.js":82,"./ValidationContext.js":105,"./specifiedRules.js":142}],144:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.versionInfo = exports.version = void 0;

/**
 * Note: This file is autogenerated using "resources/gen-version.js" script and
 * automatically updated by "npm version" command.
 */

/**
 * A string containing the version of the GraphQL.js library
 */
var version = '15.5.0';
/**
 * An object containing the components of the GraphQL.js version string
 */

exports.version = version;
var versionInfo = Object.freeze({
  major: 15,
  minor: 5,
  patch: 0,
  preReleaseTag: null
});
exports.versionInfo = versionInfo;

},{}],145:[function(require,module,exports){
'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

var context = require('@wry/context');

function defaultDispose() { }
var Cache = /** @class */ (function () {
    function Cache(max, dispose) {
        if (max === void 0) { max = Infinity; }
        if (dispose === void 0) { dispose = defaultDispose; }
        this.max = max;
        this.dispose = dispose;
        this.map = new Map();
        this.newest = null;
        this.oldest = null;
    }
    Cache.prototype.has = function (key) {
        return this.map.has(key);
    };
    Cache.prototype.get = function (key) {
        var entry = this.getEntry(key);
        return entry && entry.value;
    };
    Cache.prototype.getEntry = function (key) {
        var entry = this.map.get(key);
        if (entry && entry !== this.newest) {
            var older = entry.older, newer = entry.newer;
            if (newer) {
                newer.older = older;
            }
            if (older) {
                older.newer = newer;
            }
            entry.older = this.newest;
            entry.older.newer = entry;
            entry.newer = null;
            this.newest = entry;
            if (entry === this.oldest) {
                this.oldest = newer;
            }
        }
        return entry;
    };
    Cache.prototype.set = function (key, value) {
        var entry = this.getEntry(key);
        if (entry) {
            return entry.value = value;
        }
        entry = {
            key: key,
            value: value,
            newer: null,
            older: this.newest
        };
        if (this.newest) {
            this.newest.newer = entry;
        }
        this.newest = entry;
        this.oldest = this.oldest || entry;
        this.map.set(key, entry);
        return entry.value;
    };
    Cache.prototype.clean = function () {
        while (this.oldest && this.map.size > this.max) {
            this.delete(this.oldest.key);
        }
    };
    Cache.prototype.delete = function (key) {
        var entry = this.map.get(key);
        if (entry) {
            if (entry === this.newest) {
                this.newest = entry.older;
            }
            if (entry === this.oldest) {
                this.oldest = entry.newer;
            }
            if (entry.newer) {
                entry.newer.older = entry.older;
            }
            if (entry.older) {
                entry.older.newer = entry.newer;
            }
            this.map.delete(key);
            this.dispose(entry.value, key);
            return true;
        }
        return false;
    };
    return Cache;
}());

var parentEntrySlot = new context.Slot();

var reusableEmptyArray = [];
var emptySetPool = [];
var POOL_TARGET_SIZE = 100;
// Since this package might be used browsers, we should avoid using the
// Node built-in assert module.
function assert(condition, optionalMessage) {
    if (!condition) {
        throw new Error(optionalMessage || "assertion failure");
    }
}
function valueIs(a, b) {
    var len = a.length;
    return (
    // Unknown values are not equal to each other.
    len > 0 &&
        // Both values must be ordinary (or both exceptional) to be equal.
        len === b.length &&
        // The underlying value or exception must be the same.
        a[len - 1] === b[len - 1]);
}
function valueGet(value) {
    switch (value.length) {
        case 0: throw new Error("unknown value");
        case 1: return value[0];
        case 2: throw value[1];
    }
}
function valueCopy(value) {
    return value.slice(0);
}
var Entry = /** @class */ (function () {
    function Entry(fn, args) {
        this.fn = fn;
        this.args = args;
        this.parents = new Set();
        this.childValues = new Map();
        // When this Entry has children that are dirty, this property becomes
        // a Set containing other Entry objects, borrowed from emptySetPool.
        // When the set becomes empty, it gets recycled back to emptySetPool.
        this.dirtyChildren = null;
        this.dirty = true;
        this.recomputing = false;
        this.value = [];
        ++Entry.count;
    }
    // This is the most important method of the Entry API, because it
    // determines whether the cached this.value can be returned immediately,
    // or must be recomputed. The overall performance of the caching system
    // depends on the truth of the following observations: (1) this.dirty is
    // usually false, (2) this.dirtyChildren is usually null/empty, and thus
    // (3) valueGet(this.value) is usually returned without recomputation.
    Entry.prototype.recompute = function () {
        assert(!this.recomputing, "already recomputing");
        if (!rememberParent(this) && maybeReportOrphan(this)) {
            // The recipient of the entry.reportOrphan callback decided to dispose
            // of this orphan entry by calling entry.dispose(), so we don't need to
            // (and should not) proceed with the recomputation.
            return void 0;
        }
        return mightBeDirty(this)
            ? reallyRecompute(this)
            : valueGet(this.value);
    };
    Entry.prototype.setDirty = function () {
        if (this.dirty)
            return;
        this.dirty = true;
        this.value.length = 0;
        reportDirty(this);
        // We can go ahead and unsubscribe here, since any further dirty
        // notifications we receive will be redundant, and unsubscribing may
        // free up some resources, e.g. file watchers.
        maybeUnsubscribe(this);
    };
    Entry.prototype.dispose = function () {
        var _this = this;
        forgetChildren(this).forEach(maybeReportOrphan);
        maybeUnsubscribe(this);
        // Because this entry has been kicked out of the cache (in index.js),
        // we've lost the ability to find out if/when this entry becomes dirty,
        // whether that happens through a subscription, because of a direct call
        // to entry.setDirty(), or because one of its children becomes dirty.
        // Because of this loss of future information, we have to assume the
        // worst (that this entry might have become dirty very soon), so we must
        // immediately mark this entry's parents as dirty. Normally we could
        // just call entry.setDirty() rather than calling parent.setDirty() for
        // each parent, but that would leave this entry in parent.childValues
        // and parent.dirtyChildren, which would prevent the child from being
        // truly forgotten.
        this.parents.forEach(function (parent) {
            parent.setDirty();
            forgetChild(parent, _this);
        });
    };
    Entry.count = 0;
    return Entry;
}());
function rememberParent(child) {
    var parent = parentEntrySlot.getValue();
    if (parent) {
        child.parents.add(parent);
        if (!parent.childValues.has(child)) {
            parent.childValues.set(child, []);
        }
        if (mightBeDirty(child)) {
            reportDirtyChild(parent, child);
        }
        else {
            reportCleanChild(parent, child);
        }
        return parent;
    }
}
function reallyRecompute(entry) {
    // Since this recomputation is likely to re-remember some of this
    // entry's children, we forget our children here but do not call
    // maybeReportOrphan until after the recomputation finishes.
    var originalChildren = forgetChildren(entry);
    // Set entry as the parent entry while calling recomputeNewValue(entry).
    parentEntrySlot.withValue(entry, recomputeNewValue, [entry]);
    if (maybeSubscribe(entry)) {
        // If we successfully recomputed entry.value and did not fail to
        // (re)subscribe, then this Entry is no longer explicitly dirty.
        setClean(entry);
    }
    // Now that we've had a chance to re-remember any children that were
    // involved in the recomputation, we can safely report any orphan
    // children that remain.
    originalChildren.forEach(maybeReportOrphan);
    return valueGet(entry.value);
}
function recomputeNewValue(entry) {
    entry.recomputing = true;
    // Set entry.value as unknown.
    entry.value.length = 0;
    try {
        // If entry.fn succeeds, entry.value will become a normal Value.
        entry.value[0] = entry.fn.apply(null, entry.args);
    }
    catch (e) {
        // If entry.fn throws, entry.value will become exceptional.
        entry.value[1] = e;
    }
    // Either way, this line is always reached.
    entry.recomputing = false;
}
function mightBeDirty(entry) {
    return entry.dirty || !!(entry.dirtyChildren && entry.dirtyChildren.size);
}
function setClean(entry) {
    entry.dirty = false;
    if (mightBeDirty(entry)) {
        // This Entry may still have dirty children, in which case we can't
        // let our parents know we're clean just yet.
        return;
    }
    reportClean(entry);
}
function reportDirty(child) {
    child.parents.forEach(function (parent) { return reportDirtyChild(parent, child); });
}
function reportClean(child) {
    child.parents.forEach(function (parent) { return reportCleanChild(parent, child); });
}
// Let a parent Entry know that one of its children may be dirty.
function reportDirtyChild(parent, child) {
    // Must have called rememberParent(child) before calling
    // reportDirtyChild(parent, child).
    assert(parent.childValues.has(child));
    assert(mightBeDirty(child));
    if (!parent.dirtyChildren) {
        parent.dirtyChildren = emptySetPool.pop() || new Set;
    }
    else if (parent.dirtyChildren.has(child)) {
        // If we already know this child is dirty, then we must have already
        // informed our own parents that we are dirty, so we can terminate
        // the recursion early.
        return;
    }
    parent.dirtyChildren.add(child);
    reportDirty(parent);
}
// Let a parent Entry know that one of its children is no longer dirty.
function reportCleanChild(parent, child) {
    // Must have called rememberChild(child) before calling
    // reportCleanChild(parent, child).
    assert(parent.childValues.has(child));
    assert(!mightBeDirty(child));
    var childValue = parent.childValues.get(child);
    if (childValue.length === 0) {
        parent.childValues.set(child, valueCopy(child.value));
    }
    else if (!valueIs(childValue, child.value)) {
        parent.setDirty();
    }
    removeDirtyChild(parent, child);
    if (mightBeDirty(parent)) {
        return;
    }
    reportClean(parent);
}
function removeDirtyChild(parent, child) {
    var dc = parent.dirtyChildren;
    if (dc) {
        dc.delete(child);
        if (dc.size === 0) {
            if (emptySetPool.length < POOL_TARGET_SIZE) {
                emptySetPool.push(dc);
            }
            parent.dirtyChildren = null;
        }
    }
}
// If the given entry has a reportOrphan method, and no remaining parents,
// call entry.reportOrphan and return true iff it returns true. The
// reportOrphan function should return true to indicate entry.dispose()
// has been called, and the entry has been removed from any other caches
// (see index.js for the only current example).
function maybeReportOrphan(entry) {
    return entry.parents.size === 0 &&
        typeof entry.reportOrphan === "function" &&
        entry.reportOrphan() === true;
}
// Removes all children from this entry and returns an array of the
// removed children.
function forgetChildren(parent) {
    var children = reusableEmptyArray;
    if (parent.childValues.size > 0) {
        children = [];
        parent.childValues.forEach(function (_value, child) {
            forgetChild(parent, child);
            children.push(child);
        });
    }
    // After we forget all our children, this.dirtyChildren must be empty
    // and therefore must have been reset to null.
    assert(parent.dirtyChildren === null);
    return children;
}
function forgetChild(parent, child) {
    child.parents.delete(parent);
    parent.childValues.delete(child);
    removeDirtyChild(parent, child);
}
function maybeSubscribe(entry) {
    if (typeof entry.subscribe === "function") {
        try {
            maybeUnsubscribe(entry); // Prevent double subscriptions.
            entry.unsubscribe = entry.subscribe.apply(null, entry.args);
        }
        catch (e) {
            // If this Entry has a subscribe function and it threw an exception
            // (or an unsubscribe function it previously returned now throws),
            // return false to indicate that we were not able to subscribe (or
            // unsubscribe), and this Entry should remain dirty.
            entry.setDirty();
            return false;
        }
    }
    // Returning true indicates either that there was no entry.subscribe
    // function or that it succeeded.
    return true;
}
function maybeUnsubscribe(entry) {
    var unsubscribe = entry.unsubscribe;
    if (typeof unsubscribe === "function") {
        entry.unsubscribe = void 0;
        unsubscribe();
    }
}

// A trie data structure that holds object keys weakly, yet can also hold
// non-object keys, unlike the native `WeakMap`.
var KeyTrie = /** @class */ (function () {
    function KeyTrie(weakness) {
        this.weakness = weakness;
    }
    KeyTrie.prototype.lookup = function () {
        var array = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            array[_i] = arguments[_i];
        }
        return this.lookupArray(array);
    };
    KeyTrie.prototype.lookupArray = function (array) {
        var node = this;
        array.forEach(function (key) { return node = node.getChildTrie(key); });
        return node.data || (node.data = Object.create(null));
    };
    KeyTrie.prototype.getChildTrie = function (key) {
        var map = this.weakness && isObjRef(key)
            ? this.weak || (this.weak = new WeakMap())
            : this.strong || (this.strong = new Map());
        var child = map.get(key);
        if (!child)
            map.set(key, child = new KeyTrie(this.weakness));
        return child;
    };
    return KeyTrie;
}());
function isObjRef(value) {
    switch (typeof value) {
        case "object":
            if (value === null)
                break;
        // Fall through to return true...
        case "function":
            return true;
    }
    return false;
}

// The defaultMakeCacheKey function is remarkably powerful, because it gives
// a unique object for any shallow-identical list of arguments. If you need
// to implement a custom makeCacheKey function, you may find it helpful to
// delegate the final work to defaultMakeCacheKey, which is why we export it
// here. However, you may want to avoid defaultMakeCacheKey if your runtime
// does not support WeakMap, or you have the ability to return a string key.
// In those cases, just write your own custom makeCacheKey functions.
var keyTrie = new KeyTrie(typeof WeakMap === "function");
function defaultMakeCacheKey() {
    var args = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        args[_i] = arguments[_i];
    }
    return keyTrie.lookupArray(args);
}
var caches = new Set();
function wrap(originalFunction, options) {
    if (options === void 0) { options = Object.create(null); }
    var cache = new Cache(options.max || Math.pow(2, 16), function (entry) { return entry.dispose(); });
    var disposable = !!options.disposable;
    var makeCacheKey = options.makeCacheKey || defaultMakeCacheKey;
    function optimistic() {
        if (disposable && !parentEntrySlot.hasValue()) {
            // If there's no current parent computation, and this wrapped
            // function is disposable (meaning we don't care about entry.value,
            // just dependency tracking), then we can short-cut everything else
            // in this function, because entry.recompute() is going to recycle
            // the entry object without recomputing anything, anyway.
            return void 0;
        }
        var key = makeCacheKey.apply(null, arguments);
        if (key === void 0) {
            return originalFunction.apply(null, arguments);
        }
        var args = Array.prototype.slice.call(arguments);
        var entry = cache.get(key);
        if (entry) {
            entry.args = args;
        }
        else {
            entry = new Entry(originalFunction, args);
            cache.set(key, entry);
            entry.subscribe = options.subscribe;
            if (disposable) {
                entry.reportOrphan = function () { return cache.delete(key); };
            }
        }
        var value = entry.recompute();
        // Move this entry to the front of the least-recently used queue,
        // since we just finished computing its value.
        cache.set(key, entry);
        caches.add(cache);
        // Clean up any excess entries in the cache, but only if there is no
        // active parent entry, meaning we're not in the middle of a larger
        // computation that might be flummoxed by the cleaning.
        if (!parentEntrySlot.hasValue()) {
            caches.forEach(function (cache) { return cache.clean(); });
            caches.clear();
        }
        // If options.disposable is truthy, the caller of wrap is telling us
        // they don't care about the result of entry.recompute(), so we should
        // avoid returning the value, so it won't be accidentally used.
        return disposable ? void 0 : value;
    }
    optimistic.dirty = function () {
        var key = makeCacheKey.apply(null, arguments);
        var child = key !== void 0 && cache.get(key);
        if (child) {
            child.setDirty();
        }
    };
    return optimistic;
}

Object.defineProperty(exports, 'asyncFromGen', {
  enumerable: true,
  get: function () {
    return context.asyncFromGen;
  }
});
Object.defineProperty(exports, 'bindContext', {
  enumerable: true,
  get: function () {
    return context.bind;
  }
});
Object.defineProperty(exports, 'noContext', {
  enumerable: true,
  get: function () {
    return context.noContext;
  }
});
Object.defineProperty(exports, 'setTimeout', {
  enumerable: true,
  get: function () {
    return context.setTimeout;
  }
});
exports.KeyTrie = KeyTrie;
exports.defaultMakeCacheKey = defaultMakeCacheKey;
exports.wrap = wrap;


},{"@wry/context":1}],146:[function(require,module,exports){
// shim for using process in browser
var process = module.exports = {};

// cached from whatever global is present so that test runners that stub it
// don't break things.  But we need to wrap it in a try catch in case it is
// wrapped in strict mode code which doesn't define any globals.  It's inside a
// function because try/catches deoptimize in certain engines.

var cachedSetTimeout;
var cachedClearTimeout;

function defaultSetTimout() {
    throw new Error('setTimeout has not been defined');
}
function defaultClearTimeout () {
    throw new Error('clearTimeout has not been defined');
}
(function () {
    try {
        if (typeof setTimeout === 'function') {
            cachedSetTimeout = setTimeout;
        } else {
            cachedSetTimeout = defaultSetTimout;
        }
    } catch (e) {
        cachedSetTimeout = defaultSetTimout;
    }
    try {
        if (typeof clearTimeout === 'function') {
            cachedClearTimeout = clearTimeout;
        } else {
            cachedClearTimeout = defaultClearTimeout;
        }
    } catch (e) {
        cachedClearTimeout = defaultClearTimeout;
    }
} ())
function runTimeout(fun) {
    if (cachedSetTimeout === setTimeout) {
        //normal enviroments in sane situations
        return setTimeout(fun, 0);
    }
    // if setTimeout wasn't available but was latter defined
    if ((cachedSetTimeout === defaultSetTimout || !cachedSetTimeout) && setTimeout) {
        cachedSetTimeout = setTimeout;
        return setTimeout(fun, 0);
    }
    try {
        // when when somebody has screwed with setTimeout but no I.E. maddness
        return cachedSetTimeout(fun, 0);
    } catch(e){
        try {
            // When we are in I.E. but the script has been evaled so I.E. doesn't trust the global object when called normally
            return cachedSetTimeout.call(null, fun, 0);
        } catch(e){
            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error
            return cachedSetTimeout.call(this, fun, 0);
        }
    }


}
function runClearTimeout(marker) {
    if (cachedClearTimeout === clearTimeout) {
        //normal enviroments in sane situations
        return clearTimeout(marker);
    }
    // if clearTimeout wasn't available but was latter defined
    if ((cachedClearTimeout === defaultClearTimeout || !cachedClearTimeout) && clearTimeout) {
        cachedClearTimeout = clearTimeout;
        return clearTimeout(marker);
    }
    try {
        // when when somebody has screwed with setTimeout but no I.E. maddness
        return cachedClearTimeout(marker);
    } catch (e){
        try {
            // When we are in I.E. but the script has been evaled so I.E. doesn't  trust the global object when called normally
            return cachedClearTimeout.call(null, marker);
        } catch (e){
            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error.
            // Some versions of I.E. have different rules for clearTimeout vs setTimeout
            return cachedClearTimeout.call(this, marker);
        }
    }



}
var queue = [];
var draining = false;
var currentQueue;
var queueIndex = -1;

function cleanUpNextTick() {
    if (!draining || !currentQueue) {
        return;
    }
    draining = false;
    if (currentQueue.length) {
        queue = currentQueue.concat(queue);
    } else {
        queueIndex = -1;
    }
    if (queue.length) {
        drainQueue();
    }
}

function drainQueue() {
    if (draining) {
        return;
    }
    var timeout = runTimeout(cleanUpNextTick);
    draining = true;

    var len = queue.length;
    while(len) {
        currentQueue = queue;
        queue = [];
        while (++queueIndex < len) {
            if (currentQueue) {
                currentQueue[queueIndex].run();
            }
        }
        queueIndex = -1;
        len = queue.length;
    }
    currentQueue = null;
    draining = false;
    runClearTimeout(timeout);
}

process.nextTick = function (fun) {
    var args = new Array(arguments.length - 1);
    if (arguments.length > 1) {
        for (var i = 1; i < arguments.length; i++) {
            args[i - 1] = arguments[i];
        }
    }
    queue.push(new Item(fun, args));
    if (queue.length === 1 && !draining) {
        runTimeout(drainQueue);
    }
};

// v8 likes predictible objects
function Item(fun, array) {
    this.fun = fun;
    this.array = array;
}
Item.prototype.run = function () {
    this.fun.apply(null, this.array);
};
process.title = 'browser';
process.browser = true;
process.env = {};
process.argv = [];
process.version = ''; // empty string to avoid regexp issues
process.versions = {};

function noop() {}

process.on = noop;
process.addListener = noop;
process.once = noop;
process.off = noop;
process.removeListener = noop;
process.removeAllListeners = noop;
process.emit = noop;
process.prependListener = noop;
process.prependOnceListener = noop;

process.listeners = function (name) { return [] }

process.binding = function (name) {
    throw new Error('process.binding is not supported');
};

process.cwd = function () { return '/' };
process.chdir = function (dir) {
    throw new Error('process.chdir is not supported');
};
process.umask = function() { return 0; };

},{}],147:[function(require,module,exports){
(function (global){(function (){
"use strict";
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
var __spreadArrays = (this && this.__spreadArrays) || function () {
    for (var s = 0, i = 0, il = arguments.length; i < il; i++) s += arguments[i].length;
    for (var r = Array(s), k = 0, i = 0; i < il; i++)
        for (var a = arguments[i], j = 0, jl = a.length; j < jl; j++, k++)
            r[k] = a[j];
    return r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.SubscriptionClient = void 0;
var _global = typeof global !== 'undefined' ? global : (typeof window !== 'undefined' ? window : {});
var NativeWebSocket = _global.WebSocket || _global.MozWebSocket;
var Backoff = require("backo2");
var eventemitter3_1 = require("eventemitter3");
var is_string_1 = require("./utils/is-string");
var is_object_1 = require("./utils/is-object");
var printer_1 = require("graphql/language/printer");
var getOperationAST_1 = require("graphql/utilities/getOperationAST");
var symbol_observable_1 = require("symbol-observable");
var protocol_1 = require("./protocol");
var defaults_1 = require("./defaults");
var message_types_1 = require("./message-types");
var SubscriptionClient = (function () {
    function SubscriptionClient(url, options, webSocketImpl, webSocketProtocols) {
        var _a = (options || {}), _b = _a.connectionCallback, connectionCallback = _b === void 0 ? undefined : _b, _c = _a.connectionParams, connectionParams = _c === void 0 ? {} : _c, _d = _a.minTimeout, minTimeout = _d === void 0 ? defaults_1.MIN_WS_TIMEOUT : _d, _e = _a.timeout, timeout = _e === void 0 ? defaults_1.WS_TIMEOUT : _e, _f = _a.reconnect, reconnect = _f === void 0 ? false : _f, _g = _a.reconnectionAttempts, reconnectionAttempts = _g === void 0 ? Infinity : _g, _h = _a.lazy, lazy = _h === void 0 ? false : _h, _j = _a.inactivityTimeout, inactivityTimeout = _j === void 0 ? 0 : _j, _k = _a.wsOptionArguments, wsOptionArguments = _k === void 0 ? [] : _k;
        this.wsImpl = webSocketImpl || NativeWebSocket;
        if (!this.wsImpl) {
            throw new Error('Unable to find native implementation, or alternative implementation for WebSocket!');
        }
        this.wsProtocols = webSocketProtocols || protocol_1.GRAPHQL_WS;
        this.connectionCallback = connectionCallback;
        this.url = url;
        this.operations = {};
        this.nextOperationId = 0;
        this.minWsTimeout = minTimeout;
        this.wsTimeout = timeout;
        this.unsentMessagesQueue = [];
        this.reconnect = reconnect;
        this.reconnecting = false;
        this.reconnectionAttempts = reconnectionAttempts;
        this.lazy = !!lazy;
        this.inactivityTimeout = inactivityTimeout;
        this.closedByUser = false;
        this.backoff = new Backoff({ jitter: 0.5 });
        this.eventEmitter = new eventemitter3_1.EventEmitter();
        this.middlewares = [];
        this.client = null;
        this.maxConnectTimeGenerator = this.createMaxConnectTimeGenerator();
        this.connectionParams = this.getConnectionParams(connectionParams);
        this.wsOptionArguments = wsOptionArguments;
        if (!this.lazy) {
            this.connect();
        }
    }
    Object.defineProperty(SubscriptionClient.prototype, "status", {
        get: function () {
            if (this.client === null) {
                return this.wsImpl.CLOSED;
            }
            return this.client.readyState;
        },
        enumerable: false,
        configurable: true
    });
    SubscriptionClient.prototype.close = function (isForced, closedByUser) {
        if (isForced === void 0) { isForced = true; }
        if (closedByUser === void 0) { closedByUser = true; }
        this.clearInactivityTimeout();
        if (this.client !== null) {
            this.closedByUser = closedByUser;
            if (isForced) {
                this.clearCheckConnectionInterval();
                this.clearMaxConnectTimeout();
                this.clearTryReconnectTimeout();
                this.unsubscribeAll();
                this.sendMessage(undefined, message_types_1.default.GQL_CONNECTION_TERMINATE, null);
            }
            this.client.close();
            this.client.onopen = null;
            this.client.onclose = null;
            this.client.onerror = null;
            this.client.onmessage = null;
            this.client = null;
            this.eventEmitter.emit('disconnected');
            if (!isForced) {
                this.tryReconnect();
            }
        }
    };
    SubscriptionClient.prototype.request = function (request) {
        var _a;
        var getObserver = this.getObserver.bind(this);
        var executeOperation = this.executeOperation.bind(this);
        var unsubscribe = this.unsubscribe.bind(this);
        var opId;
        this.clearInactivityTimeout();
        return _a = {},
            _a[symbol_observable_1.default] = function () {
                return this;
            },
            _a.subscribe = function (observerOrNext, onError, onComplete) {
                var observer = getObserver(observerOrNext, onError, onComplete);
                opId = executeOperation(request, function (error, result) {
                    if (error === null && result === null) {
                        if (observer.complete) {
                            observer.complete();
                        }
                    }
                    else if (error) {
                        if (observer.error) {
                            observer.error(error[0]);
                        }
                    }
                    else {
                        if (observer.next) {
                            observer.next(result);
                        }
                    }
                });
                return {
                    unsubscribe: function () {
                        if (opId) {
                            unsubscribe(opId);
                            opId = null;
                        }
                    },
                };
            },
            _a;
    };
    SubscriptionClient.prototype.on = function (eventName, callback, context) {
        var handler = this.eventEmitter.on(eventName, callback, context);
        return function () {
            handler.off(eventName, callback, context);
        };
    };
    SubscriptionClient.prototype.onConnected = function (callback, context) {
        return this.on('connected', callback, context);
    };
    SubscriptionClient.prototype.onConnecting = function (callback, context) {
        return this.on('connecting', callback, context);
    };
    SubscriptionClient.prototype.onDisconnected = function (callback, context) {
        return this.on('disconnected', callback, context);
    };
    SubscriptionClient.prototype.onReconnected = function (callback, context) {
        return this.on('reconnected', callback, context);
    };
    SubscriptionClient.prototype.onReconnecting = function (callback, context) {
        return this.on('reconnecting', callback, context);
    };
    SubscriptionClient.prototype.onError = function (callback, context) {
        return this.on('error', callback, context);
    };
    SubscriptionClient.prototype.unsubscribeAll = function () {
        var _this = this;
        Object.keys(this.operations).forEach(function (subId) {
            _this.unsubscribe(subId);
        });
    };
    SubscriptionClient.prototype.applyMiddlewares = function (options) {
        var _this = this;
        return new Promise(function (resolve, reject) {
            var queue = function (funcs, scope) {
                var next = function (error) {
                    if (error) {
                        reject(error);
                    }
                    else {
                        if (funcs.length > 0) {
                            var f = funcs.shift();
                            if (f) {
                                f.applyMiddleware.apply(scope, [options, next]);
                            }
                        }
                        else {
                            resolve(options);
                        }
                    }
                };
                next();
            };
            queue(__spreadArrays(_this.middlewares), _this);
        });
    };
    SubscriptionClient.prototype.use = function (middlewares) {
        var _this = this;
        middlewares.map(function (middleware) {
            if (typeof middleware.applyMiddleware === 'function') {
                _this.middlewares.push(middleware);
            }
            else {
                throw new Error('Middleware must implement the applyMiddleware function.');
            }
        });
        return this;
    };
    SubscriptionClient.prototype.getConnectionParams = function (connectionParams) {
        return function () { return new Promise(function (resolve, reject) {
            if (typeof connectionParams === 'function') {
                try {
                    return resolve(connectionParams.call(null));
                }
                catch (error) {
                    return reject(error);
                }
            }
            resolve(connectionParams);
        }); };
    };
    SubscriptionClient.prototype.executeOperation = function (options, handler) {
        var _this = this;
        if (this.client === null) {
            this.connect();
        }
        var opId = this.generateOperationId();
        this.operations[opId] = { options: options, handler: handler };
        this.applyMiddlewares(options)
            .then(function (processedOptions) {
            _this.checkOperationOptions(processedOptions, handler);
            if (_this.operations[opId]) {
                _this.operations[opId] = { options: processedOptions, handler: handler };
                _this.sendMessage(opId, message_types_1.default.GQL_START, processedOptions);
            }
        })
            .catch(function (error) {
            _this.unsubscribe(opId);
            handler(_this.formatErrors(error));
        });
        return opId;
    };
    SubscriptionClient.prototype.getObserver = function (observerOrNext, error, complete) {
        if (typeof observerOrNext === 'function') {
            return {
                next: function (v) { return observerOrNext(v); },
                error: function (e) { return error && error(e); },
                complete: function () { return complete && complete(); },
            };
        }
        return observerOrNext;
    };
    SubscriptionClient.prototype.createMaxConnectTimeGenerator = function () {
        var minValue = this.minWsTimeout;
        var maxValue = this.wsTimeout;
        return new Backoff({
            min: minValue,
            max: maxValue,
            factor: 1.2,
        });
    };
    SubscriptionClient.prototype.clearCheckConnectionInterval = function () {
        if (this.checkConnectionIntervalId) {
            clearInterval(this.checkConnectionIntervalId);
            this.checkConnectionIntervalId = null;
        }
    };
    SubscriptionClient.prototype.clearMaxConnectTimeout = function () {
        if (this.maxConnectTimeoutId) {
            clearTimeout(this.maxConnectTimeoutId);
            this.maxConnectTimeoutId = null;
        }
    };
    SubscriptionClient.prototype.clearTryReconnectTimeout = function () {
        if (this.tryReconnectTimeoutId) {
            clearTimeout(this.tryReconnectTimeoutId);
            this.tryReconnectTimeoutId = null;
        }
    };
    SubscriptionClient.prototype.clearInactivityTimeout = function () {
        if (this.inactivityTimeoutId) {
            clearTimeout(this.inactivityTimeoutId);
            this.inactivityTimeoutId = null;
        }
    };
    SubscriptionClient.prototype.setInactivityTimeout = function () {
        var _this = this;
        if (this.inactivityTimeout > 0 && Object.keys(this.operations).length === 0) {
            this.inactivityTimeoutId = setTimeout(function () {
                if (Object.keys(_this.operations).length === 0) {
                    _this.close();
                }
            }, this.inactivityTimeout);
        }
    };
    SubscriptionClient.prototype.checkOperationOptions = function (options, handler) {
        var query = options.query, variables = options.variables, operationName = options.operationName;
        if (!query) {
            throw new Error('Must provide a query.');
        }
        if (!handler) {
            throw new Error('Must provide an handler.');
        }
        if ((!is_string_1.default(query) && !getOperationAST_1.getOperationAST(query, operationName)) ||
            (operationName && !is_string_1.default(operationName)) ||
            (variables && !is_object_1.default(variables))) {
            throw new Error('Incorrect option types. query must be a string or a document,' +
                '`operationName` must be a string, and `variables` must be an object.');
        }
    };
    SubscriptionClient.prototype.buildMessage = function (id, type, payload) {
        var payloadToReturn = payload && payload.query ? __assign(__assign({}, payload), { query: typeof payload.query === 'string' ? payload.query : printer_1.print(payload.query) }) :
            payload;
        return {
            id: id,
            type: type,
            payload: payloadToReturn,
        };
    };
    SubscriptionClient.prototype.formatErrors = function (errors) {
        if (Array.isArray(errors)) {
            return errors;
        }
        if (errors && errors.errors) {
            return this.formatErrors(errors.errors);
        }
        if (errors && errors.message) {
            return [errors];
        }
        return [{
                name: 'FormatedError',
                message: 'Unknown error',
                originalError: errors,
            }];
    };
    SubscriptionClient.prototype.sendMessage = function (id, type, payload) {
        this.sendMessageRaw(this.buildMessage(id, type, payload));
    };
    SubscriptionClient.prototype.sendMessageRaw = function (message) {
        switch (this.status) {
            case this.wsImpl.OPEN:
                var serializedMessage = JSON.stringify(message);
                try {
                    JSON.parse(serializedMessage);
                }
                catch (e) {
                    this.eventEmitter.emit('error', new Error("Message must be JSON-serializable. Got: " + message));
                }
                this.client.send(serializedMessage);
                break;
            case this.wsImpl.CONNECTING:
                this.unsentMessagesQueue.push(message);
                break;
            default:
                if (!this.reconnecting) {
                    this.eventEmitter.emit('error', new Error('A message was not sent because socket is not connected, is closing or ' +
                        'is already closed. Message was: ' + JSON.stringify(message)));
                }
        }
    };
    SubscriptionClient.prototype.generateOperationId = function () {
        return String(++this.nextOperationId);
    };
    SubscriptionClient.prototype.tryReconnect = function () {
        var _this = this;
        if (!this.reconnect || this.backoff.attempts >= this.reconnectionAttempts) {
            return;
        }
        if (!this.reconnecting) {
            Object.keys(this.operations).forEach(function (key) {
                _this.unsentMessagesQueue.push(_this.buildMessage(key, message_types_1.default.GQL_START, _this.operations[key].options));
            });
            this.reconnecting = true;
        }
        this.clearTryReconnectTimeout();
        var delay = this.backoff.duration();
        this.tryReconnectTimeoutId = setTimeout(function () {
            _this.connect();
        }, delay);
    };
    SubscriptionClient.prototype.flushUnsentMessagesQueue = function () {
        var _this = this;
        this.unsentMessagesQueue.forEach(function (message) {
            _this.sendMessageRaw(message);
        });
        this.unsentMessagesQueue = [];
    };
    SubscriptionClient.prototype.checkConnection = function () {
        if (this.wasKeepAliveReceived) {
            this.wasKeepAliveReceived = false;
            return;
        }
        if (!this.reconnecting) {
            this.close(false, true);
        }
    };
    SubscriptionClient.prototype.checkMaxConnectTimeout = function () {
        var _this = this;
        this.clearMaxConnectTimeout();
        this.maxConnectTimeoutId = setTimeout(function () {
            if (_this.status !== _this.wsImpl.OPEN) {
                _this.reconnecting = true;
                _this.close(false, true);
            }
        }, this.maxConnectTimeGenerator.duration());
    };
    SubscriptionClient.prototype.connect = function () {
        var _a;
        var _this = this;
        this.client = new ((_a = this.wsImpl).bind.apply(_a, __spreadArrays([void 0, this.url, this.wsProtocols], this.wsOptionArguments)))();
        this.checkMaxConnectTimeout();
        this.client.onopen = function () { return __awaiter(_this, void 0, void 0, function () {
            var connectionParams, error_1;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (!(this.status === this.wsImpl.OPEN)) return [3, 4];
                        this.clearMaxConnectTimeout();
                        this.closedByUser = false;
                        this.eventEmitter.emit(this.reconnecting ? 'reconnecting' : 'connecting');
                        _a.label = 1;
                    case 1:
                        _a.trys.push([1, 3, , 4]);
                        return [4, this.connectionParams()];
                    case 2:
                        connectionParams = _a.sent();
                        this.sendMessage(undefined, message_types_1.default.GQL_CONNECTION_INIT, connectionParams);
                        this.flushUnsentMessagesQueue();
                        return [3, 4];
                    case 3:
                        error_1 = _a.sent();
                        this.sendMessage(undefined, message_types_1.default.GQL_CONNECTION_ERROR, error_1);
                        this.flushUnsentMessagesQueue();
                        return [3, 4];
                    case 4: return [2];
                }
            });
        }); };
        this.client.onclose = function () {
            if (!_this.closedByUser) {
                _this.close(false, false);
            }
        };
        this.client.onerror = function (err) {
            _this.eventEmitter.emit('error', err);
        };
        this.client.onmessage = function (_a) {
            var data = _a.data;
            _this.processReceivedData(data);
        };
    };
    SubscriptionClient.prototype.processReceivedData = function (receivedData) {
        var parsedMessage;
        var opId;
        try {
            parsedMessage = JSON.parse(receivedData);
            opId = parsedMessage.id;
        }
        catch (e) {
            throw new Error("Message must be JSON-parseable. Got: " + receivedData);
        }
        if ([message_types_1.default.GQL_DATA,
            message_types_1.default.GQL_COMPLETE,
            message_types_1.default.GQL_ERROR,
        ].indexOf(parsedMessage.type) !== -1 && !this.operations[opId]) {
            this.unsubscribe(opId);
            return;
        }
        switch (parsedMessage.type) {
            case message_types_1.default.GQL_CONNECTION_ERROR:
                if (this.connectionCallback) {
                    this.connectionCallback(parsedMessage.payload);
                }
                break;
            case message_types_1.default.GQL_CONNECTION_ACK:
                this.eventEmitter.emit(this.reconnecting ? 'reconnected' : 'connected', parsedMessage.payload);
                this.reconnecting = false;
                this.backoff.reset();
                this.maxConnectTimeGenerator.reset();
                if (this.connectionCallback) {
                    this.connectionCallback();
                }
                break;
            case message_types_1.default.GQL_COMPLETE:
                var handler = this.operations[opId].handler;
                delete this.operations[opId];
                handler.call(this, null, null);
                break;
            case message_types_1.default.GQL_ERROR:
                this.operations[opId].handler(this.formatErrors(parsedMessage.payload), null);
                delete this.operations[opId];
                break;
            case message_types_1.default.GQL_DATA:
                var parsedPayload = !parsedMessage.payload.errors ?
                    parsedMessage.payload : __assign(__assign({}, parsedMessage.payload), { errors: this.formatErrors(parsedMessage.payload.errors) });
                this.operations[opId].handler(null, parsedPayload);
                break;
            case message_types_1.default.GQL_CONNECTION_KEEP_ALIVE:
                var firstKA = typeof this.wasKeepAliveReceived === 'undefined';
                this.wasKeepAliveReceived = true;
                if (firstKA) {
                    this.checkConnection();
                }
                if (this.checkConnectionIntervalId) {
                    clearInterval(this.checkConnectionIntervalId);
                    this.checkConnection();
                }
                this.checkConnectionIntervalId = setInterval(this.checkConnection.bind(this), this.wsTimeout);
                break;
            default:
                throw new Error('Invalid message type!');
        }
    };
    SubscriptionClient.prototype.unsubscribe = function (opId) {
        if (this.operations[opId]) {
            delete this.operations[opId];
            this.setInactivityTimeout();
            this.sendMessage(opId, message_types_1.default.GQL_STOP, undefined);
        }
    };
    return SubscriptionClient;
}());
exports.SubscriptionClient = SubscriptionClient;

}).call(this)}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./defaults":148,"./message-types":149,"./protocol":150,"./utils/is-object":151,"./utils/is-string":152,"backo2":12,"eventemitter3":13,"graphql/language/printer":61,"graphql/utilities/getOperationAST":93,"symbol-observable":153}],148:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.WS_TIMEOUT = exports.MIN_WS_TIMEOUT = void 0;
var MIN_WS_TIMEOUT = 1000;
exports.MIN_WS_TIMEOUT = MIN_WS_TIMEOUT;
var WS_TIMEOUT = 30000;
exports.WS_TIMEOUT = WS_TIMEOUT;

},{}],149:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var MessageTypes = (function () {
    function MessageTypes() {
        throw new Error('Static Class');
    }
    MessageTypes.GQL_CONNECTION_INIT = 'connection_init';
    MessageTypes.GQL_CONNECTION_ACK = 'connection_ack';
    MessageTypes.GQL_CONNECTION_ERROR = 'connection_error';
    MessageTypes.GQL_CONNECTION_KEEP_ALIVE = 'ka';
    MessageTypes.GQL_CONNECTION_TERMINATE = 'connection_terminate';
    MessageTypes.GQL_START = 'start';
    MessageTypes.GQL_DATA = 'data';
    MessageTypes.GQL_ERROR = 'error';
    MessageTypes.GQL_COMPLETE = 'complete';
    MessageTypes.GQL_STOP = 'stop';
    MessageTypes.SUBSCRIPTION_START = 'subscription_start';
    MessageTypes.SUBSCRIPTION_DATA = 'subscription_data';
    MessageTypes.SUBSCRIPTION_SUCCESS = 'subscription_success';
    MessageTypes.SUBSCRIPTION_FAIL = 'subscription_fail';
    MessageTypes.SUBSCRIPTION_END = 'subscription_end';
    MessageTypes.INIT = 'init';
    MessageTypes.INIT_SUCCESS = 'init_success';
    MessageTypes.INIT_FAIL = 'init_fail';
    MessageTypes.KEEP_ALIVE = 'keepalive';
    return MessageTypes;
}());
exports.default = MessageTypes;

},{}],150:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.GRAPHQL_SUBSCRIPTIONS = exports.GRAPHQL_WS = void 0;
var GRAPHQL_WS = 'graphql-ws';
exports.GRAPHQL_WS = GRAPHQL_WS;
var GRAPHQL_SUBSCRIPTIONS = 'graphql-subscriptions';
exports.GRAPHQL_SUBSCRIPTIONS = GRAPHQL_SUBSCRIPTIONS;

},{}],151:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function isObject(value) {
    return ((value !== null) && (typeof value === 'object'));
}
exports.default = isObject;

},{}],152:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function isString(value) {
    return typeof value === 'string';
}
exports.default = isString;

},{}],153:[function(require,module,exports){
(function (global){(function (){
'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _ponyfill = require('./ponyfill.js');

var _ponyfill2 = _interopRequireDefault(_ponyfill);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { 'default': obj }; }

var root; /* global window */


if (typeof self !== 'undefined') {
  root = self;
} else if (typeof window !== 'undefined') {
  root = window;
} else if (typeof global !== 'undefined') {
  root = global;
} else if (typeof module !== 'undefined') {
  root = module;
} else {
  root = Function('return this')();
}

var result = (0, _ponyfill2['default'])(root);
exports['default'] = result;
}).call(this)}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./ponyfill.js":154}],154:[function(require,module,exports){
'use strict';

Object.defineProperty(exports, "__esModule", {
	value: true
});
exports['default'] = symbolObservablePonyfill;
function symbolObservablePonyfill(root) {
	var result;
	var _Symbol = root.Symbol;

	if (typeof _Symbol === 'function') {
		if (_Symbol.observable) {
			result = _Symbol.observable;
		} else {
			result = _Symbol('observable');
			_Symbol.observable = result;
		}
	} else {
		result = '@@observable';
	}

	return result;
};
},{}],155:[function(require,module,exports){
(function (process){(function (){
'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

var tslib = require('tslib');

var genericMessage = "Invariant Violation";
var _a = Object.setPrototypeOf, setPrototypeOf = _a === void 0 ? function (obj, proto) {
    obj.__proto__ = proto;
    return obj;
} : _a;
var InvariantError = /** @class */ (function (_super) {
    tslib.__extends(InvariantError, _super);
    function InvariantError(message) {
        if (message === void 0) { message = genericMessage; }
        var _this = _super.call(this, typeof message === "number"
            ? genericMessage + ": " + message + " (see https://github.com/apollographql/invariant-packages)"
            : message) || this;
        _this.framesToPop = 1;
        _this.name = genericMessage;
        setPrototypeOf(_this, InvariantError.prototype);
        return _this;
    }
    return InvariantError;
}(Error));
function invariant(condition, message) {
    if (!condition) {
        throw new InvariantError(message);
    }
}
function wrapConsoleMethod(method) {
    return function () {
        return console[method].apply(console, arguments);
    };
}
(function (invariant) {
    invariant.warn = wrapConsoleMethod("warn");
    invariant.error = wrapConsoleMethod("error");
})(invariant || (invariant = {}));
// Code that uses ts-invariant with rollup-plugin-invariant may want to
// import this process stub to avoid errors evaluating process.env.NODE_ENV.
// However, because most ESM-to-CJS compilers will rewrite the process import
// as tsInvariant.process, which prevents proper replacement by minifiers, we
// also attempt to define the stub globally when it is not already defined.
exports.process = { env: {} };
if (typeof process === "object") {
    exports.process = process;
}
else
    try {
        // Using Function to evaluate this assignment in global scope also escapes
        // the strict mode of the current module, thereby allowing the assignment.
        // Inspired by https://github.com/facebook/regenerator/pull/369.
        Function("stub", "process = stub")(exports.process);
    }
    catch (atLeastWeTried) {
        // The assignment can fail if a Content Security Policy heavy-handedly
        // forbids Function usage. In those environments, developers should take
        // extra care to replace process.env.NODE_ENV in their production builds,
        // or define an appropriate global.process polyfill.
    }
var invariant$1 = invariant;

exports.default = invariant$1;
exports.InvariantError = InvariantError;
exports.invariant = invariant;


}).call(this)}).call(this,require('_process'))
},{"_process":146,"tslib":156}],156:[function(require,module,exports){
(function (global){(function (){
/*! *****************************************************************************
Copyright (c) Microsoft Corporation.

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
***************************************************************************** */

/* global global, define, System, Reflect, Promise */
var __extends;
var __assign;
var __rest;
var __decorate;
var __param;
var __metadata;
var __awaiter;
var __generator;
var __exportStar;
var __values;
var __read;
var __spread;
var __spreadArrays;
var __await;
var __asyncGenerator;
var __asyncDelegator;
var __asyncValues;
var __makeTemplateObject;
var __importStar;
var __importDefault;
var __classPrivateFieldGet;
var __classPrivateFieldSet;
var __createBinding;
(function (factory) {
    var root = typeof global === "object" ? global : typeof self === "object" ? self : typeof this === "object" ? this : {};
    if (typeof define === "function" && define.amd) {
        define("tslib", ["exports"], function (exports) { factory(createExporter(root, createExporter(exports))); });
    }
    else if (typeof module === "object" && typeof module.exports === "object") {
        factory(createExporter(root, createExporter(module.exports)));
    }
    else {
        factory(createExporter(root));
    }
    function createExporter(exports, previous) {
        if (exports !== root) {
            if (typeof Object.create === "function") {
                Object.defineProperty(exports, "__esModule", { value: true });
            }
            else {
                exports.__esModule = true;
            }
        }
        return function (id, v) { return exports[id] = previous ? previous(id, v) : v; };
    }
})
(function (exporter) {
    var extendStatics = Object.setPrototypeOf ||
        ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
        function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };

    __extends = function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };

    __assign = Object.assign || function (t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p)) t[p] = s[p];
        }
        return t;
    };

    __rest = function (s, e) {
        var t = {};
        for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
            t[p] = s[p];
        if (s != null && typeof Object.getOwnPropertySymbols === "function")
            for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
                if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
                    t[p[i]] = s[p[i]];
            }
        return t;
    };

    __decorate = function (decorators, target, key, desc) {
        var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
        if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
        else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
        return c > 3 && r && Object.defineProperty(target, key, r), r;
    };

    __param = function (paramIndex, decorator) {
        return function (target, key) { decorator(target, key, paramIndex); }
    };

    __metadata = function (metadataKey, metadataValue) {
        if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(metadataKey, metadataValue);
    };

    __awaiter = function (thisArg, _arguments, P, generator) {
        function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
        return new (P || (P = Promise))(function (resolve, reject) {
            function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
            function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
            function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
            step((generator = generator.apply(thisArg, _arguments || [])).next());
        });
    };

    __generator = function (thisArg, body) {
        var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
        return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
        function verb(n) { return function (v) { return step([n, v]); }; }
        function step(op) {
            if (f) throw new TypeError("Generator is already executing.");
            while (_) try {
                if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
                if (y = 0, t) op = [op[0] & 2, t.value];
                switch (op[0]) {
                    case 0: case 1: t = op; break;
                    case 4: _.label++; return { value: op[1], done: false };
                    case 5: _.label++; y = op[1]; op = [0]; continue;
                    case 7: op = _.ops.pop(); _.trys.pop(); continue;
                    default:
                        if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                        if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                        if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                        if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                        if (t[2]) _.ops.pop();
                        _.trys.pop(); continue;
                }
                op = body.call(thisArg, _);
            } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
            if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
        }
    };

    __createBinding = function(o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
    };

    __exportStar = function (m, exports) {
        for (var p in m) if (p !== "default" && !exports.hasOwnProperty(p)) exports[p] = m[p];
    };

    __values = function (o) {
        var s = typeof Symbol === "function" && Symbol.iterator, m = s && o[s], i = 0;
        if (m) return m.call(o);
        if (o && typeof o.length === "number") return {
            next: function () {
                if (o && i >= o.length) o = void 0;
                return { value: o && o[i++], done: !o };
            }
        };
        throw new TypeError(s ? "Object is not iterable." : "Symbol.iterator is not defined.");
    };

    __read = function (o, n) {
        var m = typeof Symbol === "function" && o[Symbol.iterator];
        if (!m) return o;
        var i = m.call(o), r, ar = [], e;
        try {
            while ((n === void 0 || n-- > 0) && !(r = i.next()).done) ar.push(r.value);
        }
        catch (error) { e = { error: error }; }
        finally {
            try {
                if (r && !r.done && (m = i["return"])) m.call(i);
            }
            finally { if (e) throw e.error; }
        }
        return ar;
    };

    __spread = function () {
        for (var ar = [], i = 0; i < arguments.length; i++)
            ar = ar.concat(__read(arguments[i]));
        return ar;
    };

    __spreadArrays = function () {
        for (var s = 0, i = 0, il = arguments.length; i < il; i++) s += arguments[i].length;
        for (var r = Array(s), k = 0, i = 0; i < il; i++)
            for (var a = arguments[i], j = 0, jl = a.length; j < jl; j++, k++)
                r[k] = a[j];
        return r;
    };

    __await = function (v) {
        return this instanceof __await ? (this.v = v, this) : new __await(v);
    };

    __asyncGenerator = function (thisArg, _arguments, generator) {
        if (!Symbol.asyncIterator) throw new TypeError("Symbol.asyncIterator is not defined.");
        var g = generator.apply(thisArg, _arguments || []), i, q = [];
        return i = {}, verb("next"), verb("throw"), verb("return"), i[Symbol.asyncIterator] = function () { return this; }, i;
        function verb(n) { if (g[n]) i[n] = function (v) { return new Promise(function (a, b) { q.push([n, v, a, b]) > 1 || resume(n, v); }); }; }
        function resume(n, v) { try { step(g[n](v)); } catch (e) { settle(q[0][3], e); } }
        function step(r) { r.value instanceof __await ? Promise.resolve(r.value.v).then(fulfill, reject) : settle(q[0][2], r);  }
        function fulfill(value) { resume("next", value); }
        function reject(value) { resume("throw", value); }
        function settle(f, v) { if (f(v), q.shift(), q.length) resume(q[0][0], q[0][1]); }
    };

    __asyncDelegator = function (o) {
        var i, p;
        return i = {}, verb("next"), verb("throw", function (e) { throw e; }), verb("return"), i[Symbol.iterator] = function () { return this; }, i;
        function verb(n, f) { i[n] = o[n] ? function (v) { return (p = !p) ? { value: __await(o[n](v)), done: n === "return" } : f ? f(v) : v; } : f; }
    };

    __asyncValues = function (o) {
        if (!Symbol.asyncIterator) throw new TypeError("Symbol.asyncIterator is not defined.");
        var m = o[Symbol.asyncIterator], i;
        return m ? m.call(o) : (o = typeof __values === "function" ? __values(o) : o[Symbol.iterator](), i = {}, verb("next"), verb("throw"), verb("return"), i[Symbol.asyncIterator] = function () { return this; }, i);
        function verb(n) { i[n] = o[n] && function (v) { return new Promise(function (resolve, reject) { v = o[n](v), settle(resolve, reject, v.done, v.value); }); }; }
        function settle(resolve, reject, d, v) { Promise.resolve(v).then(function(v) { resolve({ value: v, done: d }); }, reject); }
    };

    __makeTemplateObject = function (cooked, raw) {
        if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
        return cooked;
    };

    __importStar = function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) result[k] = mod[k];
        result["default"] = mod;
        return result;
    };

    __importDefault = function (mod) {
        return (mod && mod.__esModule) ? mod : { "default": mod };
    };

    __classPrivateFieldGet = function (receiver, privateMap) {
        if (!privateMap.has(receiver)) {
            throw new TypeError("attempted to get private field on non-instance");
        }
        return privateMap.get(receiver);
    };

    __classPrivateFieldSet = function (receiver, privateMap, value) {
        if (!privateMap.has(receiver)) {
            throw new TypeError("attempted to set private field on non-instance");
        }
        privateMap.set(receiver, value);
        return value;
    };

    exporter("__extends", __extends);
    exporter("__assign", __assign);
    exporter("__rest", __rest);
    exporter("__decorate", __decorate);
    exporter("__param", __param);
    exporter("__metadata", __metadata);
    exporter("__awaiter", __awaiter);
    exporter("__generator", __generator);
    exporter("__exportStar", __exportStar);
    exporter("__createBinding", __createBinding);
    exporter("__values", __values);
    exporter("__read", __read);
    exporter("__spread", __spread);
    exporter("__spreadArrays", __spreadArrays);
    exporter("__await", __await);
    exporter("__asyncGenerator", __asyncGenerator);
    exporter("__asyncDelegator", __asyncDelegator);
    exporter("__asyncValues", __asyncValues);
    exporter("__makeTemplateObject", __makeTemplateObject);
    exporter("__importStar", __importStar);
    exporter("__importDefault", __importDefault);
    exporter("__classPrivateFieldGet", __classPrivateFieldGet);
    exporter("__classPrivateFieldSet", __classPrivateFieldSet);
});

}).call(this)}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],157:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var tslib_1 = require("tslib");
var zenObservable_1 = require("./zenObservable");
tslib_1.__exportStar(require("./zenObservable"), exports);
exports.default = zenObservable_1.Observable;

},{"./zenObservable":158,"tslib":156}],158:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var tslib_1 = require("tslib");
var zen_observable_1 = tslib_1.__importDefault(require("zen-observable"));
exports.Observable = zen_observable_1.default;

},{"tslib":156,"zen-observable":159}],159:[function(require,module,exports){
module.exports = require('./lib/Observable.js').Observable;

},{"./lib/Observable.js":160}],160:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Observable = void 0;

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

// === Symbol Support ===
var hasSymbols = function () {
  return typeof Symbol === 'function';
};

var hasSymbol = function (name) {
  return hasSymbols() && Boolean(Symbol[name]);
};

var getSymbol = function (name) {
  return hasSymbol(name) ? Symbol[name] : '@@' + name;
};

if (hasSymbols() && !hasSymbol('observable')) {
  Symbol.observable = Symbol('observable');
}

var SymbolIterator = getSymbol('iterator');
var SymbolObservable = getSymbol('observable');
var SymbolSpecies = getSymbol('species'); // === Abstract Operations ===

function getMethod(obj, key) {
  var value = obj[key];
  if (value == null) return undefined;
  if (typeof value !== 'function') throw new TypeError(value + ' is not a function');
  return value;
}

function getSpecies(obj) {
  var ctor = obj.constructor;

  if (ctor !== undefined) {
    ctor = ctor[SymbolSpecies];

    if (ctor === null) {
      ctor = undefined;
    }
  }

  return ctor !== undefined ? ctor : Observable;
}

function isObservable(x) {
  return x instanceof Observable; // SPEC: Brand check
}

function hostReportError(e) {
  if (hostReportError.log) {
    hostReportError.log(e);
  } else {
    setTimeout(function () {
      throw e;
    });
  }
}

function enqueue(fn) {
  Promise.resolve().then(function () {
    try {
      fn();
    } catch (e) {
      hostReportError(e);
    }
  });
}

function cleanupSubscription(subscription) {
  var cleanup = subscription._cleanup;
  if (cleanup === undefined) return;
  subscription._cleanup = undefined;

  if (!cleanup) {
    return;
  }

  try {
    if (typeof cleanup === 'function') {
      cleanup();
    } else {
      var unsubscribe = getMethod(cleanup, 'unsubscribe');

      if (unsubscribe) {
        unsubscribe.call(cleanup);
      }
    }
  } catch (e) {
    hostReportError(e);
  }
}

function closeSubscription(subscription) {
  subscription._observer = undefined;
  subscription._queue = undefined;
  subscription._state = 'closed';
}

function flushSubscription(subscription) {
  var queue = subscription._queue;

  if (!queue) {
    return;
  }

  subscription._queue = undefined;
  subscription._state = 'ready';

  for (var i = 0; i < queue.length; ++i) {
    notifySubscription(subscription, queue[i].type, queue[i].value);
    if (subscription._state === 'closed') break;
  }
}

function notifySubscription(subscription, type, value) {
  subscription._state = 'running';
  var observer = subscription._observer;

  try {
    var m = getMethod(observer, type);

    switch (type) {
      case 'next':
        if (m) m.call(observer, value);
        break;

      case 'error':
        closeSubscription(subscription);
        if (m) m.call(observer, value);else throw value;
        break;

      case 'complete':
        closeSubscription(subscription);
        if (m) m.call(observer);
        break;
    }
  } catch (e) {
    hostReportError(e);
  }

  if (subscription._state === 'closed') cleanupSubscription(subscription);else if (subscription._state === 'running') subscription._state = 'ready';
}

function onNotify(subscription, type, value) {
  if (subscription._state === 'closed') return;

  if (subscription._state === 'buffering') {
    subscription._queue.push({
      type: type,
      value: value
    });

    return;
  }

  if (subscription._state !== 'ready') {
    subscription._state = 'buffering';
    subscription._queue = [{
      type: type,
      value: value
    }];
    enqueue(function () {
      return flushSubscription(subscription);
    });
    return;
  }

  notifySubscription(subscription, type, value);
}

var Subscription =
/*#__PURE__*/
function () {
  function Subscription(observer, subscriber) {
    _classCallCheck(this, Subscription);

    // ASSERT: observer is an object
    // ASSERT: subscriber is callable
    this._cleanup = undefined;
    this._observer = observer;
    this._queue = undefined;
    this._state = 'initializing';
    var subscriptionObserver = new SubscriptionObserver(this);

    try {
      this._cleanup = subscriber.call(undefined, subscriptionObserver);
    } catch (e) {
      subscriptionObserver.error(e);
    }

    if (this._state === 'initializing') this._state = 'ready';
  }

  _createClass(Subscription, [{
    key: "unsubscribe",
    value: function unsubscribe() {
      if (this._state !== 'closed') {
        closeSubscription(this);
        cleanupSubscription(this);
      }
    }
  }, {
    key: "closed",
    get: function () {
      return this._state === 'closed';
    }
  }]);

  return Subscription;
}();

var SubscriptionObserver =
/*#__PURE__*/
function () {
  function SubscriptionObserver(subscription) {
    _classCallCheck(this, SubscriptionObserver);

    this._subscription = subscription;
  }

  _createClass(SubscriptionObserver, [{
    key: "next",
    value: function next(value) {
      onNotify(this._subscription, 'next', value);
    }
  }, {
    key: "error",
    value: function error(value) {
      onNotify(this._subscription, 'error', value);
    }
  }, {
    key: "complete",
    value: function complete() {
      onNotify(this._subscription, 'complete');
    }
  }, {
    key: "closed",
    get: function () {
      return this._subscription._state === 'closed';
    }
  }]);

  return SubscriptionObserver;
}();

var Observable =
/*#__PURE__*/
function () {
  function Observable(subscriber) {
    _classCallCheck(this, Observable);

    if (!(this instanceof Observable)) throw new TypeError('Observable cannot be called as a function');
    if (typeof subscriber !== 'function') throw new TypeError('Observable initializer must be a function');
    this._subscriber = subscriber;
  }

  _createClass(Observable, [{
    key: "subscribe",
    value: function subscribe(observer) {
      if (typeof observer !== 'object' || observer === null) {
        observer = {
          next: observer,
          error: arguments[1],
          complete: arguments[2]
        };
      }

      return new Subscription(observer, this._subscriber);
    }
  }, {
    key: "forEach",
    value: function forEach(fn) {
      var _this = this;

      return new Promise(function (resolve, reject) {
        if (typeof fn !== 'function') {
          reject(new TypeError(fn + ' is not a function'));
          return;
        }

        function done() {
          subscription.unsubscribe();
          resolve();
        }

        var subscription = _this.subscribe({
          next: function (value) {
            try {
              fn(value, done);
            } catch (e) {
              reject(e);
              subscription.unsubscribe();
            }
          },
          error: reject,
          complete: resolve
        });
      });
    }
  }, {
    key: "map",
    value: function map(fn) {
      var _this2 = this;

      if (typeof fn !== 'function') throw new TypeError(fn + ' is not a function');
      var C = getSpecies(this);
      return new C(function (observer) {
        return _this2.subscribe({
          next: function (value) {
            try {
              value = fn(value);
            } catch (e) {
              return observer.error(e);
            }

            observer.next(value);
          },
          error: function (e) {
            observer.error(e);
          },
          complete: function () {
            observer.complete();
          }
        });
      });
    }
  }, {
    key: "filter",
    value: function filter(fn) {
      var _this3 = this;

      if (typeof fn !== 'function') throw new TypeError(fn + ' is not a function');
      var C = getSpecies(this);
      return new C(function (observer) {
        return _this3.subscribe({
          next: function (value) {
            try {
              if (!fn(value)) return;
            } catch (e) {
              return observer.error(e);
            }

            observer.next(value);
          },
          error: function (e) {
            observer.error(e);
          },
          complete: function () {
            observer.complete();
          }
        });
      });
    }
  }, {
    key: "reduce",
    value: function reduce(fn) {
      var _this4 = this;

      if (typeof fn !== 'function') throw new TypeError(fn + ' is not a function');
      var C = getSpecies(this);
      var hasSeed = arguments.length > 1;
      var hasValue = false;
      var seed = arguments[1];
      var acc = seed;
      return new C(function (observer) {
        return _this4.subscribe({
          next: function (value) {
            var first = !hasValue;
            hasValue = true;

            if (!first || hasSeed) {
              try {
                acc = fn(acc, value);
              } catch (e) {
                return observer.error(e);
              }
            } else {
              acc = value;
            }
          },
          error: function (e) {
            observer.error(e);
          },
          complete: function () {
            if (!hasValue && !hasSeed) return observer.error(new TypeError('Cannot reduce an empty sequence'));
            observer.next(acc);
            observer.complete();
          }
        });
      });
    }
  }, {
    key: "concat",
    value: function concat() {
      var _this5 = this;

      for (var _len = arguments.length, sources = new Array(_len), _key = 0; _key < _len; _key++) {
        sources[_key] = arguments[_key];
      }

      var C = getSpecies(this);
      return new C(function (observer) {
        var subscription;
        var index = 0;

        function startNext(next) {
          subscription = next.subscribe({
            next: function (v) {
              observer.next(v);
            },
            error: function (e) {
              observer.error(e);
            },
            complete: function () {
              if (index === sources.length) {
                subscription = undefined;
                observer.complete();
              } else {
                startNext(C.from(sources[index++]));
              }
            }
          });
        }

        startNext(_this5);
        return function () {
          if (subscription) {
            subscription.unsubscribe();
            subscription = undefined;
          }
        };
      });
    }
  }, {
    key: "flatMap",
    value: function flatMap(fn) {
      var _this6 = this;

      if (typeof fn !== 'function') throw new TypeError(fn + ' is not a function');
      var C = getSpecies(this);
      return new C(function (observer) {
        var subscriptions = [];

        var outer = _this6.subscribe({
          next: function (value) {
            if (fn) {
              try {
                value = fn(value);
              } catch (e) {
                return observer.error(e);
              }
            }

            var inner = C.from(value).subscribe({
              next: function (value) {
                observer.next(value);
              },
              error: function (e) {
                observer.error(e);
              },
              complete: function () {
                var i = subscriptions.indexOf(inner);
                if (i >= 0) subscriptions.splice(i, 1);
                completeIfDone();
              }
            });
            subscriptions.push(inner);
          },
          error: function (e) {
            observer.error(e);
          },
          complete: function () {
            completeIfDone();
          }
        });

        function completeIfDone() {
          if (outer.closed && subscriptions.length === 0) observer.complete();
        }

        return function () {
          subscriptions.forEach(function (s) {
            return s.unsubscribe();
          });
          outer.unsubscribe();
        };
      });
    }
  }, {
    key: SymbolObservable,
    value: function () {
      return this;
    }
  }], [{
    key: "from",
    value: function from(x) {
      var C = typeof this === 'function' ? this : Observable;
      if (x == null) throw new TypeError(x + ' is not an object');
      var method = getMethod(x, SymbolObservable);

      if (method) {
        var observable = method.call(x);
        if (Object(observable) !== observable) throw new TypeError(observable + ' is not an object');
        if (isObservable(observable) && observable.constructor === C) return observable;
        return new C(function (observer) {
          return observable.subscribe(observer);
        });
      }

      if (hasSymbol('iterator')) {
        method = getMethod(x, SymbolIterator);

        if (method) {
          return new C(function (observer) {
            enqueue(function () {
              if (observer.closed) return;
              var _iteratorNormalCompletion = true;
              var _didIteratorError = false;
              var _iteratorError = undefined;

              try {
                for (var _iterator = method.call(x)[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
                  var _item = _step.value;
                  observer.next(_item);
                  if (observer.closed) return;
                }
              } catch (err) {
                _didIteratorError = true;
                _iteratorError = err;
              } finally {
                try {
                  if (!_iteratorNormalCompletion && _iterator.return != null) {
                    _iterator.return();
                  }
                } finally {
                  if (_didIteratorError) {
                    throw _iteratorError;
                  }
                }
              }

              observer.complete();
            });
          });
        }
      }

      if (Array.isArray(x)) {
        return new C(function (observer) {
          enqueue(function () {
            if (observer.closed) return;

            for (var i = 0; i < x.length; ++i) {
              observer.next(x[i]);
              if (observer.closed) return;
            }

            observer.complete();
          });
        });
      }

      throw new TypeError(x + ' is not observable');
    }
  }, {
    key: "of",
    value: function of() {
      for (var _len2 = arguments.length, items = new Array(_len2), _key2 = 0; _key2 < _len2; _key2++) {
        items[_key2] = arguments[_key2];
      }

      var C = typeof this === 'function' ? this : Observable;
      return new C(function (observer) {
        enqueue(function () {
          if (observer.closed) return;

          for (var i = 0; i < items.length; ++i) {
            observer.next(items[i]);
            if (observer.closed) return;
          }

          observer.complete();
        });
      });
    }
  }, {
    key: SymbolSpecies,
    get: function () {
      return this;
    }
  }]);

  return Observable;
}();

exports.Observable = Observable;

if (hasSymbols()) {
  Object.defineProperty(Observable, Symbol('extensions'), {
    value: {
      symbol: SymbolObservable,
      hostReportError: hostReportError
    },
    configurable: true
  });
}
},{}],161:[function(require,module,exports){
var apolloClient = require("apollo-client")
var apolloLinkWS = require('apollo-link-ws')
var apolloCacheInMemory = require("apollo-cache-inmemory")
var gql = require('graphql-tag')
var GRAPHQL_URI = 'localhost:8080/ws/graphql';

//require("material-components-web-elm/dist/material-components-web-elm.js"); // from aforemny/material-components-web-elm
//require("material-components-web-elm/dist/material-components-web-elm.css");

const getClient = () => {
    var wsLink = new apolloLinkWS.WebSocketLink({
        uri: `ws://${GRAPHQL_URI}`,
        options: {
            reconnect: true,
        }
    });
    var client = new apolloClient.ApolloClient({
        link: wsLink,
        cache: new apolloCacheInMemory.InMemoryCache({
            addTypename: true
        })
    });
    return client;
};

document.addEventListener("DOMContentLoaded", function() {
    var app = Elm.Main.init({node: document.getElementById('main')})
    var client = getClient();

     app.ports.joinGame.subscribe(function(data) {
       /* Initiate subscription request */
        client.subscribe({
           query: gql`${data}`,
           variables: {}
         }).subscribe({
           next(resp) {
           var resp2 = { data: resp.data[Object.keys(resp.data)[0]] }
             app.ports.receivedGameEvent.send(resp2);
           },
           error(err) {
             console.log(err);
           }
         });
     });
//
//      app.ports.createTestSubscription.subscribe(function(data) {
//        /* Initiate subscription request */
//         getClient().subscribe({
//            query: gql`${data}`,
//            variables: {}
//          }).subscribe({
//            next(resp) {
//              console.log(resp)
//              var data = resp['data'][Object.keys(resp['data'])[0]];
//              console.log(data)
//              app.ports.receivedTestSubscriptionData.send(data);
//            },
//            error(err) {
//              console.log(err);
//            }
//          });
//      });
})
},{"apollo-cache-inmemory":3,"apollo-client":5,"apollo-link-ws":6,"graphql-tag":16}]},{},[161]);
