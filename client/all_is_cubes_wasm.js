import { makeAllStaticGuiHelpers } from './snippets/all-is-cubes-wasm-ed7534c6cefcab5e/src/js/gui.js';

let wasm;

const heap = new Array(128).fill(undefined);

heap.push(undefined, null, true, false);

function getObject(idx) { return heap[idx]; }

let heap_next = heap.length;

function addHeapObject(obj) {
    if (heap_next === heap.length) heap.push(heap.length + 1);
    const idx = heap_next;
    heap_next = heap[idx];

    if (typeof(heap_next) !== 'number') throw new Error('corrupt heap');

    heap[idx] = obj;
    return idx;
}

function dropObject(idx) {
    if (idx < 132) return;
    heap[idx] = heap_next;
    heap_next = idx;
}

function takeObject(idx) {
    const ret = getObject(idx);
    dropObject(idx);
    return ret;
}

const cachedTextDecoder = (typeof TextDecoder !== 'undefined' ? new TextDecoder('utf-8', { ignoreBOM: true, fatal: true }) : { decode: () => { throw Error('TextDecoder not available') } } );

if (typeof TextDecoder !== 'undefined') { cachedTextDecoder.decode(); };

let cachedUint8Memory0 = null;

function getUint8Memory0() {
    if (cachedUint8Memory0 === null || cachedUint8Memory0.byteLength === 0) {
        cachedUint8Memory0 = new Uint8Array(wasm.memory.buffer);
    }
    return cachedUint8Memory0;
}

function getStringFromWasm0(ptr, len) {
    ptr = ptr >>> 0;
    return cachedTextDecoder.decode(getUint8Memory0().subarray(ptr, ptr + len));
}

function _assertBoolean(n) {
    if (typeof(n) !== 'boolean') {
        throw new Error(`expected a boolean argument, found ${typeof(n)}`);
    }
}

let WASM_VECTOR_LEN = 0;

const cachedTextEncoder = (typeof TextEncoder !== 'undefined' ? new TextEncoder('utf-8') : { encode: () => { throw Error('TextEncoder not available') } } );

const encodeString = (typeof cachedTextEncoder.encodeInto === 'function'
    ? function (arg, view) {
    return cachedTextEncoder.encodeInto(arg, view);
}
    : function (arg, view) {
    const buf = cachedTextEncoder.encode(arg);
    view.set(buf);
    return {
        read: arg.length,
        written: buf.length
    };
});

function passStringToWasm0(arg, malloc, realloc) {

    if (typeof(arg) !== 'string') throw new Error(`expected a string argument, found ${typeof(arg)}`);

    if (realloc === undefined) {
        const buf = cachedTextEncoder.encode(arg);
        const ptr = malloc(buf.length, 1) >>> 0;
        getUint8Memory0().subarray(ptr, ptr + buf.length).set(buf);
        WASM_VECTOR_LEN = buf.length;
        return ptr;
    }

    let len = arg.length;
    let ptr = malloc(len, 1) >>> 0;

    const mem = getUint8Memory0();

    let offset = 0;

    for (; offset < len; offset++) {
        const code = arg.charCodeAt(offset);
        if (code > 0x7F) break;
        mem[ptr + offset] = code;
    }

    if (offset !== len) {
        if (offset !== 0) {
            arg = arg.slice(offset);
        }
        ptr = realloc(ptr, len, len = offset + arg.length * 3, 1) >>> 0;
        const view = getUint8Memory0().subarray(ptr + offset, ptr + len);
        const ret = encodeString(arg, view);
        if (ret.read !== arg.length) throw new Error('failed to pass whole string');
        offset += ret.written;
        ptr = realloc(ptr, len, offset, 1) >>> 0;
    }

    WASM_VECTOR_LEN = offset;
    return ptr;
}

function isLikeNone(x) {
    return x === undefined || x === null;
}

let cachedInt32Memory0 = null;

function getInt32Memory0() {
    if (cachedInt32Memory0 === null || cachedInt32Memory0.byteLength === 0) {
        cachedInt32Memory0 = new Int32Array(wasm.memory.buffer);
    }
    return cachedInt32Memory0;
}

function _assertNum(n) {
    if (typeof(n) !== 'number') throw new Error(`expected a number argument, found ${typeof(n)}`);
}

let cachedFloat64Memory0 = null;

function getFloat64Memory0() {
    if (cachedFloat64Memory0 === null || cachedFloat64Memory0.byteLength === 0) {
        cachedFloat64Memory0 = new Float64Array(wasm.memory.buffer);
    }
    return cachedFloat64Memory0;
}

function debugString(val) {
    // primitive types
    const type = typeof val;
    if (type == 'number' || type == 'boolean' || val == null) {
        return  `${val}`;
    }
    if (type == 'string') {
        return `"${val}"`;
    }
    if (type == 'symbol') {
        const description = val.description;
        if (description == null) {
            return 'Symbol';
        } else {
            return `Symbol(${description})`;
        }
    }
    if (type == 'function') {
        const name = val.name;
        if (typeof name == 'string' && name.length > 0) {
            return `Function(${name})`;
        } else {
            return 'Function';
        }
    }
    // objects
    if (Array.isArray(val)) {
        const length = val.length;
        let debug = '[';
        if (length > 0) {
            debug += debugString(val[0]);
        }
        for(let i = 1; i < length; i++) {
            debug += ', ' + debugString(val[i]);
        }
        debug += ']';
        return debug;
    }
    // Test for built-in
    const builtInMatches = /\[object ([^\]]+)\]/.exec(toString.call(val));
    let className;
    if (builtInMatches.length > 1) {
        className = builtInMatches[1];
    } else {
        // Failed to match the standard '[object ClassName]'
        return toString.call(val);
    }
    if (className == 'Object') {
        // we're a user defined class or Object
        // JSON.stringify avoids problems with cycles, and is generally much
        // easier than looping through ownProperties of `val`.
        try {
            return 'Object(' + JSON.stringify(val) + ')';
        } catch (_) {
            return 'Object';
        }
    }
    // errors
    if (val instanceof Error) {
        return `${val.name}: ${val.message}\n${val.stack}`;
    }
    // TODO we could test for more things here, like `Set`s and `Map`s.
    return className;
}

const CLOSURE_DTORS = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(state => {
    wasm.__wbindgen_export_2.get(state.dtor)(state.a, state.b)
});

function makeClosure(arg0, arg1, dtor, f) {
    const state = { a: arg0, b: arg1, cnt: 1, dtor };
    const real = (...args) => {
        // First up with a closure we increment the internal reference
        // count. This ensures that the Rust closure environment won't
        // be deallocated while we're invoking it.
        state.cnt++;
        try {
            return f(state.a, state.b, ...args);
        } finally {
            if (--state.cnt === 0) {
                wasm.__wbindgen_export_2.get(state.dtor)(state.a, state.b);
                state.a = 0;
                CLOSURE_DTORS.unregister(state);
            }
        }
    };
    real.original = state;
    CLOSURE_DTORS.register(real, state, state);
    return real;
}

function logError(f, args) {
    try {
        return f.apply(this, args);
    } catch (e) {
        let error = (function () {
            try {
                return e instanceof Error ? `${e.message}\n\nStack:\n${e.stack}` : e.toString();
            } catch(_) {
                return "<failed to stringify thrown value>";
            }
        }());
        console.error("wasm-bindgen: imported JS function that was not marked as `catch` threw an error:", error);
        throw e;
    }
}
function __wbg_adapter_32(arg0, arg1, arg2) {
    _assertNum(arg0);
    _assertNum(arg1);
    wasm.wasm_bindgen__convert__closures__invoke1__h72e47fc1edd66b29(arg0, arg1, addHeapObject(arg2));
}

function makeMutClosure(arg0, arg1, dtor, f) {
    const state = { a: arg0, b: arg1, cnt: 1, dtor };
    const real = (...args) => {
        // First up with a closure we increment the internal reference
        // count. This ensures that the Rust closure environment won't
        // be deallocated while we're invoking it.
        state.cnt++;
        const a = state.a;
        state.a = 0;
        try {
            return f(a, state.b, ...args);
        } finally {
            if (--state.cnt === 0) {
                wasm.__wbindgen_export_2.get(state.dtor)(a, state.b);
                CLOSURE_DTORS.unregister(state);
            } else {
                state.a = a;
            }
        }
    };
    real.original = state;
    CLOSURE_DTORS.register(real, state, state);
    return real;
}
function __wbg_adapter_35(arg0, arg1, arg2) {
    _assertNum(arg0);
    _assertNum(arg1);
    wasm._dyn_core__ops__function__FnMut__A____Output___R_as_wasm_bindgen__closure__WasmClosure___describe__invoke__he8e26dda3e320a4e(arg0, arg1, arg2);
}

function __wbg_adapter_38(arg0, arg1) {
    _assertNum(arg0);
    _assertNum(arg1);
    wasm._dyn_core__ops__function__FnMut_____Output___R_as_wasm_bindgen__closure__WasmClosure___describe__invoke__h0e508a2a12b9c91b(arg0, arg1);
}

function __wbg_adapter_41(arg0, arg1, arg2) {
    _assertNum(arg0);
    _assertNum(arg1);
    wasm._dyn_core__ops__function__FnMut__A____Output___R_as_wasm_bindgen__closure__WasmClosure___describe__invoke__hb8553dbee0eecbf5(arg0, arg1, addHeapObject(arg2));
}

function __wbg_adapter_46(arg0, arg1, arg2) {
    _assertNum(arg0);
    _assertNum(arg1);
    wasm._dyn_core__ops__function__FnMut__A____Output___R_as_wasm_bindgen__closure__WasmClosure___describe__invoke__hfffd1990a7e213f8(arg0, arg1, addHeapObject(arg2));
}

/**
* Entry point for normal game-in-a-web-page operation.
* @returns {Promise<void>}
*/
export function start_game() {
    const ret = wasm.start_game();
    return takeObject(ret);
}

function passArrayF64ToWasm0(arg, malloc) {
    const ptr = malloc(arg.length * 8, 8) >>> 0;
    getFloat64Memory0().set(arg, ptr / 8);
    WASM_VECTOR_LEN = arg.length;
    return ptr;
}

let cachedUint32Memory0 = null;

function getUint32Memory0() {
    if (cachedUint32Memory0 === null || cachedUint32Memory0.byteLength === 0) {
        cachedUint32Memory0 = new Uint32Array(wasm.memory.buffer);
    }
    return cachedUint32Memory0;
}

function getArrayU32FromWasm0(ptr, len) {
    ptr = ptr >>> 0;
    return getUint32Memory0().subarray(ptr / 4, ptr / 4 + len);
}

function getArrayI32FromWasm0(ptr, len) {
    ptr = ptr >>> 0;
    return getInt32Memory0().subarray(ptr / 4, ptr / 4 + len);
}

function handleError(f, args) {
    try {
        return f.apply(this, args);
    } catch (e) {
        wasm.__wbindgen_exn_store(addHeapObject(e));
    }
}

let cachedFloat32Memory0 = null;

function getFloat32Memory0() {
    if (cachedFloat32Memory0 === null || cachedFloat32Memory0.byteLength === 0) {
        cachedFloat32Memory0 = new Float32Array(wasm.memory.buffer);
    }
    return cachedFloat32Memory0;
}

function getArrayF32FromWasm0(ptr, len) {
    ptr = ptr >>> 0;
    return getFloat32Memory0().subarray(ptr / 4, ptr / 4 + len);
}
function __wbg_adapter_1014(arg0, arg1, arg2, arg3) {
    _assertNum(arg0);
    _assertNum(arg1);
    wasm.wasm_bindgen__convert__closures__invoke2_mut__h977e3017ac4bf5d8(arg0, arg1, addHeapObject(arg2), addHeapObject(arg3));
}

async function __wbg_load(module, imports) {
    if (typeof Response === 'function' && module instanceof Response) {
        if (typeof WebAssembly.instantiateStreaming === 'function') {
            try {
                return await WebAssembly.instantiateStreaming(module, imports);

            } catch (e) {
                if (module.headers.get('Content-Type') != 'application/wasm') {
                    console.warn("`WebAssembly.instantiateStreaming` failed because your server does not serve wasm with `application/wasm` MIME type. Falling back to `WebAssembly.instantiate` which is slower. Original error:\n", e);

                } else {
                    throw e;
                }
            }
        }

        const bytes = await module.arrayBuffer();
        return await WebAssembly.instantiate(bytes, imports);

    } else {
        const instance = await WebAssembly.instantiate(module, imports);

        if (instance instanceof WebAssembly.Instance) {
            return { instance, module };

        } else {
            return instance;
        }
    }
}

function __wbg_get_imports() {
    const imports = {};
    imports.wbg = {};
    imports.wbg.__wbindgen_object_clone_ref = function(arg0) {
        const ret = getObject(arg0);
        return addHeapObject(ret);
    };
    imports.wbg.__wbg_makeAllStaticGuiHelpers_1e532dd1718eff6f = function() { return logError(function (arg0, arg1) {
        const ret = makeAllStaticGuiHelpers(takeObject(arg0), takeObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbindgen_object_drop_ref = function(arg0) {
        takeObject(arg0);
    };
    imports.wbg.__wbindgen_string_new = function(arg0, arg1) {
        const ret = getStringFromWasm0(arg0, arg1);
        return addHeapObject(ret);
    };
    imports.wbg.__wbg_setFullscreen_5b77b0e8989ad1af = function() { return logError(function (arg0, arg1) {
        getObject(arg0).setFullscreen(arg1 !== 0);
    }, arguments) };
    imports.wbg.__wbg_canvasHelper_14331da732a26538 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).canvasHelper;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_canvas_d4d764f77faa8d13 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).canvas;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbindgen_cb_drop = function(arg0) {
        const obj = takeObject(arg0).original;
        if (obj.cnt-- == 1) {
            obj.a = 0;
            return true;
        }
        const ret = false;
        _assertBoolean(ret);
        return ret;
    };
    imports.wbg.__wbg_isFullscreen_facf83adf3f23575 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).isFullscreen();
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbindgen_is_function = function(arg0) {
        const ret = typeof(getObject(arg0)) === 'function';
        _assertBoolean(ret);
        return ret;
    };
    imports.wbg.__wbg_viewportPx_559f48b3291379ec = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg1).viewportPx;
        const ptr1 = passArrayF64ToWasm0(ret, wasm.__wbindgen_malloc);
        const len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    }, arguments) };
    imports.wbg.__wbg_viewportDev_2117187582cf1797 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg1).viewportDev;
        const ptr1 = passArrayF64ToWasm0(ret, wasm.__wbindgen_malloc);
        const len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    }, arguments) };
    imports.wbg.__wbg_new_abda76e883ba8a5f = function() { return logError(function () {
        const ret = new Error();
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_stack_658279fe44541cf6 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg1).stack;
        const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    }, arguments) };
    imports.wbg.__wbg_error_f851667af71bcfc6 = function() { return logError(function (arg0, arg1) {
        let deferred0_0;
        let deferred0_1;
        try {
            deferred0_0 = arg0;
            deferred0_1 = arg1;
            console.error(getStringFromWasm0(arg0, arg1));
        } finally {
            wasm.__wbindgen_free(deferred0_0, deferred0_1, 1);
        }
    }, arguments) };
    imports.wbg.__wbg_now_abd80e969af37148 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).now();
        return ret;
    }, arguments) };
    imports.wbg.__wbg_performance_a1b8bde2ee512264 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).performance;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbindgen_is_undefined = function(arg0) {
        const ret = getObject(arg0) === undefined;
        _assertBoolean(ret);
        return ret;
    };
    imports.wbg.__wbg_instanceof_GpuAdapter_76bb05881d5f91d1 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof GPUAdapter;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbindgen_number_new = function(arg0) {
        const ret = arg0;
        return addHeapObject(ret);
    };
    imports.wbg.__wbindgen_string_get = function(arg0, arg1) {
        const obj = getObject(arg1);
        const ret = typeof(obj) === 'string' ? obj : undefined;
        var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    };
    imports.wbg.__wbg_copyExternalImageToTexture_5389ee5babf9d86f = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).copyExternalImageToTexture(getObject(arg1), getObject(arg2), getObject(arg3));
    }, arguments) };
    imports.wbg.__wbg_submit_c512d9a4b5ff838d = function() { return logError(function (arg0, arg1) {
        getObject(arg0).submit(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_writeBuffer_b225dafa1a52c298 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).writeBuffer(getObject(arg1), arg2, getObject(arg3), arg4, arg5);
    }, arguments) };
    imports.wbg.__wbg_writeTexture_05b125d21ce9740e = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).writeTexture(getObject(arg1), getObject(arg2), getObject(arg3), getObject(arg4));
    }, arguments) };
    imports.wbg.__wbg_has_d655f3a252d0b10a = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).has(getStringFromWasm0(arg1, arg2));
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxTextureDimension1D_53351b4a7253c324 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxTextureDimension1D;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxTextureDimension2D_26995ffa94733f82 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxTextureDimension2D;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxTextureDimension3D_8d77c6d768caef58 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxTextureDimension3D;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxTextureArrayLayers_cbf7e90284df66c3 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxTextureArrayLayers;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxBindGroups_54fa38a646718d85 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxBindGroups;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxBindingsPerBindGroup_e8f7a2792b9ac107 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxBindingsPerBindGroup;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxDynamicUniformBuffersPerPipelineLayout_7c5942f359a6fb1b = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxDynamicUniformBuffersPerPipelineLayout;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxDynamicStorageBuffersPerPipelineLayout_bd22a382d13e6ef5 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxDynamicStorageBuffersPerPipelineLayout;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxSampledTexturesPerShaderStage_5704d5ff400bceee = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxSampledTexturesPerShaderStage;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxSamplersPerShaderStage_5e8845f07c33913a = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxSamplersPerShaderStage;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxStorageBuffersPerShaderStage_18a674788ed5fdad = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxStorageBuffersPerShaderStage;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxStorageTexturesPerShaderStage_bfff5cb8d91bcfcc = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxStorageTexturesPerShaderStage;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxUniformBuffersPerShaderStage_ef06df9be2943d45 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxUniformBuffersPerShaderStage;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxUniformBufferBindingSize_f84670235a7e5df9 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxUniformBufferBindingSize;
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxStorageBufferBindingSize_9245cd89c719dbf2 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxStorageBufferBindingSize;
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxVertexBuffers_73da155813feea78 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxVertexBuffers;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxBufferSize_7087869d4548c87d = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxBufferSize;
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxVertexAttributes_3a0ea01143239608 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxVertexAttributes;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxVertexBufferArrayStride_d699c03944dd52d9 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxVertexBufferArrayStride;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_minUniformBufferOffsetAlignment_5574ef5e4f6d62da = function() { return logError(function (arg0) {
        const ret = getObject(arg0).minUniformBufferOffsetAlignment;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_minStorageBufferOffsetAlignment_a6666e346184b953 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).minStorageBufferOffsetAlignment;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxInterStageShaderComponents_09be6edd346cb8da = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxInterStageShaderComponents;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxComputeWorkgroupStorageSize_58415be93e502f25 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxComputeWorkgroupStorageSize;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxComputeInvocationsPerWorkgroup_8aa2f0a5861ce5ef = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxComputeInvocationsPerWorkgroup;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxComputeWorkgroupSizeX_789174905500f6c7 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxComputeWorkgroupSizeX;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxComputeWorkgroupSizeY_926ec1c24c6136da = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxComputeWorkgroupSizeY;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxComputeWorkgroupSizeZ_562c888ae9402be1 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxComputeWorkgroupSizeZ;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_maxComputeWorkgroupsPerDimension_07fa50cdca40e120 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).maxComputeWorkgroupsPerDimension;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_queue_9f8d8658085c6f43 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).queue;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbindgen_is_object = function(arg0) {
        const val = getObject(arg0);
        const ret = typeof(val) === 'object' && val !== null;
        _assertBoolean(ret);
        return ret;
    };
    imports.wbg.__wbg_instanceof_GpuCanvasContext_05351086956f1883 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof GPUCanvasContext;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_getMappedRange_8229b08f744819c0 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).getMappedRange(arg1, arg2);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_Window_a1459b9c171b6eed = function() { return logError(function (arg0) {
        const ret = getObject(arg0).Window;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_WorkerGlobalScope_e1b8bcefd2818e94 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).WorkerGlobalScope;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_gpu_4ac835f782ad971d = function() { return logError(function (arg0) {
        const ret = getObject(arg0).gpu;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_requestAdapter_913357b9788f14cd = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).requestAdapter(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_requestDevice_baf0b46015a90431 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).requestDevice(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_features_7fd6ee02e18d77a4 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).features;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_limits_7c1e17ce28ddf954 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).limits;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getPreferredCanvasFormat_c57006806f2efe1b = function() { return logError(function (arg0) {
        const ret = getObject(arg0).getPreferredCanvasFormat();
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_configure_8ae8b7e66a9d6189 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).configure(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_getCurrentTexture_26a07297d850dcb1 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).getCurrentTexture();
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_features_01f848ca4efe700b = function() { return logError(function (arg0) {
        const ret = getObject(arg0).features;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_limits_cf6e9ab92d696f0c = function() { return logError(function (arg0) {
        const ret = getObject(arg0).limits;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createShaderModule_6851cf2067c2f947 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createShaderModule(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createBindGroupLayout_6adcd872318d899a = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createBindGroupLayout(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createBindGroup_5ac37963cb812b24 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createBindGroup(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createPipelineLayout_2648fbc756354294 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createPipelineLayout(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createRenderPipeline_513576fa326b8ccf = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createRenderPipeline(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createComputePipeline_957ea1dbcd97e6de = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createComputePipeline(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createBuffer_90ac080c7cc1375d = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createBuffer(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createTexture_4297303d703376ef = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createTexture(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createSampler_e56450d56435986f = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createSampler(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createQuerySet_c6b5390470139efb = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createQuerySet(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createCommandEncoder_9ee63be2a93c77dd = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createCommandEncoder(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createRenderBundleEncoder_bbce060a45e55caf = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createRenderBundleEncoder(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_destroy_6e1daab7792230a0 = function() { return logError(function (arg0) {
        getObject(arg0).destroy();
    }, arguments) };
    imports.wbg.__wbg_setonuncapturederror_0901d4d8bff41810 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).onuncapturederror = getObject(arg1);
    }, arguments) };
    imports.wbg.__wbg_error_7ced2e8034eb1f3f = function() { return logError(function (arg0) {
        const ret = getObject(arg0).error;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_pushErrorScope_d39727ef0414ac9f = function() { return logError(function (arg0, arg1) {
        getObject(arg0).pushErrorScope(takeObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_popErrorScope_1d998d85c7b134be = function() { return logError(function (arg0) {
        const ret = getObject(arg0).popErrorScope();
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_mapAsync_7d9fc5c22fb1f55e = function() { return logError(function (arg0, arg1, arg2, arg3) {
        const ret = getObject(arg0).mapAsync(arg1 >>> 0, arg2, arg3);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_unmap_abe29e47be94736f = function() { return logError(function (arg0) {
        getObject(arg0).unmap();
    }, arguments) };
    imports.wbg.__wbg_createView_8463cbef5f0c4d5c = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createView(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_destroy_b8ea7d8b8cee78c4 = function() { return logError(function (arg0) {
        getObject(arg0).destroy();
    }, arguments) };
    imports.wbg.__wbg_destroy_7fe69567d342b339 = function() { return logError(function (arg0) {
        getObject(arg0).destroy();
    }, arguments) };
    imports.wbg.__wbg_getBindGroupLayout_255eaa69c120a995 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).getBindGroupLayout(arg1 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getBindGroupLayout_d573a4d2adfb5ae8 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).getBindGroupLayout(arg1 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_copyBufferToBuffer_0a44e23b31a7ca5a = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).copyBufferToBuffer(getObject(arg1), arg2, getObject(arg3), arg4, arg5);
    }, arguments) };
    imports.wbg.__wbg_copyBufferToTexture_de6f3cd9ac87a870 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).copyBufferToTexture(getObject(arg1), getObject(arg2), getObject(arg3));
    }, arguments) };
    imports.wbg.__wbg_copyTextureToBuffer_7ab49ff0dd12cd22 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).copyTextureToBuffer(getObject(arg1), getObject(arg2), getObject(arg3));
    }, arguments) };
    imports.wbg.__wbg_copyTextureToTexture_45800f5fb0aaaf6c = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).copyTextureToTexture(getObject(arg1), getObject(arg2), getObject(arg3));
    }, arguments) };
    imports.wbg.__wbg_beginComputePass_99e2aa27fb960fa5 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).beginComputePass(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_end_a895c7d0f47bb8e0 = function() { return logError(function (arg0) {
        getObject(arg0).end();
    }, arguments) };
    imports.wbg.__wbg_beginRenderPass_b4c178a1fd787b5c = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).beginRenderPass(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_end_0fafe47bdc78c53d = function() { return logError(function (arg0) {
        getObject(arg0).end();
    }, arguments) };
    imports.wbg.__wbg_label_4956528ad99b1650 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg1).label;
        const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    }, arguments) };
    imports.wbg.__wbg_finish_cbd8e5d52fe81fd6 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).finish(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_finish_3cd844105a9de3e9 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).finish();
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_clearBuffer_50e1d3d029849fdb = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).clearBuffer(getObject(arg1), arg2);
    }, arguments) };
    imports.wbg.__wbg_clearBuffer_157bab025583c473 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).clearBuffer(getObject(arg1), arg2, arg3);
    }, arguments) };
    imports.wbg.__wbg_writeTimestamp_70875f22e698e86b = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).writeTimestamp(getObject(arg1), arg2 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_resolveQuerySet_8f696a33e8da099f = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).resolveQuerySet(getObject(arg1), arg2 >>> 0, arg3 >>> 0, getObject(arg4), arg5 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_finish_806df42c71c712c3 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).finish();
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_finish_55ef253db8a2e02a = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).finish(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_usage_2e5ff7c87b5e9737 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).usage;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_size_7838da1244dcc49f = function() { return logError(function (arg0) {
        const ret = getObject(arg0).size;
        return ret;
    }, arguments) };
    imports.wbg.__wbg_setPipeline_9730cb37968bb3d1 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).setPipeline(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_setBindGroup_c11c5cfe30b7ec4a = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).setBindGroup(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_setBindGroup_0184ac17323d75b2 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6) {
        getObject(arg0).setBindGroup(arg1 >>> 0, getObject(arg2), getArrayU32FromWasm0(arg3, arg4), arg5, arg6 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_dispatchWorkgroups_2190ad793cd27850 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).dispatchWorkgroups(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_dispatchWorkgroupsIndirect_cfc6272439398a21 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).dispatchWorkgroupsIndirect(getObject(arg1), arg2);
    }, arguments) };
    imports.wbg.__wbg_setPipeline_b1e4ff4a2d89b8aa = function() { return logError(function (arg0, arg1) {
        getObject(arg0).setPipeline(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_setBindGroup_2054136f79b0fed9 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).setBindGroup(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_setBindGroup_7908d39626c7bcc5 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6) {
        getObject(arg0).setBindGroup(arg1 >>> 0, getObject(arg2), getArrayU32FromWasm0(arg3, arg4), arg5, arg6 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_setIndexBuffer_4deca629ec05a510 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).setIndexBuffer(getObject(arg1), takeObject(arg2), arg3);
    }, arguments) };
    imports.wbg.__wbg_setIndexBuffer_ea5677e397c8df89 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).setIndexBuffer(getObject(arg1), takeObject(arg2), arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_setVertexBuffer_4c924a9cc335e437 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).setVertexBuffer(arg1 >>> 0, getObject(arg2), arg3);
    }, arguments) };
    imports.wbg.__wbg_setVertexBuffer_0aca41ad007e04fc = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).setVertexBuffer(arg1 >>> 0, getObject(arg2), arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_draw_2ea14b17b7ad7b86 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).draw(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0, arg4 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_drawIndexed_81f7662bc9f8bda1 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).drawIndexed(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0, arg4, arg5 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_drawIndirect_3de3a4df802f8f74 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).drawIndirect(getObject(arg1), arg2);
    }, arguments) };
    imports.wbg.__wbg_drawIndexedIndirect_74e31bc5d14e7aab = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).drawIndexedIndirect(getObject(arg1), arg2);
    }, arguments) };
    imports.wbg.__wbg_setPipeline_d3556629635bf281 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).setPipeline(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_setBindGroup_4147d4ebb7213bb3 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).setBindGroup(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_setBindGroup_96a4847ff3077350 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6) {
        getObject(arg0).setBindGroup(arg1 >>> 0, getObject(arg2), getArrayU32FromWasm0(arg3, arg4), arg5, arg6 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_setIndexBuffer_1860608e395ec140 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).setIndexBuffer(getObject(arg1), takeObject(arg2), arg3);
    }, arguments) };
    imports.wbg.__wbg_setIndexBuffer_83f311a5a378a545 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).setIndexBuffer(getObject(arg1), takeObject(arg2), arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_setVertexBuffer_d439a224a2369412 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).setVertexBuffer(arg1 >>> 0, getObject(arg2), arg3);
    }, arguments) };
    imports.wbg.__wbg_setVertexBuffer_0dca9fc7421bd152 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).setVertexBuffer(arg1 >>> 0, getObject(arg2), arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_draw_7266fe228aea02a8 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).draw(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0, arg4 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_drawIndexed_23bcd62668716ed0 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).drawIndexed(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0, arg4, arg5 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_drawIndirect_1a15176b1b8537ff = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).drawIndirect(getObject(arg1), arg2);
    }, arguments) };
    imports.wbg.__wbg_drawIndexedIndirect_6f3721f18ad10b1e = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).drawIndexedIndirect(getObject(arg1), arg2);
    }, arguments) };
    imports.wbg.__wbg_setBlendConstant_a946e294911337e9 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).setBlendConstant(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_setScissorRect_cd8f44130fd71416 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).setScissorRect(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0, arg4 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_setViewport_66dfe2ad99a0ccd6 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6) {
        getObject(arg0).setViewport(arg1, arg2, arg3, arg4, arg5, arg6);
    }, arguments) };
    imports.wbg.__wbg_setStencilReference_08db4d5601a3f285 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).setStencilReference(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_executeBundles_4bcd6c8ecfaedf51 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).executeBundles(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_message_867097f776344069 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg1).message;
        const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    }, arguments) };
    imports.wbg.__wbg_instanceof_GpuValidationError_3128431f7a0514f4 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof GPUValidationError;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_instanceof_GpuOutOfMemoryError_b37a08bfb7cee038 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof GPUOutOfMemoryError;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_queueMicrotask_3cbae2ec6b6cd3d6 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).queueMicrotask;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_queueMicrotask_481971b0d87f3dd4 = function() { return logError(function (arg0) {
        queueMicrotask(getObject(arg0));
    }, arguments) };
    imports.wbg.__wbindgen_boolean_get = function(arg0) {
        const v = getObject(arg0);
        const ret = typeof(v) === 'boolean' ? (v ? 1 : 0) : 2;
        _assertNum(ret);
        return ret;
    };
    imports.wbg.__wbindgen_number_get = function(arg0, arg1) {
        const obj = getObject(arg1);
        const ret = typeof(obj) === 'number' ? obj : undefined;
        if (!isLikeNone(ret)) {
            _assertNum(ret);
        }
        getFloat64Memory0()[arg0 / 8 + 1] = isLikeNone(ret) ? 0 : ret;
        getInt32Memory0()[arg0 / 4 + 0] = !isLikeNone(ret);
    };
    imports.wbg.__wbg_now_e0d8ec93dd25766a = function() { return logError(function (arg0) {
        const ret = getObject(arg0).now();
        return ret;
    }, arguments) };
    imports.wbg.__wbg_performance_eeefc685c9bc38b4 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).performance;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_instanceof_WebGl2RenderingContext_6b8f92d566ced9e1 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof WebGL2RenderingContext;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_beginQuery_3d6bb95151ccc499 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).beginQuery(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_bindBufferRange_e7b7d4cd65a6f94d = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).bindBufferRange(arg1 >>> 0, arg2 >>> 0, getObject(arg3), arg4, arg5);
    }, arguments) };
    imports.wbg.__wbg_bindSampler_065f0bdf49888ff1 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).bindSampler(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_bindVertexArray_239574d42dbbd203 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).bindVertexArray(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_blitFramebuffer_4d77c70dcb183e0c = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) {
        getObject(arg0).blitFramebuffer(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 >>> 0, arg10 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_bufferData_194f0914aaada840 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).bufferData(arg1 >>> 0, arg2, arg3 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_bufferData_c787516945ba48c2 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).bufferData(arg1 >>> 0, getObject(arg2), arg3 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_bufferSubData_7f5ddd4fdc628963 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).bufferSubData(arg1 >>> 0, arg2, getObject(arg3));
    }, arguments) };
    imports.wbg.__wbg_clearBufferiv_519fe97abe38622e = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).clearBufferiv(arg1 >>> 0, arg2, getArrayI32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_clearBufferuiv_1ae6df4bc96ffe37 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).clearBufferuiv(arg1 >>> 0, arg2, getArrayU32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_clientWaitSync_8f9f625ae9a42de6 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        const ret = getObject(arg0).clientWaitSync(getObject(arg1), arg2 >>> 0, arg3 >>> 0);
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_compressedTexSubImage2D_f77856eab95e8671 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) {
        getObject(arg0).compressedTexSubImage2D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7 >>> 0, arg8, arg9);
    }, arguments) };
    imports.wbg.__wbg_compressedTexSubImage2D_87d89d4b3f413805 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) {
        getObject(arg0).compressedTexSubImage2D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7 >>> 0, getObject(arg8));
    }, arguments) };
    imports.wbg.__wbg_compressedTexSubImage3D_b69e67d3cd62b756 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) {
        getObject(arg0).compressedTexSubImage3D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 >>> 0, arg10, arg11);
    }, arguments) };
    imports.wbg.__wbg_compressedTexSubImage3D_ff8eceb18a7ea2d6 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) {
        getObject(arg0).compressedTexSubImage3D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 >>> 0, getObject(arg10));
    }, arguments) };
    imports.wbg.__wbg_copyBufferSubData_db2c040cc06be689 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).copyBufferSubData(arg1 >>> 0, arg2 >>> 0, arg3, arg4, arg5);
    }, arguments) };
    imports.wbg.__wbg_copyTexSubImage3D_0a3f60d0ee6409c7 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) {
        getObject(arg0).copyTexSubImage3D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
    }, arguments) };
    imports.wbg.__wbg_createQuery_576d391ec549ed5e = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createQuery();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createSampler_49de055e495fedf8 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createSampler();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createVertexArray_4f450ed4d4a69acf = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createVertexArray();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_deleteQuery_9aaca8e15da5bc9c = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteQuery(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_deleteSampler_93e35dc696f633c9 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteSampler(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_deleteSync_80326e1fc23a1016 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteSync(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_deleteVertexArray_67635c7fe59aa660 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteVertexArray(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_drawArraysInstanced_3f02ae8708f8c4c7 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).drawArraysInstanced(arg1 >>> 0, arg2, arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_drawBuffers_6d32a0c370b9cb7f = function() { return logError(function (arg0, arg1) {
        getObject(arg0).drawBuffers(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_drawElementsInstanced_981861e70f6f9991 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).drawElementsInstanced(arg1 >>> 0, arg2, arg3 >>> 0, arg4, arg5);
    }, arguments) };
    imports.wbg.__wbg_endQuery_f256667aaa2e9fac = function() { return logError(function (arg0, arg1) {
        getObject(arg0).endQuery(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_fenceSync_f9c8da648fd4e444 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).fenceSync(arg1 >>> 0, arg2 >>> 0);
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_framebufferTextureLayer_45cb5a2978de4939 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).framebufferTextureLayer(arg1 >>> 0, arg2 >>> 0, getObject(arg3), arg4, arg5);
    }, arguments) };
    imports.wbg.__wbg_getBufferSubData_7f31bd9ec3682832 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).getBufferSubData(arg1 >>> 0, arg2, getObject(arg3));
    }, arguments) };
    imports.wbg.__wbg_getIndexedParameter_ad00bfb1210dbb28 = function() { return handleError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).getIndexedParameter(arg1 >>> 0, arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getQueryParameter_ea4da47c69182e79 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).getQueryParameter(getObject(arg1), arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getSyncParameter_295178259afc15d8 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).getSyncParameter(getObject(arg1), arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getUniformBlockIndex_091bee5be624ff21 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        const ret = getObject(arg0).getUniformBlockIndex(getObject(arg1), getStringFromWasm0(arg2, arg3));
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_invalidateFramebuffer_99c0131e9e958f49 = function() { return handleError(function (arg0, arg1, arg2) {
        getObject(arg0).invalidateFramebuffer(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_readBuffer_c02ab6ce6d95c99b = function() { return logError(function (arg0, arg1) {
        getObject(arg0).readBuffer(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_readPixels_40ba392d7aaf6ac0 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) {
        getObject(arg0).readPixels(arg1, arg2, arg3, arg4, arg5 >>> 0, arg6 >>> 0, getObject(arg7));
    }, arguments) };
    imports.wbg.__wbg_readPixels_db02ea1a888b611a = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) {
        getObject(arg0).readPixels(arg1, arg2, arg3, arg4, arg5 >>> 0, arg6 >>> 0, arg7);
    }, arguments) };
    imports.wbg.__wbg_renderbufferStorageMultisample_37c0b1b9e8a4f342 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).renderbufferStorageMultisample(arg1 >>> 0, arg2, arg3 >>> 0, arg4, arg5);
    }, arguments) };
    imports.wbg.__wbg_samplerParameterf_f60306a8facede3e = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).samplerParameterf(getObject(arg1), arg2 >>> 0, arg3);
    }, arguments) };
    imports.wbg.__wbg_samplerParameteri_da5225ffbb653046 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).samplerParameteri(getObject(arg1), arg2 >>> 0, arg3);
    }, arguments) };
    imports.wbg.__wbg_texImage2D_2558a70047650d54 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) {
        getObject(arg0).texImage2D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7 >>> 0, arg8 >>> 0, getObject(arg9));
    }, arguments) };
    imports.wbg.__wbg_texImage3D_7987a4b692d91b21 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) {
        getObject(arg0).texImage3D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7, arg8 >>> 0, arg9 >>> 0, getObject(arg10));
    }, arguments) };
    imports.wbg.__wbg_texStorage2D_0fff70234489e5a8 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).texStorage2D(arg1 >>> 0, arg2, arg3 >>> 0, arg4, arg5);
    }, arguments) };
    imports.wbg.__wbg_texStorage3D_7d322e9790add281 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6) {
        getObject(arg0).texStorage3D(arg1 >>> 0, arg2, arg3 >>> 0, arg4, arg5, arg6);
    }, arguments) };
    imports.wbg.__wbg_texSubImage2D_b4ac5eac47418cc5 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) {
        getObject(arg0).texSubImage2D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7 >>> 0, arg8 >>> 0, getObject(arg9));
    }, arguments) };
    imports.wbg.__wbg_texSubImage2D_b962ba533b866161 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) {
        getObject(arg0).texSubImage2D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7 >>> 0, arg8 >>> 0, arg9);
    }, arguments) };
    imports.wbg.__wbg_texSubImage2D_0b72a7308c3e78d3 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) {
        getObject(arg0).texSubImage2D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7 >>> 0, arg8 >>> 0, getObject(arg9));
    }, arguments) };
    imports.wbg.__wbg_texSubImage2D_8f2db7871647d37a = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) {
        getObject(arg0).texSubImage2D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7 >>> 0, arg8 >>> 0, getObject(arg9));
    }, arguments) };
    imports.wbg.__wbg_texSubImage2D_defc51298c31c0e3 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) {
        getObject(arg0).texSubImage2D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7 >>> 0, arg8 >>> 0, getObject(arg9));
    }, arguments) };
    imports.wbg.__wbg_texSubImage3D_bd2fd28608206fe5 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) {
        getObject(arg0).texSubImage3D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 >>> 0, arg10 >>> 0, arg11);
    }, arguments) };
    imports.wbg.__wbg_texSubImage3D_895cc20d45e04909 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) {
        getObject(arg0).texSubImage3D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 >>> 0, arg10 >>> 0, getObject(arg11));
    }, arguments) };
    imports.wbg.__wbg_texSubImage3D_f75ab42a48d9b789 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) {
        getObject(arg0).texSubImage3D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 >>> 0, arg10 >>> 0, getObject(arg11));
    }, arguments) };
    imports.wbg.__wbg_texSubImage3D_2b48a701e63f042e = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) {
        getObject(arg0).texSubImage3D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 >>> 0, arg10 >>> 0, getObject(arg11));
    }, arguments) };
    imports.wbg.__wbg_texSubImage3D_f983428ce1099b7f = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) {
        getObject(arg0).texSubImage3D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 >>> 0, arg10 >>> 0, getObject(arg11));
    }, arguments) };
    imports.wbg.__wbg_uniform1ui_71145d62b7bd13f4 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).uniform1ui(getObject(arg1), arg2 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_uniform2fv_4bd352337ccc4530 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform2fv(getObject(arg1), getArrayF32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniform2iv_829bd2f635ddf819 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform2iv(getObject(arg1), getArrayI32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniform2uiv_6ae4fe2845703965 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform2uiv(getObject(arg1), getArrayU32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniform3fv_3d2854c81603e498 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform3fv(getObject(arg1), getArrayF32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniform3iv_71333eb685ad9616 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform3iv(getObject(arg1), getArrayI32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniform3uiv_998cd5452e009d35 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform3uiv(getObject(arg1), getArrayU32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniform4fv_39cdcce4b1acc767 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform4fv(getObject(arg1), getArrayF32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniform4iv_f54116c4cfdcd96e = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform4iv(getObject(arg1), getArrayI32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniform4uiv_c1b79c253aa0271f = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform4uiv(getObject(arg1), getArrayU32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniformBlockBinding_52117c1104e3ac8a = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniformBlockBinding(getObject(arg1), arg2 >>> 0, arg3 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_uniformMatrix2fv_756ddcf41f02aa75 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).uniformMatrix2fv(getObject(arg1), arg2 !== 0, getArrayF32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_uniformMatrix2x3fv_b11505178375085e = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).uniformMatrix2x3fv(getObject(arg1), arg2 !== 0, getArrayF32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_uniformMatrix2x4fv_9a96ca1263d07814 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).uniformMatrix2x4fv(getObject(arg1), arg2 !== 0, getArrayF32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_uniformMatrix3fv_f26b98137276fd3d = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).uniformMatrix3fv(getObject(arg1), arg2 !== 0, getArrayF32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_uniformMatrix3x2fv_8e447d81dfee8f45 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).uniformMatrix3x2fv(getObject(arg1), arg2 !== 0, getArrayF32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_uniformMatrix3x4fv_0b4125c5150e9ebc = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).uniformMatrix3x4fv(getObject(arg1), arg2 !== 0, getArrayF32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_uniformMatrix4fv_5d8e0e047546456b = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).uniformMatrix4fv(getObject(arg1), arg2 !== 0, getArrayF32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_uniformMatrix4x2fv_15b6f3535fd4ce98 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).uniformMatrix4x2fv(getObject(arg1), arg2 !== 0, getArrayF32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_uniformMatrix4x3fv_5550b8543a32bbbd = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).uniformMatrix4x3fv(getObject(arg1), arg2 !== 0, getArrayF32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_vertexAttribDivisor_8479e8b81c913ed6 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).vertexAttribDivisor(arg1 >>> 0, arg2 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_vertexAttribIPointer_69f2f4bd74cf0bcb = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).vertexAttribIPointer(arg1 >>> 0, arg2, arg3 >>> 0, arg4, arg5);
    }, arguments) };
    imports.wbg.__wbg_activeTexture_d42cec3a26e47a5b = function() { return logError(function (arg0, arg1) {
        getObject(arg0).activeTexture(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_attachShader_2112634b3ffa9e9f = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).attachShader(getObject(arg1), getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_bindAttribLocation_e05596ff4f5413c3 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).bindAttribLocation(getObject(arg1), arg2 >>> 0, getStringFromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_bindBuffer_90d4fb91538001d5 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).bindBuffer(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_bindFramebuffer_4f950b884dc4be83 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).bindFramebuffer(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_bindRenderbuffer_1e0b14f526ed7a9d = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).bindRenderbuffer(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_bindTexture_75a698c47a923814 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).bindTexture(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_blendColor_7d3bf5e5214b44f7 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).blendColor(arg1, arg2, arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_blendEquation_6ca8e567e79464a4 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).blendEquation(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_blendEquationSeparate_34aa4cecd02882ab = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).blendEquationSeparate(arg1 >>> 0, arg2 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_blendFunc_cffe61957c92e9ac = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).blendFunc(arg1 >>> 0, arg2 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_blendFuncSeparate_3c342f57887c2900 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).blendFuncSeparate(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0, arg4 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_clear_8e2508724944df18 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).clear(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_clearColor_480962bfac4e1cbd = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).clearColor(arg1, arg2, arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_clearDepth_f5b4a73c4b8050eb = function() { return logError(function (arg0, arg1) {
        getObject(arg0).clearDepth(arg1);
    }, arguments) };
    imports.wbg.__wbg_clearStencil_1e4bb9932be75fce = function() { return logError(function (arg0, arg1) {
        getObject(arg0).clearStencil(arg1);
    }, arguments) };
    imports.wbg.__wbg_colorMask_21a93d0180bcbffa = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).colorMask(arg1 !== 0, arg2 !== 0, arg3 !== 0, arg4 !== 0);
    }, arguments) };
    imports.wbg.__wbg_compileShader_f40e0c51a7a836fd = function() { return logError(function (arg0, arg1) {
        getObject(arg0).compileShader(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_copyTexSubImage2D_65140521b061c61b = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) {
        getObject(arg0).copyTexSubImage2D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
    }, arguments) };
    imports.wbg.__wbg_createBuffer_7f57647465d111f0 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createBuffer();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createFramebuffer_8ebfde8c77472024 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createFramebuffer();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createProgram_7759fb2effb5d9b3 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createProgram();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createRenderbuffer_340b1c428d564bfd = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createRenderbuffer();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createShader_b474ef421ec0f80b = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createShader(arg1 >>> 0);
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createTexture_18b4a88c14cb086e = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createTexture();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_cullFace_fe427cdf8d0ea4e2 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).cullFace(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_deleteBuffer_fca5d765302c9a4e = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteBuffer(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_deleteFramebuffer_da681ed1dfa6d543 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteFramebuffer(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_deleteProgram_a06d69620332cc70 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteProgram(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_deleteRenderbuffer_5dcdde247a392125 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteRenderbuffer(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_deleteShader_138a810cc0ca9986 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteShader(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_deleteTexture_eae7abcfa3015f09 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteTexture(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_depthFunc_5527d3ee35e25a8d = function() { return logError(function (arg0, arg1) {
        getObject(arg0).depthFunc(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_depthMask_9120207d491c649a = function() { return logError(function (arg0, arg1) {
        getObject(arg0).depthMask(arg1 !== 0);
    }, arguments) };
    imports.wbg.__wbg_depthRange_d8d5ad00fd133fc0 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).depthRange(arg1, arg2);
    }, arguments) };
    imports.wbg.__wbg_disable_f0ef6e9a7ac6ddd7 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).disable(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_disableVertexAttribArray_e4f458e34e54fe78 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).disableVertexAttribArray(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_drawArrays_5bf0d92947e472af = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).drawArrays(arg1 >>> 0, arg2, arg3);
    }, arguments) };
    imports.wbg.__wbg_enable_8b3019da8846ce76 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).enable(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_enableVertexAttribArray_9d7b7e199f86e09b = function() { return logError(function (arg0, arg1) {
        getObject(arg0).enableVertexAttribArray(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_framebufferRenderbuffer_0144c6e35e2edb19 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).framebufferRenderbuffer(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0, getObject(arg4));
    }, arguments) };
    imports.wbg.__wbg_framebufferTexture2D_a6ad7148f7983ae6 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).framebufferTexture2D(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0, getObject(arg4), arg5);
    }, arguments) };
    imports.wbg.__wbg_frontFace_41ab8e7ce3e48cae = function() { return logError(function (arg0, arg1) {
        getObject(arg0).frontFace(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_getExtension_bef4112494c87f34 = function() { return handleError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).getExtension(getStringFromWasm0(arg1, arg2));
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getParameter_aa9af66884d2b210 = function() { return handleError(function (arg0, arg1) {
        const ret = getObject(arg0).getParameter(arg1 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getProgramInfoLog_4d189135f8d5a2de = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg1).getProgramInfoLog(getObject(arg2));
        var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    }, arguments) };
    imports.wbg.__wbg_getProgramParameter_7b04ca71a79d9047 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).getProgramParameter(getObject(arg1), arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getShaderInfoLog_d5de3e4eab06fc46 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg1).getShaderInfoLog(getObject(arg2));
        var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    }, arguments) };
    imports.wbg.__wbg_getShaderParameter_4ddb51279bb1500b = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).getShaderParameter(getObject(arg1), arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getSupportedExtensions_7a174085f9e1983a = function() { return logError(function (arg0) {
        const ret = getObject(arg0).getSupportedExtensions();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getUniformLocation_51ec30e3755e574d = function() { return logError(function (arg0, arg1, arg2, arg3) {
        const ret = getObject(arg0).getUniformLocation(getObject(arg1), getStringFromWasm0(arg2, arg3));
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_linkProgram_eabc664217816e72 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).linkProgram(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_pixelStorei_162a23ba7872b886 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).pixelStorei(arg1 >>> 0, arg2);
    }, arguments) };
    imports.wbg.__wbg_polygonOffset_9f20aa27db3ea0a2 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).polygonOffset(arg1, arg2);
    }, arguments) };
    imports.wbg.__wbg_renderbufferStorage_ff5740fb95ecf231 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).renderbufferStorage(arg1 >>> 0, arg2 >>> 0, arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_scissor_726eea865bbd6809 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).scissor(arg1, arg2, arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_shaderSource_7943d06f24862a3b = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).shaderSource(getObject(arg1), getStringFromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_stencilFuncSeparate_c16750a621e43580 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).stencilFuncSeparate(arg1 >>> 0, arg2 >>> 0, arg3, arg4 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_stencilMask_9abfc669d9c2a893 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).stencilMask(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_stencilMaskSeparate_a1f8f805de62aac5 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).stencilMaskSeparate(arg1 >>> 0, arg2 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_stencilOpSeparate_2f2cc25254360270 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).stencilOpSeparate(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0, arg4 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_texParameteri_8f70dffce11d7da1 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).texParameteri(arg1 >>> 0, arg2 >>> 0, arg3);
    }, arguments) };
    imports.wbg.__wbg_uniform1f_9b9e5339e7560722 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).uniform1f(getObject(arg1), arg2);
    }, arguments) };
    imports.wbg.__wbg_uniform1i_bdcd75be097285e6 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).uniform1i(getObject(arg1), arg2);
    }, arguments) };
    imports.wbg.__wbg_uniform4f_b143081575a3bb56 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).uniform4f(getObject(arg1), arg2, arg3, arg4, arg5);
    }, arguments) };
    imports.wbg.__wbg_useProgram_757fab437af29c20 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).useProgram(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_vertexAttribPointer_4416f0325c02aa13 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6) {
        getObject(arg0).vertexAttribPointer(arg1 >>> 0, arg2, arg3 >>> 0, arg4 !== 0, arg5, arg6);
    }, arguments) };
    imports.wbg.__wbg_viewport_7414e7e2a83afc72 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).viewport(arg1, arg2, arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_instanceof_Window_f401953a2cf86220 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof Window;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_document_5100775d18896c16 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).document;
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_navigator_6c8fa55c5cc8796e = function() { return logError(function (arg0) {
        const ret = getObject(arg0).navigator;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_requestAnimationFrame_549258cfa66011f0 = function() { return handleError(function (arg0, arg1) {
        const ret = getObject(arg0).requestAnimationFrame(getObject(arg1));
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_setTimeout_c172d5704ef82276 = function() { return handleError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).setTimeout(getObject(arg1), arg2);
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_location_1325817a58c77ceb = function() { return logError(function (arg0) {
        const ret = getObject(arg0).location;
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_pointerLockElement_571dbde6150f0fa5 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).pointerLockElement;
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_exitPointerLock_b62fe3c7830470e4 = function() { return logError(function (arg0) {
        getObject(arg0).exitPointerLock();
    }, arguments) };
    imports.wbg.__wbg_getElementById_c369ff43f0db99cf = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).getElementById(getStringFromWasm0(arg1, arg2));
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_querySelector_a5f74efc5fa193dd = function() { return handleError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).querySelector(getStringFromWasm0(arg1, arg2));
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_querySelectorAll_4e0fcdb64cda2cd5 = function() { return handleError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).querySelectorAll(getStringFromWasm0(arg1, arg2));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_instanceof_Element_6945fc210db80ea9 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof Element;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_classList_1f0528ee002e56d4 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).classList;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_requestPointerLock_78b2a4a24cb69366 = function() { return logError(function (arg0) {
        getObject(arg0).requestPointerLock();
    }, arguments) };
    imports.wbg.__wbg_instanceof_HtmlElement_3bcc4ff70cfdcba5 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof HTMLElement;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_style_c3fc3dd146182a2d = function() { return logError(function (arg0) {
        const ret = getObject(arg0).style;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_setdata_8c2b43af041cc1b3 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).data = getStringFromWasm0(arg1, arg2);
    }, arguments) };
    imports.wbg.__wbg_appendData_e04d7c6b33c7b800 = function() { return handleError(function (arg0, arg1, arg2) {
        getObject(arg0).appendData(getStringFromWasm0(arg1, arg2));
    }, arguments) };
    imports.wbg.__wbg_bufferData_bb9321e8fa042bac = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).bufferData(arg1 >>> 0, arg2, arg3 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_bufferData_5d1e6b8eaa7d23c8 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).bufferData(arg1 >>> 0, getObject(arg2), arg3 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_bufferSubData_a6cea5e056662bd7 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).bufferSubData(arg1 >>> 0, arg2, getObject(arg3));
    }, arguments) };
    imports.wbg.__wbg_compressedTexSubImage2D_db8b170a99900aff = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) {
        getObject(arg0).compressedTexSubImage2D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7 >>> 0, getObject(arg8));
    }, arguments) };
    imports.wbg.__wbg_readPixels_551d0505625c865b = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) {
        getObject(arg0).readPixels(arg1, arg2, arg3, arg4, arg5 >>> 0, arg6 >>> 0, getObject(arg7));
    }, arguments) };
    imports.wbg.__wbg_texImage2D_a14a3c7863e25c89 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) {
        getObject(arg0).texImage2D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7 >>> 0, arg8 >>> 0, getObject(arg9));
    }, arguments) };
    imports.wbg.__wbg_texSubImage2D_55a407e48f3a5cb4 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) {
        getObject(arg0).texSubImage2D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7 >>> 0, arg8 >>> 0, getObject(arg9));
    }, arguments) };
    imports.wbg.__wbg_uniform2fv_dcb8b73e2637092a = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform2fv(getObject(arg1), getArrayF32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniform2iv_fc73855d9dec793a = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform2iv(getObject(arg1), getArrayI32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniform3fv_3e32c897d3ed1eaa = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform3fv(getObject(arg1), getArrayF32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniform3iv_2b3fa9d97dff01a2 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform3iv(getObject(arg1), getArrayI32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniform4fv_980ce05d950ee599 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform4fv(getObject(arg1), getArrayF32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniform4iv_f112dcc4401f5469 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).uniform4iv(getObject(arg1), getArrayI32FromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_uniformMatrix2fv_4417ed4d88a140be = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).uniformMatrix2fv(getObject(arg1), arg2 !== 0, getArrayF32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_uniformMatrix3fv_d46553a1248946b5 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).uniformMatrix3fv(getObject(arg1), arg2 !== 0, getArrayF32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_uniformMatrix4fv_cd46ed81bccb0cb2 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).uniformMatrix4fv(getObject(arg1), arg2 !== 0, getArrayF32FromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_activeTexture_5f084e1b3f14853e = function() { return logError(function (arg0, arg1) {
        getObject(arg0).activeTexture(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_attachShader_6397dc4fd87343d3 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).attachShader(getObject(arg1), getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_bindAttribLocation_7ab87f5815dce9f0 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).bindAttribLocation(getObject(arg1), arg2 >>> 0, getStringFromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_bindBuffer_1e5043751efddd4f = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).bindBuffer(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_bindFramebuffer_c301d73a2c2842bb = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).bindFramebuffer(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_bindRenderbuffer_8ec7d02bd60bdfb2 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).bindRenderbuffer(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_bindTexture_772f5eb022019d87 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).bindTexture(arg1 >>> 0, getObject(arg2));
    }, arguments) };
    imports.wbg.__wbg_blendColor_f25a274ecd388a1e = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).blendColor(arg1, arg2, arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_blendEquation_a442d97b5c6efedb = function() { return logError(function (arg0, arg1) {
        getObject(arg0).blendEquation(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_blendEquationSeparate_721f30ba584a5233 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).blendEquationSeparate(arg1 >>> 0, arg2 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_blendFunc_fc4b298f39801a9c = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).blendFunc(arg1 >>> 0, arg2 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_blendFuncSeparate_abe2ad4272c8365e = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).blendFuncSeparate(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0, arg4 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_clear_f9731a47df2e70d8 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).clear(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_clearColor_42707553c40e0e0f = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).clearColor(arg1, arg2, arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_clearDepth_42ac48f2ab25c419 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).clearDepth(arg1);
    }, arguments) };
    imports.wbg.__wbg_clearStencil_0f906e2d8b61aa7a = function() { return logError(function (arg0, arg1) {
        getObject(arg0).clearStencil(arg1);
    }, arguments) };
    imports.wbg.__wbg_colorMask_03aa359acc86fd70 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).colorMask(arg1 !== 0, arg2 !== 0, arg3 !== 0, arg4 !== 0);
    }, arguments) };
    imports.wbg.__wbg_compileShader_3af4719dfdb508e3 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).compileShader(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_copyTexSubImage2D_0e21b1e1089c410a = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) {
        getObject(arg0).copyTexSubImage2D(arg1 >>> 0, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
    }, arguments) };
    imports.wbg.__wbg_createBuffer_34e01f5c10929b41 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createBuffer();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createFramebuffer_49ca64e9e1c6f5eb = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createFramebuffer();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createProgram_9affbfa62b7b2608 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createProgram();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createRenderbuffer_375d7f4004bc49bd = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createRenderbuffer();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createShader_55ca04b44164bd41 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).createShader(arg1 >>> 0);
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_createTexture_c13c31b2b132c17f = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createTexture();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_cullFace_af37bb1c2d22ab73 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).cullFace(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_deleteBuffer_96df38349e3487d2 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteBuffer(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_deleteFramebuffer_417b62b6156d4894 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteFramebuffer(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_deleteProgram_641402f7551587d8 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteProgram(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_deleteRenderbuffer_d3aedb394b1ea546 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteRenderbuffer(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_deleteShader_e5c778f25b722e68 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteShader(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_deleteTexture_f89d8e417b156960 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteTexture(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_depthFunc_1ee4bf1e0127bf7f = function() { return logError(function (arg0, arg1) {
        getObject(arg0).depthFunc(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_depthMask_dd6cd8a9aff90e5c = function() { return logError(function (arg0, arg1) {
        getObject(arg0).depthMask(arg1 !== 0);
    }, arguments) };
    imports.wbg.__wbg_depthRange_7e521414b51cf5de = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).depthRange(arg1, arg2);
    }, arguments) };
    imports.wbg.__wbg_disable_5dd8c3842de93e92 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).disable(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_disableVertexAttribArray_12bc9adefa738796 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).disableVertexAttribArray(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_drawArrays_f619a26a53ab5ab3 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).drawArrays(arg1 >>> 0, arg2, arg3);
    }, arguments) };
    imports.wbg.__wbg_enable_7abe812a71c76206 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).enable(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_enableVertexAttribArray_6d44444aa994f42a = function() { return logError(function (arg0, arg1) {
        getObject(arg0).enableVertexAttribArray(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_framebufferRenderbuffer_e1c9c64aea848b39 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).framebufferRenderbuffer(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0, getObject(arg4));
    }, arguments) };
    imports.wbg.__wbg_framebufferTexture2D_66e1968fd5b7b3e3 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).framebufferTexture2D(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0, getObject(arg4), arg5);
    }, arguments) };
    imports.wbg.__wbg_frontFace_bb8a1ded6f52865e = function() { return logError(function (arg0, arg1) {
        getObject(arg0).frontFace(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_getParameter_a77768abe8a51f24 = function() { return handleError(function (arg0, arg1) {
        const ret = getObject(arg0).getParameter(arg1 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getProgramInfoLog_bf1fba8fa90667c7 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg1).getProgramInfoLog(getObject(arg2));
        var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    }, arguments) };
    imports.wbg.__wbg_getProgramParameter_10c8a43809fb8c2e = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).getProgramParameter(getObject(arg1), arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getShaderInfoLog_0262cb299092ce92 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg1).getShaderInfoLog(getObject(arg2));
        var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    }, arguments) };
    imports.wbg.__wbg_getShaderParameter_60b69083e8d662ce = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).getShaderParameter(getObject(arg1), arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getUniformLocation_6eedfb513ccce732 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        const ret = getObject(arg0).getUniformLocation(getObject(arg1), getStringFromWasm0(arg2, arg3));
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_linkProgram_af5fed9dc3f1cdf9 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).linkProgram(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_pixelStorei_054e50b5fdc17824 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).pixelStorei(arg1 >>> 0, arg2);
    }, arguments) };
    imports.wbg.__wbg_polygonOffset_2927e355350d4327 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).polygonOffset(arg1, arg2);
    }, arguments) };
    imports.wbg.__wbg_renderbufferStorage_f41b3c99f6a8f25e = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).renderbufferStorage(arg1 >>> 0, arg2 >>> 0, arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_scissor_75ba2245d4db0eaf = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).scissor(arg1, arg2, arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_shaderSource_7891a1fcb69a0023 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).shaderSource(getObject(arg1), getStringFromWasm0(arg2, arg3));
    }, arguments) };
    imports.wbg.__wbg_stencilFuncSeparate_a3699f92e69c1494 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).stencilFuncSeparate(arg1 >>> 0, arg2 >>> 0, arg3, arg4 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_stencilMask_c5ad44ea27c5f169 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).stencilMask(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_stencilMaskSeparate_a7830b1e1eabf5bd = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).stencilMaskSeparate(arg1 >>> 0, arg2 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_stencilOpSeparate_321604240216c55c = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).stencilOpSeparate(arg1 >>> 0, arg2 >>> 0, arg3 >>> 0, arg4 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_texParameteri_d1035ed45d6c5655 = function() { return logError(function (arg0, arg1, arg2, arg3) {
        getObject(arg0).texParameteri(arg1 >>> 0, arg2 >>> 0, arg3);
    }, arguments) };
    imports.wbg.__wbg_uniform1f_8914cb45b3ad5887 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).uniform1f(getObject(arg1), arg2);
    }, arguments) };
    imports.wbg.__wbg_uniform1i_badd5ff70c0d30bf = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).uniform1i(getObject(arg1), arg2);
    }, arguments) };
    imports.wbg.__wbg_uniform4f_fb56c7f4de64dd4c = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).uniform4f(getObject(arg1), arg2, arg3, arg4, arg5);
    }, arguments) };
    imports.wbg.__wbg_useProgram_c637e43f9cd4c07a = function() { return logError(function (arg0, arg1) {
        getObject(arg0).useProgram(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_vertexAttribPointer_c25e4c5ed17f8a1d = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6) {
        getObject(arg0).vertexAttribPointer(arg1 >>> 0, arg2, arg3 >>> 0, arg4 !== 0, arg5, arg6);
    }, arguments) };
    imports.wbg.__wbg_viewport_221ade2aef6032c8 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).viewport(arg1, arg2, arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_navigator_56803b85352a0575 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).navigator;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_instanceof_DragEvent_329fd02ae838527e = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof DragEvent;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_dataTransfer_cef7816623bd8478 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).dataTransfer;
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_debug_5fb96680aecf5dc8 = function() { return logError(function (arg0) {
        console.debug(getObject(arg0));
    }, arguments) };
    imports.wbg.__wbg_error_8e3928cfb8a43e2b = function() { return logError(function (arg0) {
        console.error(getObject(arg0));
    }, arguments) };
    imports.wbg.__wbg_error_6e987ee48d9fdf45 = function() { return logError(function (arg0, arg1) {
        console.error(getObject(arg0), getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_info_530a29cb2e4e3304 = function() { return logError(function (arg0) {
        console.info(getObject(arg0));
    }, arguments) };
    imports.wbg.__wbg_log_5bb5f88f245d7762 = function() { return logError(function (arg0) {
        console.log(getObject(arg0));
    }, arguments) };
    imports.wbg.__wbg_warn_63bbae1730aead09 = function() { return logError(function (arg0) {
        console.warn(getObject(arg0));
    }, arguments) };
    imports.wbg.__wbg_setProperty_ea7d15a2b591aa97 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).setProperty(getStringFromWasm0(arg1, arg2), getStringFromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_name_f35eb93a73d94973 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg1).name;
        const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    }, arguments) };
    imports.wbg.__wbg_videoWidth_f0b751704b53672c = function() { return logError(function (arg0) {
        const ret = getObject(arg0).videoWidth;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_videoHeight_e75550285bbbfdab = function() { return logError(function (arg0) {
        const ret = getObject(arg0).videoHeight;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_width_0e2f1c393242f16e = function() { return logError(function (arg0) {
        const ret = getObject(arg0).width;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_height_d6c8a3041eff461a = function() { return logError(function (arg0) {
        const ret = getObject(arg0).height;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_ownerDocument_a93c92720a050068 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).ownerDocument;
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_textContent_8e392d624539e731 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg1).textContent;
        var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    }, arguments) };
    imports.wbg.__wbg_settextContent_d271bab459cbb1ba = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).textContent = arg1 === 0 ? undefined : getStringFromWasm0(arg1, arg2);
    }, arguments) };
    imports.wbg.__wbg_appendChild_580ccb11a660db68 = function() { return handleError(function (arg0, arg1) {
        const ret = getObject(arg0).appendChild(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_get_8cd5eba00ab6304f = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0)[arg1 >>> 0];
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_arrayBuffer_307ddd1bd1d04e23 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).arrayBuffer();
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_kind_7052b934e898cbef = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg1).kind;
        const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    }, arguments) };
    imports.wbg.__wbg_getAsFile_5619cae709cec67d = function() { return handleError(function (arg0) {
        const ret = getObject(arg0).getAsFile();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_bindVertexArrayOES_abe2fd389c6a2f56 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).bindVertexArrayOES(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_createVertexArrayOES_886be8a08db32ce6 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).createVertexArrayOES();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_deleteVertexArrayOES_153f352862874f30 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).deleteVertexArrayOES(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_instanceof_HtmlCanvasElement_46bdbf323b0b18d1 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof HTMLCanvasElement;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_width_aee8b8809b033b05 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).width;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_setwidth_080107476e633963 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).width = arg1 >>> 0;
    }, arguments) };
    imports.wbg.__wbg_height_80053d3c71b338e0 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).height;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_setheight_dc240617639f1f51 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).height = arg1 >>> 0;
    }, arguments) };
    imports.wbg.__wbg_getContext_df50fa48a8876636 = function() { return handleError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).getContext(getStringFromWasm0(arg1, arg2));
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getContext_fec464290556673c = function() { return handleError(function (arg0, arg1, arg2, arg3) {
        const ret = getObject(arg0).getContext(getStringFromWasm0(arg1, arg2), getObject(arg3));
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_instanceof_HtmlProgressElement_a546427a3080be57 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof HTMLProgressElement;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_setvalue_701313998b046741 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).value = arg1;
    }, arguments) };
    imports.wbg.__wbg_width_6aa39fc77f088914 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).width;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_setwidth_83d936c4b04dcbec = function() { return logError(function (arg0, arg1) {
        getObject(arg0).width = arg1 >>> 0;
    }, arguments) };
    imports.wbg.__wbg_height_05a87854adf24d83 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).height;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_setheight_6025ba0d58e6cc8c = function() { return logError(function (arg0, arg1) {
        getObject(arg0).height = arg1 >>> 0;
    }, arguments) };
    imports.wbg.__wbg_getContext_c102f659d540d068 = function() { return handleError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).getContext(getStringFromWasm0(arg1, arg2));
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_getContext_c9fc178d1fa6f8fe = function() { return handleError(function (arg0, arg1, arg2, arg3) {
        const ret = getObject(arg0).getContext(getStringFromWasm0(arg1, arg2), getObject(arg3));
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_instanceof_KeyboardEvent_d51b1a079e0c6a46 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof KeyboardEvent;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_keyCode_2af7775f99bf8e33 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).keyCode;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_altKey_2e6c34c37088d8b1 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).altKey;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_ctrlKey_bb5b6fef87339703 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).ctrlKey;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_metaKey_6bf4ae4e83a11278 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).metaKey;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_search_489f12953342ec1f = function() { return handleError(function (arg0, arg1) {
        const ret = getObject(arg1).search;
        const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    }, arguments) };
    imports.wbg.__wbg_getSupportedProfiles_904a0392ad42295b = function() { return logError(function (arg0) {
        const ret = getObject(arg0).getSupportedProfiles();
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_add_dcb05a8ba423bdac = function() { return handleError(function (arg0, arg1, arg2) {
        getObject(arg0).add(getStringFromWasm0(arg1, arg2));
    }, arguments) };
    imports.wbg.__wbg_remove_698118fb25ab8150 = function() { return handleError(function (arg0, arg1, arg2) {
        getObject(arg0).remove(getStringFromWasm0(arg1, arg2));
    }, arguments) };
    imports.wbg.__wbg_framebufferTextureMultiviewOVR_a4eb1a11052508f4 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5, arg6) {
        getObject(arg0).framebufferTextureMultiviewOVR(arg1 >>> 0, arg2 >>> 0, getObject(arg3), arg4, arg5, arg6);
    }, arguments) };
    imports.wbg.__wbg_drawBuffersWEBGL_4c663e042e093892 = function() { return logError(function (arg0, arg1) {
        getObject(arg0).drawBuffersWEBGL(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_drawArraysInstancedANGLE_6afae595a484db93 = function() { return logError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).drawArraysInstancedANGLE(arg1 >>> 0, arg2, arg3, arg4);
    }, arguments) };
    imports.wbg.__wbg_drawElementsInstancedANGLE_f175a178d553357e = function() { return logError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
        getObject(arg0).drawElementsInstancedANGLE(arg1 >>> 0, arg2, arg3 >>> 0, arg4, arg5);
    }, arguments) };
    imports.wbg.__wbg_vertexAttribDivisorANGLE_b258d7388e466921 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).vertexAttribDivisorANGLE(arg1 >>> 0, arg2 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_items_5070ce38a6d53ed2 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).items;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_length_a23c520109d9ba0a = function() { return logError(function (arg0) {
        const ret = getObject(arg0).length;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_get_0fa6ec8bd6a5c256 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0)[arg1 >>> 0];
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_instanceof_Event_d64fe4ffce1db0b2 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof Event;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_preventDefault_b1a4aafc79409429 = function() { return logError(function (arg0) {
        getObject(arg0).preventDefault();
    }, arguments) };
    imports.wbg.__wbg_stopPropagation_fa5b666049c9fd02 = function() { return logError(function (arg0) {
        getObject(arg0).stopPropagation();
    }, arguments) };
    imports.wbg.__wbg_addEventListener_4283b15b4f039eb5 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4) {
        getObject(arg0).addEventListener(getStringFromWasm0(arg1, arg2), getObject(arg3), getObject(arg4));
    }, arguments) };
    imports.wbg.__wbg_instanceof_FocusEvent_f00e8be2e08fecc0 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof FocusEvent;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_instanceof_MouseEvent_fdc007d866fdd0df = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof MouseEvent;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_clientX_fef6bf7a6bcf41b8 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).clientX;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_clientY_df42f8fceab3cef2 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).clientY;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_button_367cdc7303e3cf9b = function() { return logError(function (arg0) {
        const ret = getObject(arg0).button;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_movementX_b800a0cacd14d9bf = function() { return logError(function (arg0) {
        const ret = getObject(arg0).movementX;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_movementY_7907e03eb8c0ea1e = function() { return logError(function (arg0) {
        const ret = getObject(arg0).movementY;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_new_d7841ab0a0905e04 = function() { return handleError(function () {
        const ret = new Text();
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_crypto_d05b68a3572bb8ca = function() { return logError(function (arg0) {
        const ret = getObject(arg0).crypto;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_process_b02b3570280d0366 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).process;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_versions_c1cb42213cedf0f5 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).versions;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_node_43b1089f407e4ec2 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).node;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbindgen_is_string = function(arg0) {
        const ret = typeof(getObject(arg0)) === 'string';
        _assertBoolean(ret);
        return ret;
    };
    imports.wbg.__wbg_require_9a7e0f667ead4995 = function() { return handleError(function () {
        const ret = module.require;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_msCrypto_10fc94afee92bd76 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).msCrypto;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_randomFillSync_b70ccbdf4926a99d = function() { return handleError(function (arg0, arg1) {
        getObject(arg0).randomFillSync(takeObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_getRandomValues_7e42b4fb8779dc6d = function() { return handleError(function (arg0, arg1) {
        getObject(arg0).getRandomValues(getObject(arg1));
    }, arguments) };
    imports.wbg.__wbg_get_bd8e338fbd5f5cc8 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0)[arg1 >>> 0];
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_length_cd7af8117672b8b8 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).length;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_new_16b304a2cfa7ff4a = function() { return logError(function () {
        const ret = new Array();
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_newnoargs_e258087cd0daa0ea = function() { return logError(function (arg0, arg1) {
        const ret = new Function(getStringFromWasm0(arg0, arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_call_27c0f87801dedf93 = function() { return handleError(function (arg0, arg1) {
        const ret = getObject(arg0).call(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_new_72fb9a18b5ae2624 = function() { return logError(function () {
        const ret = new Object();
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_self_ce0dbfc45cf2f5be = function() { return handleError(function () {
        const ret = self.self;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_window_c6fb939a7f436783 = function() { return handleError(function () {
        const ret = window.window;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_globalThis_d1e6af4856ba331b = function() { return handleError(function () {
        const ret = globalThis.globalThis;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_global_207b558942527489 = function() { return handleError(function () {
        const ret = global.global;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_includes_310a37f41280ae42 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).includes(getObject(arg1), arg2);
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_of_4a2b313a453ec059 = function() { return logError(function (arg0) {
        const ret = Array.of(getObject(arg0));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_push_a5b05aedc7234f9f = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).push(getObject(arg1));
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_instanceof_ArrayBuffer_836825be07d4c9d2 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof ArrayBuffer;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_new_28c511d9baebfa89 = function() { return logError(function (arg0, arg1) {
        const ret = new Error(getStringFromWasm0(arg0, arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_call_b3ca7c6051f9bec1 = function() { return handleError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).call(getObject(arg1), getObject(arg2));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_instanceof_Object_71ca3c0a59266746 = function() { return logError(function (arg0) {
        let result;
        try {
            result = getObject(arg0) instanceof Object;
        } catch (_) {
            result = false;
        }
        const ret = result;
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_is_010fdc0f4ab96916 = function() { return logError(function (arg0, arg1) {
        const ret = Object.is(getObject(arg0), getObject(arg1));
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_valueOf_a0b7c836f68a054b = function() { return logError(function (arg0) {
        const ret = getObject(arg0).valueOf();
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_new_81740750da40724f = function() { return logError(function (arg0, arg1) {
        try {
            var state0 = {a: arg0, b: arg1};
            var cb0 = (arg0, arg1) => {
                const a = state0.a;
                state0.a = 0;
                try {
                    return __wbg_adapter_1014(a, state0.b, arg0, arg1);
                } finally {
                    state0.a = a;
                }
            };
            const ret = new Promise(cb0);
            return addHeapObject(ret);
        } finally {
            state0.a = state0.b = 0;
        }
    }, arguments) };
    imports.wbg.__wbg_resolve_b0083a7967828ec8 = function() { return logError(function (arg0) {
        const ret = Promise.resolve(getObject(arg0));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_then_0c86a60e8fcfe9f6 = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).then(getObject(arg1));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_then_a73caa9a87991566 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).then(getObject(arg1), getObject(arg2));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_buffer_12d079cc21e14bdb = function() { return logError(function (arg0) {
        const ret = getObject(arg0).buffer;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_newwithbyteoffsetandlength_41559f654c4e743c = function() { return logError(function (arg0, arg1, arg2) {
        const ret = new Int8Array(getObject(arg0), arg1 >>> 0, arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_newwithbyteoffsetandlength_4bea9f904a7e0aef = function() { return logError(function (arg0, arg1, arg2) {
        const ret = new Int16Array(getObject(arg0), arg1 >>> 0, arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_newwithbyteoffsetandlength_425360430a1c8206 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = new Int32Array(getObject(arg0), arg1 >>> 0, arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_newwithbyteoffsetandlength_aa4a17c33a06e5cb = function() { return logError(function (arg0, arg1, arg2) {
        const ret = new Uint8Array(getObject(arg0), arg1 >>> 0, arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_new_63b92bc8671ed464 = function() { return logError(function (arg0) {
        const ret = new Uint8Array(getObject(arg0));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_set_a47bac70306a19a7 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).set(getObject(arg1), arg2 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_length_c20a40f15020d68a = function() { return logError(function (arg0) {
        const ret = getObject(arg0).length;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_newwithbyteoffsetandlength_9fd64654bc0b0817 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = new Uint16Array(getObject(arg0), arg1 >>> 0, arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_newwithbyteoffsetandlength_3125852e5a7fbcff = function() { return logError(function (arg0, arg1, arg2) {
        const ret = new Uint32Array(getObject(arg0), arg1 >>> 0, arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_newwithbyteoffsetandlength_4a659d079a1650e0 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = new Float32Array(getObject(arg0), arg1 >>> 0, arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_newwithlength_e9b4878cebadb3d3 = function() { return logError(function (arg0) {
        const ret = new Uint8Array(arg0 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_buffer_dd7f74bc60f1faab = function() { return logError(function (arg0) {
        const ret = getObject(arg0).buffer;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_subarray_a1f73cd4b5b42fe1 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).subarray(arg1 >>> 0, arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_set_1f9b04f170055d33 = function() { return handleError(function (arg0, arg1, arg2) {
        const ret = Reflect.set(getObject(arg0), getObject(arg1), getObject(arg2));
        _assertBoolean(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbindgen_debug_string = function(arg0, arg1) {
        const ret = debugString(getObject(arg1));
        const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    };
    imports.wbg.__wbindgen_throw = function(arg0, arg1) {
        throw new Error(getStringFromWasm0(arg0, arg1));
    };
    imports.wbg.__wbindgen_memory = function() {
        const ret = wasm.memory;
        return addHeapObject(ret);
    };
    imports.wbg.__wbindgen_closure_wrapper1018 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = makeClosure(arg0, arg1, 302, __wbg_adapter_32);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbindgen_closure_wrapper1020 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = makeMutClosure(arg0, arg1, 302, __wbg_adapter_35);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbindgen_closure_wrapper1022 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = makeMutClosure(arg0, arg1, 302, __wbg_adapter_38);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbindgen_closure_wrapper8914 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = makeMutClosure(arg0, arg1, 3908, __wbg_adapter_41);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbindgen_closure_wrapper8916 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = makeMutClosure(arg0, arg1, 3908, __wbg_adapter_41);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbindgen_closure_wrapper9806 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = makeMutClosure(arg0, arg1, 4391, __wbg_adapter_46);
        return addHeapObject(ret);
    }, arguments) };

    return imports;
}

function __wbg_init_memory(imports, maybe_memory) {

}

function __wbg_finalize_init(instance, module) {
    wasm = instance.exports;
    __wbg_init.__wbindgen_wasm_module = module;
    cachedFloat32Memory0 = null;
    cachedFloat64Memory0 = null;
    cachedInt32Memory0 = null;
    cachedUint32Memory0 = null;
    cachedUint8Memory0 = null;


    return wasm;
}

function initSync(module) {
    if (wasm !== undefined) return wasm;

    const imports = __wbg_get_imports();

    __wbg_init_memory(imports);

    if (!(module instanceof WebAssembly.Module)) {
        module = new WebAssembly.Module(module);
    }

    const instance = new WebAssembly.Instance(module, imports);

    return __wbg_finalize_init(instance, module);
}

async function __wbg_init(input) {
    if (wasm !== undefined) return wasm;

    if (typeof input === 'undefined') {
        input = new URL('all_is_cubes_wasm_bg.wasm', import.meta.url);
    }
    const imports = __wbg_get_imports();

    if (typeof input === 'string' || (typeof Request === 'function' && input instanceof Request) || (typeof URL === 'function' && input instanceof URL)) {
        input = fetch(input);
    }

    __wbg_init_memory(imports);

    const { instance, module } = await __wbg_load(await input, imports);

    return __wbg_finalize_init(instance, module);
}

export { initSync }
export default __wbg_init;
