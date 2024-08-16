// generated by diplomat-tool
import { FixedDecimalGroupingStrategy } from "./FixedDecimalGroupingStrategy.mjs"
import wasm from "./diplomat-wasm.mjs";
import * as diplomatRuntime from "./diplomat-runtime.mjs";

export class FixedDecimalFormatterOptions {

    #groupingStrategy;
    get groupingStrategy()  {
        return this.#groupingStrategy;
    }
    set groupingStrategy(value) {
        this.#groupingStrategy = value;
    }

    #someOtherConfig;
    get someOtherConfig()  {
        return this.#someOtherConfig;
    }
    set someOtherConfig(value) {
        this.#someOtherConfig = value;
    }

    // Return this struct in FFI function friendly format.
    // Returns an array that can be expanded with spread syntax (...)
    
    _intoFFI(
        functionCleanupArena,
        appendArrayMap
    ) {
        return [this.#groupingStrategy.ffiValue, this.#someOtherConfig]
    }

    // This struct contains borrowed fields, so this takes in a list of
    // "edges" corresponding to where each lifetime's data may have been borrowed from
    // and passes it down to individual fields containing the borrow.
    // This method does not attempt to handle any dependencies between lifetimes, the caller
    // should handle this when constructing edge arrays.
    _fromFFI(ptr) {
        const groupingStrategyDeref = diplomatRuntime.enumDiscriminant(wasm, ptr);
        this.#groupingStrategy = FixedDecimalGroupingStrategy[Array.from(FixedDecimalGroupingStrategy.values.keys())[groupingStrategyDeref]];
        const someOtherConfigDeref = (new Uint8Array(wasm.memory.buffer, ptr + 4, 1))[0] === 1;
        this.#someOtherConfig = someOtherConfigDeref;

        return this;
    }

    static default_() {
        
        const diplomatReceive = new diplomatRuntime.DiplomatReceiveBuf(wasm, 5, 4, false);
        const result = wasm.icu4x_FixedDecimalFormatterOptions_default_mv1(diplomatReceive.buffer);
    
        try {
            return new FixedDecimalFormatterOptions()._fromFFI(diplomatReceive.buffer);
        }
        
        finally {
            diplomatReceive.free();
        }
    }
}