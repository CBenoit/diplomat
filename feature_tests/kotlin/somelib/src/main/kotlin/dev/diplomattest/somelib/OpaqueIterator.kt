package dev.diplomattest.somelib;
import com.sun.jna.Library
import com.sun.jna.Native
import com.sun.jna.Pointer


internal interface OpaqueIteratorLib: Library {
    fun OpaqueIterator_destroy(handle: Pointer)
    fun namespace_OpaqueIterator_next(handle: Pointer): Pointer?
}
typealias OpaqueIteratorIteratorItem = AttrOpaque1Renamed?

class OpaqueIterator internal constructor (
    internal val handle: Pointer,
    // These ensure that anything that is borrowed is kept alive and not cleaned
    // up by the garbage collector.
    internal val selfEdges: List<Any>,
    internal val aEdges: List<Any>,
): Iterator<AttrOpaque1Renamed?> {

    internal class OpaqueIteratorCleaner(val handle: Pointer, val lib: OpaqueIteratorLib) : Runnable {
        override fun run() {
            lib.OpaqueIterator_destroy(handle)
        }
    }

    companion object {
        internal val libClass: Class<OpaqueIteratorLib> = OpaqueIteratorLib::class.java
        internal val lib: OpaqueIteratorLib = Native.load("somelib", libClass)
    }
    
    internal fun nextInternal(): AttrOpaque1Renamed? {
        
        val returnVal = lib.namespace_OpaqueIterator_next(handle);
        val selfEdges: List<Any> = listOf()
        val handle = returnVal ?: return null
        val returnOpaque = AttrOpaque1(handle, selfEdges)
        CLEANER.register(returnOpaque, AttrOpaque1.AttrOpaque1Cleaner(handle, AttrOpaque1.lib));
        
        return returnOpaque
    }

    var iterVal = nextInternal()

    override fun hasNext(): Boolean {
       return iterVal != null
    }

    override fun next(): AttrOpaque1Renamed?{
        val returnVal = iterVal
        if (returnVal == null) {
            throw NoSuchElementException()
        } else {
            iterVal = nextInternal()
            return returnVal
        }
    }

}
