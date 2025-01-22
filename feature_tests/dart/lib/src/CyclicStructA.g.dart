// generated by diplomat-tool

part of 'lib.g.dart';

final class _CyclicStructAFfi extends ffi.Struct {
  external _CyclicStructBFfi a;
}

final class CyclicStructA {
  CyclicStructB a;

  CyclicStructA({required this.a});

  // This struct contains borrowed fields, so this takes in a list of
  // "edges" corresponding to where each lifetime's data may have been borrowed from
  // and passes it down to individual fields containing the borrow.
  // This method does not attempt to handle any dependencies between lifetimes, the caller
  // should handle this when constructing edge arrays.
  // ignore: unused_element
  CyclicStructA._fromFfi(_CyclicStructAFfi ffi) :
    a = CyclicStructB._fromFfi(ffi.a);

  // ignore: unused_element
  _CyclicStructAFfi _toFfi(ffi.Allocator temp) {
    final struct = ffi.Struct.create<_CyclicStructAFfi>();
    struct.a = a._toFfi(temp);
    return struct;
  }

  static CyclicStructB getB() {
    final result = _CyclicStructA_get_b();
    return CyclicStructB._fromFfi(result);
  }

  String cyclicOut() {
    final temp = _FinalizedArena();
    final write = _Write();
    _CyclicStructA_cyclic_out(_toFfi(temp.arena), write._ffi);
    return write.finalize();
  }

  String doubleCyclicOut(CyclicStructA cyclicStructA) {
    final temp = _FinalizedArena();
    final write = _Write();
    _CyclicStructA_double_cyclic_out(_toFfi(temp.arena), cyclicStructA._toFfi(temp.arena), write._ffi);
    return write.finalize();
  }

  String get getterOut {
    final temp = _FinalizedArena();
    final write = _Write();
    _CyclicStructA_getter_out(_toFfi(temp.arena), write._ffi);
    return write.finalize();
  }

  @override
  bool operator ==(Object other) =>
      other is CyclicStructA &&
      other.a == a;

  @override
  int get hashCode => Object.hashAll([
        a,
      ]);
}

@meta.RecordUse()
@ffi.Native<_CyclicStructBFfi Function()>(isLeaf: true, symbol: 'CyclicStructA_get_b')
// ignore: non_constant_identifier_names
external _CyclicStructBFfi _CyclicStructA_get_b();

@meta.RecordUse()
@ffi.Native<ffi.Void Function(_CyclicStructAFfi, ffi.Pointer<ffi.Opaque>)>(isLeaf: true, symbol: 'CyclicStructA_cyclic_out')
// ignore: non_constant_identifier_names
external void _CyclicStructA_cyclic_out(_CyclicStructAFfi self, ffi.Pointer<ffi.Opaque> write);

@meta.RecordUse()
@ffi.Native<ffi.Void Function(_CyclicStructAFfi, _CyclicStructAFfi, ffi.Pointer<ffi.Opaque>)>(isLeaf: true, symbol: 'CyclicStructA_double_cyclic_out')
// ignore: non_constant_identifier_names
external void _CyclicStructA_double_cyclic_out(_CyclicStructAFfi self, _CyclicStructAFfi cyclicStructA, ffi.Pointer<ffi.Opaque> write);

@meta.RecordUse()
@ffi.Native<ffi.Void Function(_CyclicStructAFfi, ffi.Pointer<ffi.Opaque>)>(isLeaf: true, symbol: 'CyclicStructA_getter_out')
// ignore: non_constant_identifier_names
external void _CyclicStructA_getter_out(_CyclicStructAFfi self, ffi.Pointer<ffi.Opaque> write);
