// generated by diplomat-tool

// https://github.com/dart-lang/sdk/issues/53946
// ignore_for_file: non_native_function_type_argument_to_pointer

part of 'lib.g.dart';

/// See the [Rust documentation for `FixedDecimal`](https://docs.rs/fixed_decimal/latest/fixed_decimal/struct.FixedDecimal.html) for more information.
final class ICU4XFixedDecimal implements ffi.Finalizable {
  final ffi.Pointer<ffi.Opaque> _underlying;

  ICU4XFixedDecimal._(this._underlying) {
    _finalizer.attach(this, _underlying.cast());
  }

  static final _finalizer = ffi.NativeFinalizer(_capi('ICU4XFixedDecimal_destroy'));

  /// Construct an [`ICU4XFixedDecimal`] from an integer.
  factory ICU4XFixedDecimal(int v) {
    final result = _ICU4XFixedDecimal_new(v);
    return ICU4XFixedDecimal._(result);
  }

  // ignore: non_constant_identifier_names
  static final _ICU4XFixedDecimal_new =
    _capi<ffi.NativeFunction<ffi.Pointer<ffi.Opaque> Function(ffi.Int32)>>('ICU4XFixedDecimal_new')
      .asFunction<ffi.Pointer<ffi.Opaque> Function(int)>(isLeaf: true);

  /// Multiply the [`ICU4XFixedDecimal`] by a given power of ten.
  ///
  /// See the [Rust documentation for `multiply_pow10`](https://docs.rs/fixed_decimal/latest/fixed_decimal/struct.FixedDecimal.html#method.multiply_pow10) for more information.
  void multiplyPow10(int power) {
    _ICU4XFixedDecimal_multiply_pow10(_underlying, power);
  }

  // ignore: non_constant_identifier_names
  static final _ICU4XFixedDecimal_multiply_pow10 =
    _capi<ffi.NativeFunction<ffi.Void Function(ffi.Pointer<ffi.Opaque>, ffi.Int16)>>('ICU4XFixedDecimal_multiply_pow10')
      .asFunction<void Function(ffi.Pointer<ffi.Opaque>, int)>(isLeaf: true);

  /// Format the [`ICU4XFixedDecimal`] as a string.
  ///
  /// See the [Rust documentation for `write_to`](https://docs.rs/fixed_decimal/latest/fixed_decimal/struct.FixedDecimal.html#method.write_to) for more information.
  ///
  /// Throws [VoidError] on failure.
  @override
  String toString() {
    final writeable = _Writeable();
    final result = _ICU4XFixedDecimal_to_string(_underlying, writeable._underlying);
    if (!result.isOk) {
      throw VoidError();
    }
    return writeable.finalize();
  }

  // ignore: non_constant_identifier_names
  static final _ICU4XFixedDecimal_to_string =
    _capi<ffi.NativeFunction<_ResultVoidVoid Function(ffi.Pointer<ffi.Opaque>, ffi.Pointer<ffi.Opaque>)>>('ICU4XFixedDecimal_to_string')
      .asFunction<_ResultVoidVoid Function(ffi.Pointer<ffi.Opaque>, ffi.Pointer<ffi.Opaque>)>(isLeaf: true);
}