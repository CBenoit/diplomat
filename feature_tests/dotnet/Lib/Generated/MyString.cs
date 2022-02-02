// Automatically generated by Diplomat

#pragma warning disable 0105
using System;
using System.Runtime.InteropServices;

using DiplomatFeatures.Diplomat;
#pragma warning restore 0105

namespace DiplomatFeatures;

#nullable enable

public partial class MyString: IDisposable
{
    private unsafe Raw.MyString* _inner;

    /// <summary>
    /// Creates a managed <c>MyString</c> from a raw handle.
    /// </summary>
    /// <remarks>
    /// Safety: you should not build two managed objects using the same raw handle (may causes use-after-free and double-free).
    /// </remarks>
    /// <remarks>
    /// This constructor assumes the raw struct is allocated on Rust side.
    /// If implemented, the custom Drop implementation on Rust side WILL run on destruction.
    /// </remarks>
    public unsafe MyString(Raw.MyString* handle)
    {
        _inner = handle;
    }

    /// <returns>
    /// A <c>MyString</c> allocated on Rust side.
    /// </returns>
    public static MyString New(string v)
    {
        unsafe
        {
            byte[] vBuf = DiplomatUtils.StringToUtf8(v);
            nuint vBufLength = (nuint)vBuf.Length;
            fixed (byte* vBufPtr = vBuf)
            {
                Raw.MyString* retVal = Raw.MyString.New(vBufPtr, vBufLength);
                return new MyString(retVal);
            }
        }
    }

    public void SetStr(string newStr)
    {
        unsafe
        {
            if (_inner == null)
            {
                throw new ObjectDisposedException("MyString");
            }
            byte[] newStrBuf = DiplomatUtils.StringToUtf8(newStr);
            nuint newStrBufLength = (nuint)newStrBuf.Length;
            fixed (byte* newStrBufPtr = newStrBuf)
            {
                Raw.MyString.SetStr(_inner, newStrBufPtr, newStrBufLength);
            }
        }
    }

    public void GetStr(DiplomatWriteable writeable)
    {
        unsafe
        {
            if (_inner == null)
            {
                throw new ObjectDisposedException("MyString");
            }
            Raw.MyString.GetStr(_inner, &writeable);
        }
    }

    public string GetStr()
    {
        unsafe
        {
            if (_inner == null)
            {
                throw new ObjectDisposedException("MyString");
            }
            DiplomatWriteable writeable = new DiplomatWriteable();
            Raw.MyString.GetStr(_inner, &writeable);
            string retVal = writeable.ToUnicode();
            writeable.Dispose();
            return retVal;
        }
    }

    public static string MakeUppercase(string v)
    {
        unsafe
        {
            byte[] vBuf = DiplomatUtils.StringToUtf8(v);
            nuint vBufLength = (nuint)vBuf.Length;
            fixed (byte* vBufPtr = vBuf)
            {
                Raw.MyString.MakeUppercase(vBufPtr, vBufLength);
                string retVal = DiplomatUtils.Utf8ToString(vBuf);
                return retVal;
            }
        }
    }

    /// <summary>
    /// Returns the underlying raw handle.
    /// </summary>
    public unsafe Raw.MyString* AsFFI()
    {
        return _inner;
    }

    /// <summary>
    /// Destroys the underlying object immediately.
    /// </summary>
    public void Dispose()
    {
        unsafe
        {
            if (_inner == null)
            {
                return;
            }

            Raw.MyString.Destroy(_inner);
            _inner = null;

            GC.SuppressFinalize(this);
        }
    }

    ~MyString()
    {
        Dispose();
    }
}
