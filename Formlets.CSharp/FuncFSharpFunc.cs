using System;
using Microsoft.FSharp.Core;

namespace Formlets.CSharp {
    public class FuncFSharpFunc<A, B> : FSharpFunc<A, B> {
        private readonly Func<A, B> f;

        public FuncFSharpFunc(Func<A, B> f) {
            this.f = f;
        }

        public override B Invoke(A func) {
            return f(func);
        }
    }

    public static class FuncFSharpFunc {
        public static FSharpFunc<A, B> FromFunc<A,B>(Func<A, B> f) {
            return new FuncFSharpFunc<A, B>(f);
        }

        public static FSharpFunc<A, FSharpFunc<B,C>> FromFunc<A,B,C>(Func<A,B,C> f) {
            return FromFunc((A a) => FromFunc(L.F((B b) => f(a, b))));
        }
    }
}