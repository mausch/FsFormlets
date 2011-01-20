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

        public static FSharpFunc<A, B> FromFunc(Func<A, B> f) {
            return new FuncFSharpFunc<A, B>(f);
        }
    }
}