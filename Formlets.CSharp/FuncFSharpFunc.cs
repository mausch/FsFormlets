using System;
using Microsoft.FSharp.Core;

namespace Formlets.CSharp {
    /// <summary>
    /// Wraps a <see cref="Func&lt;A,B&gt;"/> as an F# function
    /// </summary>
    /// <typeparam name="A"></typeparam>
    /// <typeparam name="B"></typeparam>
    public class FuncFSharpFunc<A, B> : FSharpFunc<A, B> {
        private readonly Func<A, B> f;

        /// <summary>
        /// Wraps a <see cref="Func&lt;A,B&gt;"/> as an F# function
        /// </summary>
        /// <param name="f"></param>
        public FuncFSharpFunc(Func<A, B> f) {
            this.f = f;
        }

        public override B Invoke(A func) {
            return f(func);
        }
    }

    /// <summary>
    /// Wraps System.Funcs as F# functions
    /// </summary>
    public static class FuncFSharpFunc {
        /// <summary>
        /// Wraps a <see cref="Func&lt;A,B&gt;"/> as an F# function
        /// </summary>
        /// <typeparam name="A"></typeparam>
        /// <typeparam name="B"></typeparam>
        /// <param name="f"></param>
        /// <returns></returns>
        public static FSharpFunc<A, B> FromFunc<A,B>(Func<A, B> f) {
            return new FuncFSharpFunc<A, B>(f);
        }

        /// <summary>
        /// Wraps a <see cref="Func&lt;A,B,C&gt;"/> as a curried F# function
        /// </summary>
        /// <typeparam name="A"></typeparam>
        /// <typeparam name="B"></typeparam>
        /// <typeparam name="C"></typeparam>
        /// <param name="f"></param>
        /// <returns></returns>
        public static FSharpFunc<A, FSharpFunc<B,C>> FromFunc<A,B,C>(Func<A,B,C> f) {
            return FromFunc((A a) => FromFunc(L.F((B b) => f(a, b))));
        }
    }
}