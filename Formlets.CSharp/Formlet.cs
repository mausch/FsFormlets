using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

namespace Formlets.CSharp {
    public static class Formlet {
        public static FSharpList<Tuple<K, V>> DictToTupleList<K, V>(IEnumerable<KeyValuePair<K, V>> dict) {
            var tuples = dict.Select(kv => Tuple.Create(kv.Key, kv.Value));
            return SeqModule.ToList(tuples);
        }

        public static Formlet<string> Input(string defaultValue, IEnumerable<KeyValuePair<string, string>> attributes) {
            return new Formlet<string>(FormletModule.input(defaultValue, DictToTupleList(attributes)));
        }

        public static Formlet<Unit> Text(string text) {
            return new Formlet<Unit>(FormletModule.text(text));
        }

        public static Formlet<FSharpFunc<A, B>> FormletFSharpFunc<A, B>(Formlet<Func<A, B>> f) {
            FSharpFunc<Func<A, B>, FSharpFunc<A, B>> toff = new FuncFSharpFunc<Func<A, B>, FSharpFunc<A, B>>(a => new FuncFSharpFunc<A, B>(a));
            var ff = FormletModule.lift(toff, f);
            return new Formlet<FSharpFunc<A, B>>(ff);
        }

        public static Formlet<B> Apply<A, B>(Formlet<Func<A, B>> a, Formlet<A> b) {
            return b.Apply(a);
        }

        public static Formlet<B> Lift<A,B>(Func<A,B> f, Formlet<A> a) {
            return new Formlet<B>(FormletModule.lift(FuncFSharpFunc.FromFunc(f), a));
        }

        public static Formlet<C> Lift2<A,B,C>(Func<A,B,C> f, Formlet<A> a, Formlet<B> b) {
            var ff = FuncFSharpFunc.FromFunc(f);
            var r = FormletModule.lift2(ff, a, b);
            return new Formlet<C>(r);
        }

        public static Formlet<T> Yield<T>(T value) {
            var r =  FormletModule.puree(value);
            return new Formlet<T>(r);
        }
    }
}