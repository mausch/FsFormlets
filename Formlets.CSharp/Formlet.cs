using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

namespace Formlets.CSharp {
    public class Formlet<T> {
        private readonly FSharpFunc<int, Tuple<Tuple<FSharpList<xml_item>, FSharpFunc<FSharpList<Tuple<string, InputValue>>, Tuple<FSharpList<xml_item>, FSharpOption<T>>>>, int>> f;

        public Formlet(FSharpFunc<int, Tuple<Tuple<FSharpList<xml_item>, FSharpFunc<FSharpList<Tuple<string, InputValue>>, Tuple<FSharpList<xml_item>, FSharpOption<T>>>>, int>> f) {
            this.f = f;
        }

        public static implicit operator FSharpFunc<int, Tuple<Tuple<FSharpList<xml_item>, FSharpFunc<FSharpList<Tuple<string, InputValue>>, Tuple<FSharpList<xml_item>, FSharpOption<T>>>>, int>>(Formlet<T> f) {
            return f.f;
        }

        public Formlet<B> Apply<B>(Formlet<Func<T, B>> a) {
            var ff = Formlet.FormletFSharpFunc(a);
            var r = new Formlet<B>(FormletModule.ap(ff.f, this.f));
            return r;
        }

    }

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

    public static class Formlet {
        public static FSharpList<Tuple<K, V>> DictToTupleList<K, V>(IEnumerable<KeyValuePair<K, V>> dict) {
            var tuples = dict.Select(kv => Tuple.Create(kv.Key, kv.Value));
            return SeqModule.ToList(tuples);
        }

        public static Formlet<string> Input(string defaultValue, IEnumerable<KeyValuePair<string, string>> attributes) {
            return new Formlet<string>(FormletModule.input(defaultValue, DictToTupleList(attributes)));
        }

        public static Formlet<FSharpFunc<A, B>> FormletFSharpFunc<A, B>(Formlet<Func<A, B>> f) {
            FSharpFunc<Func<A, B>, FSharpFunc<A, B>> toff = new FuncFSharpFunc<Func<A, B>, FSharpFunc<A, B>>(a => new FuncFSharpFunc<A, B>(a));
            var ff = FormletModule.lift(toff, f);
            return new Formlet<FSharpFunc<A, B>>(ff);
        }

        public static Formlet<B> Apply<A, B>(Formlet<Func<A, B>> a, Formlet<A> b) {
            return b.Apply(a);
        }
    }
}