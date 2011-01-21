using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

namespace Formlets.CSharp {
    public class Formlet<T> {
        // real formlet, wrapped
        private readonly FSharpFunc<int, Tuple<Tuple<FSharpList<xml_item>, FSharpFunc<FSharpList<Tuple<string, InputValue>>, Tuple<FSharpList<xml_item>, FSharpOption<T>>>>, int>> f;

        /// <summary>
        /// Creates a <see cref="Formlet{T}"/> by wrapping an F# formlet
        /// </summary>
        /// <param name="f">F# formlet</param>
        public Formlet(FSharpFunc<int, Tuple<Tuple<FSharpList<xml_item>, FSharpFunc<FSharpList<Tuple<string, InputValue>>, Tuple<FSharpList<xml_item>, FSharpOption<T>>>>, int>> f) {
            this.f = f;
        }

        public static implicit operator FSharpFunc<int, Tuple<Tuple<FSharpList<xml_item>, FSharpFunc<FSharpList<Tuple<string, InputValue>>, Tuple<FSharpList<xml_item>, FSharpOption<T>>>>, int>>(Formlet<T> f) {
            return f.f;
        }

        /// <summary>
        /// Applicative application (i.e. &lt;*&gt;)
        /// </summary>
        /// <typeparam name="B"></typeparam>
        /// <param name="a"></param>
        /// <returns></returns>
        public Formlet<B> Apply<B>(Formlet<Func<T, B>> a) {
            var ff = Formlet.FormletFSharpFunc(a);
            var r = FormletModule.ap(ff.f, f);
            return new Formlet<B>(r);
        }

        /// <summary>
        /// Applicative left application (i.e. &lt;*)
        /// </summary>
        /// <typeparam name="C"></typeparam>
        /// <param name="a"></param>
        /// <returns></returns>
        public Formlet<T> ApplyIgnore<C>(Formlet<C> a) {
            var r = FormletModule.apl(f, a.f);
            return new Formlet<T>(r);
        }

        /// <summary>
        /// Lifts and applies a function
        /// </summary>
        /// <typeparam name="B"></typeparam>
        /// <param name="a"></param>
        /// <returns></returns>
        public Formlet<B> Lift<B>(Func<T, B> a) {
            var ff = FuncFSharpFunc.FromFunc(a);
            var r = FormletModule.lift(ff, f);
            return new Formlet<B>(r);
        }

        /// <summary>
        /// Runs a formlet against an environment
        /// </summary>
        /// <param name="env"></param>
        /// <returns></returns>
        public FormletResult<T> Run(IEnumerable<KeyValuePair<string, string>> env) {
            var tuples = env.Select(kv => Tuple.Create(kv.Key, InputValue.NewValue(kv.Value)));
            var list = SeqModule.ToList(tuples);
            return Run(list);
        }

        /// <summary>
        /// Runs a formlet against an environment
        /// </summary>
        /// <param name="list"></param>
        /// <returns></returns>
        private FormletResult<T> Run(FSharpList<Tuple<string,InputValue>> list) {
            var ff = FormletModule.run(f);
            var r = ff.Invoke(list);
            var xdoc = XmlWriter.render(r.Item1);
            var value = r.Item2;
            return new FormletResult<T>(xdoc, value);
        }

        /// <summary>
        /// Runs a formlet against an environment
        /// </summary>
        /// <param name="nv"></param>
        /// <returns></returns>
        public FormletResult<T> Run(System.Collections.Specialized.NameValueCollection nv) {
            var list = EnvDictModule.fromNV(nv);
            return Run(list);
        }

        /// <summary>
        /// Renders a formlet
        /// </summary>
        /// <returns></returns>
        public string Render() {
            return FormletModule.render(f);
        }
    }
}