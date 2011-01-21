using System;

namespace Formlets.CSharp {
    public static class FormletExtensions {
        public static Formlet<B> Apply<A,B>(this Formlet<Func<A,B>> a, Formlet<A> b) {
            return b.Apply(a);
        }
    }
}