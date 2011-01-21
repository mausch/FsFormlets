using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Formlets.CSharp {
    /// <summary>
    /// Helps C# with type inference for lambdas
    /// </summary>
    public static class L {
        /// <summary>
        /// Helps C# with type inference for lambdas
        /// </summary>
        /// <typeparam name="A"></typeparam>
        /// <typeparam name="B"></typeparam>
        /// <param name="f"></param>
        /// <returns></returns>
        public static Func<A,B> F<A,B>(Func<A,B> f) {
            return f;
        }
    }
}
