using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Formlets.CSharp {
    public static class L {
        public static Func<A,B> F<A,B>(Func<A,B> f) {
            return f;
        }
    }
}
