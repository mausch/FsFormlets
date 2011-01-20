using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Formlets.CSharp.Tests {
    public class Usage {
        [Fact]
        public void Test() {
            var input = Formlet.Input("", Enumerable.Empty<KeyValuePair<string,string>>());
        }
    }
}