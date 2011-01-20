using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Formlets.CSharp.Tests {
    public class Usage {
        [Fact]
        public void Run() {
            var input = Formlet.Input("", Enumerable.Empty<KeyValuePair<string,string>>());
            var result = input.Run(new Dictionary<string, string> { { "input_0", "something" } });
            Assert.True(result.Value.IsSome());
            Assert.Equal("something", result.Value.Value);
            Console.WriteLine(result.ErrorForm);
            Assert.Equal("<input name=\"input_0\" value=\"something\" />", result.ErrorForm.ToString());
        }
    }
}