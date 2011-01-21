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

        [Fact]
        public void Render() {
            var input = Formlet.Input("a value", new Dictionary<string, string> {{"size","10"}});
            var html = input.Render();
            Console.WriteLine(html);
            Assert.Equal("<input name=\"input_0\" value=\"a value\" size=\"10\" />", html);
        }

        [Fact]
        public void Lift() {
            var input = Formlet.Input("a value", new Dictionary<string, string> { { "size", "10" } });
            var inputInt = input.Lift(int.Parse);
            var result = inputInt.Run(new Dictionary<string, string> { {"input_0", "15" } });
            Assert.True(result.Value.IsSome());
            Assert.Equal(15, result.Value.Value);

        }
    }
}