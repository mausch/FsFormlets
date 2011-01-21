using System.Xml.Linq;
using Microsoft.FSharp.Core;

namespace Formlets.CSharp {
    /// <summary>
    /// Formlet result
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class FormletResult<T> {
        private readonly XDocument errorForm;
        private readonly FSharpOption<T> value;

        public FormletResult(XDocument errorForm, FSharpOption<T> value) {
            this.errorForm = errorForm;
            this.value = value;
        }

        /// <summary>
        /// Error form
        /// </summary>
        public XDocument ErrorForm {
            get { return errorForm; }
        }

        /// <summary>
        /// Formlet result value. If None, there was an error.
        /// </summary>
        public FSharpOption<T> Value {
            get { return value; }
        }
    }
}