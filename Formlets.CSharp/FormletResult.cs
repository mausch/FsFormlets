using Microsoft.FSharp.Core;

namespace Formlets.CSharp {
    /// <summary>
    /// Formlet result
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class FormletResult<T> {
        private readonly string errorForm;
        private readonly FSharpOption<T> value;

        public FormletResult(string errorForm, FSharpOption<T> value) {
            this.errorForm = errorForm;
            this.value = value;
        }

        /// <summary>
        /// Error form
        /// </summary>
        public string ErrorForm {
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