using System.Xml.Linq;
using Microsoft.FSharp.Core;

namespace Formlets.CSharp {
    public class FormletResult<T> {
        private readonly XDocument errorForm;
        private readonly FSharpOption<T> value;

        public FormletResult(XDocument errorForm, FSharpOption<T> value) {
            this.errorForm = errorForm;
            this.value = value;
        }

        public XDocument ErrorForm {
            get { return errorForm; }
        }

        public FSharpOption<T> Value {
            get { return value; }
        }
    }
}