// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Razor.Language;

namespace Microsoft.CodeAnalysis.Razor.ProjectSystem
{
    internal class TestGeneratedOutputPublisher : GeneratedOutputPublisher
    {
        public List<(DefaultDocumentSnapshot document, RazorCodeDocument output, VersionStamp version)> Output = new List<(DefaultDocumentSnapshot document, RazorCodeDocument output, VersionStamp version)>();

        public override Task PublishGeneratedOutputAsync(DefaultDocumentSnapshot document, RazorCodeDocument output, VersionStamp version)
        {
            Output.Add((document, output, version));
            return Task.CompletedTask;
        }
    }
}
