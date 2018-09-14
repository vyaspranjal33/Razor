// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading.Tasks;
using Microsoft.AspNetCore.Razor.Language;

namespace Microsoft.CodeAnalysis.Razor.ProjectSystem
{
    internal class DefaultGeneratedOutputPublisher : GeneratedOutputPublisher
    {
        public override Task PublishGeneratedOutputAsync(DefaultDocumentSnapshot document, RazorCodeDocument output, VersionStamp version)
        {
            // Do nothing
            return Task.CompletedTask;
        }
    }
}
